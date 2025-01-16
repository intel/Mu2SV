/* 
 * Model of the memory renaming algorithm (MRN) of RYL OOO.
 *
 * User program has PC (with NumPC) values. Some of these are associated with loads and stores.
 * FE of OOO has a (store_pc, load_pc) map that identifies MRNable pairs. Multiple stores can forward
 * to the same load_pc but a store_pc cannot forward to multiple loads (Todo: check. Not sure if this is necessary.) 
 * User program gets unrolled into a series of uops, each with a uopid (max val MaxUopId) and associated PC location. 
 * Each uop also carries with it a stage number. Initially, all uop.stages are 0.
 * This model is written to reduce the amount asynchrony in the rules by having only one rule that operates per stage.
 * Rule for stage x will iterate through all uops and any uop in stage x will be processed and advanced to the 
 * next stage (x+1). Overall model cycles through stages from DQ -> FE -> RA1 -> RA2 -> RA3 -> Mem1 -> Mem2 -> ROB -> DQ
 * Initially, all uop start at stage DQ, indicating that they are outside OOO. The first rule inserts a line's worth
 * of uops into the FE and marks their uop.stage as FE. It also advances the global stage variable to FE.
 * All uops are stored in two arrays: inflight and idq. Idq holds the golden copy of all uops whereas  inflight contains
 * the working copy of uop as they are getting modified in the pipeline. In case of branch mispredicts (which we don't
 * have in this model) both idq and inflight will have to be cleared from bad branch uop downwards.
  (1) This model stores uops by lines
  (2) It arranges lines into strands for load checks in Mem (L0)
  (3) uops currently supported: 
    (a) int_load, int_store --> map to one memory cell of 1 bits, stays
        in slice 
    (b) vec_load, vec_store --> map to two consequtive cells of
        1 bit each. Each bit might be in a different slice. Each bit gets mapped to a different
        MRN_hi and MRN_lo regs which can get mapped to different physical regs.
    (c) Add uop -- a different uop than loads/stores
  (4) Memory has NumMemLocs each with 1 bit of data. This is sliced into NumSlices sections of contiguous locations.
  (5)    
  * Memory model is important in what we can mode and not model:
  * In our model Memory consists of NumMemLoc addresses and each address holds 1 bit item.
  * Each MRN reg holds 1 bit as well. For vector MRN we have NumVecMRN reg name and there are hi and lo versions
  * for each vec MRN reg name. Each hi and lo reg holds only 1 bit.
  * 
  * With this model we can cover the following cases:
    (1) int store writes to addr1 and int load reads from addr1
    (2) vec store writes to addr1, addr2 and vec load reads from addr1, addr2
      (a) two cases here: addr1, addr2 belong to the same slice or different slices
    (3) vec stores writes to addr1, addr2 and int load reads from addr1 or addr2
    (4) two int stores to addr1 and addr2 and one vec load reads from addr1 and addr2
    (5) two vec stores, one to addr1+addr2 and one to addr3+addr4. one vec load from addr2, addr3
      (a) two case depending on whether addr2 and addr3 belong to the same slice or not.
    
    Actually (3) and (4) are not allowed: stores and loads have to belong to the same domain to be MRNed.
    So in MRN map will have to distinguish between int and vec PC locations. Similarly we need is_load_int/_vec and
    is_store_int/_vec functions to distinguish PCs correctly.
    Since each mem loc is only 1 bit what is not supported in this model is an int load overlapping with
    two int stores. This would require each int mrn reg to hold 2 bits.
    Todo: it should include splits...what does that mean. From Strawman talk @20:12
    Not clear: AVX 256 and AVX 128 are both vector operations but one uses both hi and lo, and the other uses only lo.
    A store that uses only the lo part will invalidate the hi part too. So it is fine with having only color for
    vec MRN hi, lo registers.
  * 
 */

/* Constants */

const

  MaxRegVal: 3;/* Tied to NumMemLocs */
  NumPC: 6; /* Num of program locs in the original prog.*/ 
  NumStages: 7; /* Num stages in the MRN pipeline */
  NumLogicalRegs: 2; /* Num int/logical regs */
  NumPhysicalRegs: 6; /* Num physical regs */
  NumVecLogicalRegs: 2; /* Num vec logical regs*/
  NumVecPhysicalRegs: 6; /* Num vec physical regs */
  NumMRNRegs: 2; /* Num int MRN regs*/
  NumVecMRNRegs:2; /* Num vec MRN regs*/
  NumMemLocs: 4; /* Has to be >= MaxRegVal */
  LineWidth : 2;  /* Num uops per line */
  StrandWidth : 2; /* Num lines per strand */
  MaxStrands: 20; /* Used to bound the model */
  MaxLines: 6; /* some num > LineWidth * StrandWidth */
  MaxUopId: 10; /* Max uops in a unrolled program. Was 100 */
  NumLoadBufEntries: 5; /* LB size, set to a large value to not constrain the model. Was 50 */
  NumStBufEntries: 5; /* SB size, set to a large value to not constrain the model. Was 50 */
  NumROBEntries: 10; /* ROB size, set to a large value to not constrain the model. Was 100 */
  NumColors: 3; 
  NumSlices: 2;
  MaxMRNID: NumMRNRegs * NumColors;
  MaxVecMRNID: NumVecMRNRegs * NumColors;
  NumMRNMaps: 2;
  /* Each rht entry has NumMaps Map entries. Each Map entry has lreg, new_preg, old_preg.
  Since our uop have at most 2 logical regs (incluing mrn regs) NumMaps is 2 here.
  */
  NumMaps: 2;
  NumSCBEntries: 10; /* Makes it large so that it doesn't impose constraints. Was 10 */
  Invalid_Uopid: 0; /* Used to denote an undefined UopId */

/* Types */

Type

PC: 1..NumPC;

Slice: 1..NumSlices;

Stage: Enum { DQ, FE, RA1, RA2, Mem1, Mem2, RB };

UopType : Enum
{
  Add, -- Representative Exe op
  Load_int, -- Load from Mem
  Load_int_mrn, -- For MRNed loads
  Store_int, -- Store to Mem
  Store_int_mrn, -- for MRNed stores
  Load_vec, -- Vector load from Mem
  Load_vec_mrn, -- for MRNed vec loads
  Store_vec, -- Vector store from Mem
  Store_vec_mrn, -- MRNed vec stores
  Nop
  /*
    To be added later: LoadOpStore
  */
};


UopID  : 1..MaxUopId;
UopID_Ext: 0..MaxUopId;
ROBID : 1..NumROBEntries;
LBID: 1..NumLoadBufEntries;
SBID: 1..NumStBufEntries;

PhysicalRegs: 1..NumPhysicalRegs;
LogicalRegs: 1..(NumLogicalRegs + NumMRNRegs); /* MRN regs are grouped with logical regs */
VecPhysicalRegs: 1..NumVecPhysicalRegs;
VecLogicalRegs: 1..(NumVecLogicalRegs + NumVecMRNRegs); /* Vec MRN regs groups with vec log regs */
/*MRNRegs: 1..NumMRNRegs;*/
MRNColors: 1..NumColors;
/*VecMRNRegs: 1..NumVecMRNRegs;*/
MemLocs: 1..NumMemLocs;
MRN_ID: 1..MaxMRNID;
MRN_ID_Ext: 0..MaxMRNID;
VMRN_ID: 1..MaxVecMRNID;
VMRN_ID_Ext: 0..MaxVecMRNID;

CheckerRegs: 1..NumLogicalRegs; /* Used for in-order checker */
VecCheckerRegs: 1..NumVecLogicalRegs; 

RegVal: 0..MaxRegVal;

VecRegVal: Record
  high: RegVal; 
  low: RegVal;
end;

MRNMap: Record
  lpc: PC;
  spc: PC;
end;

Map: Record
  lreg: LogicalRegs;
  new_preg: PhysicalRegs;
  old_preg: PhysicalRegs;
end;

Maps: array[1..NumMaps] of Map;

VMap: Record
  lreg: VecLogicalRegs;
  new_preg: VecPhysicalRegs;
  old_preg: VecPhysicalRegs;
end;

VMaps: array[1..NumMaps] of VMap;

Bool_Map: Record
  updated: boolean;
  map: Map;
end;

Bool_VMap: Record
  updated: boolean;
  vmap: VMap;
end;

RHT_S: Record
  maps: Maps;
  vmaps: VMaps;
  prev_lbid: LBID;
  prev_sbid: SBID;
end;

SCBID: 1..NumSCBEntries;

Uop : Record
  uoptype : UopType;
  pc: PC;
  src1: LogicalRegs; /* Following x86 convention src1 can also be the dest.*/
  src2: LogicalRegs; 
  dst: LogicalRegs;
  mem_addr: MemLocs; /* For loads and stores. */
  vsrc1: VecLogicalRegs; /* Following x86 convention src1 can also be the dest.*/
  vsrc2: VecLogicalRegs; 
  vdst: VecLogicalRegs;
  mem_width: 1..2; /* For loads and stores. */
  uopid: UopID;
  uopstage: Stage; /* Moving to a uop centric description. Each uop carries its own stage info. */
  line_id: 0..MaxLines;
  strand_id: 0..MaxStrands;
  mrn_valid: boolean; /* used in FE to indicate if the pc is predicted to be MRN'able*/
  mrn_reg: LogicalRegs;
  vmrn_reg: VecLogicalRegs;
  /* mrn_color is same for both int and vec sides */
  mrn_color: MRNColors;
  rht: RHT_S;
  strand: 0..MaxStrands;
  /*maps: Maps;
  vmaps: VMaps;*/
End;

-- Renamed uops. Src1/2 are physical regs
Ruop : Record
  uoptype : UopType;
  pc: PC;
  src1: PhysicalRegs; 
  src2: PhysicalRegs;
  vsrc1: VecPhysicalRegs;
  vsrc2: VecPhysicalRegs;
  dst: PhysicalRegs; -- Here we have an explicit dst
  --dst2: PhysicalRegs; -- Here we have an explicit dst
  vdst: VecPhysicalRegs;
  --vdst2: VecPhysicalRegs;
  mrn_valid: boolean; /* used in FE to indicate if the pc is predicted to be MRN'able*/
  mrn_reg: LogicalRegs;
  --mrn_reg2: LogicalRegs;
  vmrn_reg: VecLogicalRegs;
  mem_addr: MemLocs; -- For loads and stores
  mem_width: 1..2; /* For loads and stores*/
  uopid: UopID;
  uopstage: Stage;
  line_id: 0..MaxLines;
  strand_id: 0..MaxStrands;
  nextuopid: UopID; /* Used only for branch uops*/
  robid: ROBID; /* Rob id. this is not present in uop*/
  sbid: SBID; /* Store buffer id. not present in uop*/
  lbid: LBID; /* Load buffer id. not present in uop*/
  prec_sbid: SBID; /* Used to compute previous stores in case of a load */
  prec_lbid: SBID; /* Used to compute previous loads in case of a store */
  /* auxiliary var to indicate which physical regs are produced or consumed by the renamed uop*/
  produced: PhysicalRegs; 
  consumed1: PhysicalRegs;
  consumed2: PhysicalRegs;
  vproduced: VecPhysicalRegs;
  vconsumed1: VecPhysicalRegs;
  vconsumed2: VecPhysicalRegs;
  rht: RHT_S;  
  strand: 0..MaxStrands;
  /*done: boolean;
  nuke: boolean;*/
  status: Enum{done, nuke};
  retired: boolean;
  /*maps: Maps;
  vmaps: VMaps;*/
End;

/* Variables */
Var

stage : Stage; /* stage 0 is where uops enter the OOO pipeline from outside*/

/* the instruction pointer always increases but can jump over sequence of uopids.*/
ip : UopID; 

/*
 uop_count is an aux variable that tracks the number of uops entering the model.
 always increases by 1 till it reaches the max value.
 at max value we can either wrap around or just deadlock the system.
*/

uop_count : UopID; -- this is running count of number of uop entering


line : array[1..LineWidth] of Uop;
idq: array[1..MaxUopId] of Uop;
inflight: array[1..MaxUopId] of Uop;
inflight_r: array[1..MaxUopId] of Ruop;
numlines: 0..MaxLines;
lineindex : 1..(LineWidth+1);
renamed_line: array[1..LineWidth] of Ruop;

lregs: array[LogicalRegs] of RegVal;
vregs: array[VecLogicalRegs] of VecRegVal;
pr: array[PhysicalRegs] of RegVal;
vpr: array[VecPhysicalRegs] of VecRegVal;
mem: array[1..NumMemLocs] of RegVal;
/* checker regs */
cregs: array[LogicalRegs] of RegVal;
cvecregs: array[VecLogicalRegs] of VecRegVal;
/* checker mem */
cmem: array[1..NumMemLocs] of RegVal;

latest_mrn_id: array[PC] of MRN_ID;
latest_vec_mrn_id: array[PC] of VMRN_ID;
curr_mrn_id: MRN_ID;
curr_vmrn_id: VMRN_ID;
mrn_map: array[1..NumMRNMaps] of MRNMap;
mrn_reg_color_map: array[LogicalRegs] of MRNColors;
mrn_reg_color_map_vec: array[VecLogicalRegs] of MRNColors;

--  no clusters currently but strands used in MRN l0_check
numstrands: 0..MaxStrands;

-- a physical reg r is free if freelist[r] is true
-- initially all the entries are true
freelist: array[1..NumPhysicalRegs] of boolean;

--freelist for vec regs
vfreelist: array[1..NumVecPhysicalRegs] of boolean;

-- rat maps logical regs to physical regs
-- since there are no failures/mispredicts etc a simple map should be enough
rat: array[1..(NumLogicalRegs+NumMRNRegs)] of PhysicalRegs;

vrat: array[1..(NumVecLogicalRegs + NumVecMRNRegs)] of VecPhysicalRegs;

--Inverse map from physical regs to logical regs
rrat: array[1..NumPhysicalRegs] of LogicalRegs;

vrrat: array[1..NumVecPhysicalRegs] of VecLogicalRegs;

-- pr_ready array says a physical reg is ready to be used
-- initially all are set to false
pr_ready: array[1..NumPhysicalRegs] of boolean;

vpr_ready: array[1..NumVecPhysicalRegs] of boolean;

-- latest consumer: tracks the consumer after which the prf can be freed
pr_consumers: array[1..NumPhysicalRegs] of UopID;

vpr_consumers: array[1..NumVecPhysicalRegs] of UopID;

-- current lbid. Wraps around
curr_lbid: LBID;

--current sb_id. Wraps around
curr_sbid: SBID;

-- execution units will use in_bypass to indicate availability of sources
-- Rob doesnt have to set it when an instruction retires
in_bypass: array[PhysicalRegs] of boolean;

in_bypass_vec: array[VecPhysicalRegs] of boolean;

rht: array[UopID] of RHT_S;

-- ROB

-- retire from head entry i.e. entry at index 1
-- allocate at tail = rob_tail
rob: array [1..NumROBEntries] of Ruop;
rob_tail: 1..NumROBEntries; -- used to allocate robid for uop
rob_head: 1..NumROBEntries; -- used to retire uop

-- TSB and SCB 
tsb: array [Slice] of array [LogicalRegs] of MemLocs;
vtsb: array [Slice] of array [VecLogicalRegs] of MemLocs;

scb_id: SCBID;
scb: array[SCBID] of Ruop;


/* lighthouse, assumes etcinstrumentation */
num_split_stores: 0..10;
num_add_uops: 0..10;
num_load_int_uops : 0..10;
num_load_vec_uops : 0..10;
num_store_int_uops : 0..10;
num_store_vec_uops : 0..10;

/* checker stuff */

retire_rat: array[LogicalRegs] of PhysicalRegs;
retire_rat_vec: array[VecLogicalRegs] of VecPhysicalRegs;

/*Functions and procedures */

/* Hardcoding mrn_map, slicing, store/load pcs */

function get_slice(a: MemLocs): Slice;
begin
  return (((a-1) / NumSlices) + 1);
end;

function is_load_pc(pc: PC):boolean;
begin
  if (pc = 2 | pc = 5) then 
    return (true);
  else 
    return(false);
  end;
end;

function is_store_pc(pc: PC):boolean;
begin
  if (pc = 1 | pc = 4) then 
    return (true);
  else 
    return(false);
  end;
end;

function is_add_pc(pc: PC): boolean;
begin
  return(pc = 3);
end;

function is_store(uop: UopType): boolean;
var res: boolean;
begin
  if (uop = Store_int | uop = Store_int_mrn | uop = Store_vec | uop = Store_vec_mrn) then
    res := true;
  else 
    res := false;
  end;
  return(res);
end;

function is_load(uop: UopType): boolean;
var res: boolean;
begin
  if (uop = Load_int | uop = Load_int_mrn | uop = Load_vec | uop = Load_vec_mrn) then
    res := true;
  else 
    res := false;
  end;
  return(res);
end;

function is_int_uop(uop: UopType): boolean;
begin
  if(uop = Add | uop = Store_int | uop = Store_int_mrn | uop = Load_int | uop = Load_int_mrn) then 
    return(true);
  else return(false);
  end;
end;

function is_non_mrn_reg(r: LogicalRegs):boolean;
begin
  return(r <= NumLogicalRegs)
end;

function is_non_vec_mrn_reg(r: VecLogicalRegs):boolean;
begin
  return(r <= NumVecLogicalRegs)
end;


function create_map_1(): MRNMap;
var m: MRNMap;
begin
  m.lpc := 2;
  m.spc := 1;
  return(m);
end;

function create_map_2(): MRNMap;
var m: MRNMap;
begin
  m.lpc := 5;
  m.spc := 4;
  return(m);
end;


function next_mem_addr(addr: MemLocs): MemLocs;
var next: MemLocs;
begin
  if (addr = NumMemLocs) then next := 1;
  else next := (addr + 1);
  end;
  assert(next <= NumMemLocs);
  return(next);
end;

function next_sbid(id: SBID): SBID;
begin
  if (id = NumStBufEntries) then return (1);
  else return ( id + 1);
  end;
end;

function next_lbid(id: LBID): LBID;
begin
  if (id = NumLoadBufEntries) then return (1);
  else return ( id + 1);
  end;
end;

function next_scbid(id: SCBID): SCBID;
begin
  if (id = NumSCBEntries) then return (1);
  else return ( id + 1);
  end;
end;

 /* For this version assume every load & store is in mrn_map */
function exists_in_mrn_map(pc: PC): boolean;
var ex: boolean;
begin
  /*ex := false;
  for i : 0 .. NumMRNMaps do
    if (mrn_map[i].lpc = pc | mrn_map[i].spc = pc) then  
      ex := true;
    end;
  end;
  return(ex);*/

  /* For this version assume every load & store is in mrn_map */
  return(true);
end;

function exists_in_vec_mrn_map(pc: PC): boolean;
var ex: boolean;
begin
  return(true);
end;


function reg_of(m: MRN_ID): LogicalRegs;
var temp: 0..NumMRNRegs;
begin
/* LogicalRegs includes MRN Regs but NumLogicalRegs are pure logical regs
   So MRN regs start after NumLogicalRegs in LogicalRegs
 */
  /* Mod by NumColors and add one*/
  temp := m % NumMRNRegs;
  if temp = 0 then temp := NumMRNRegs; end;
  return (temp + NumLogicalRegs);
end;

function vec_reg_of(m: VMRN_ID): VecLogicalRegs;
var temp: 0..NumVecMRNRegs;
begin
  /* VecLogicalRegs includes MRN Regs but NumVecLogicalRegs are pure vec logical regs
   So MRN regs start after NumLogicalRegs in LogicalRegs
 */
  /* Mod by NumColors and add one*/
  /*put "vmrn_id is:\n";
  put m;
  put "NumColors is:\n";
  put NumColors;
  put "NumVecLogicalRegs is:\n";
  put NumVecLogicalRegs;*/
  temp := m % NumVecMRNRegs;
  if temp = 0 then temp := NumVecMRNRegs; end;
  return (temp + NumVecLogicalRegs);
end;

function color_of(m:MRN_ID): MRNColors;
begin
  return((m / NumColors)+1);
end;

function vec_color_of(m:VMRN_ID): MRNColors;
begin
  return((m / NumColors)+1);
end;

function next_mrn_id(): MRN_ID;
begin
  if curr_mrn_id = MaxMRNID then 
    return(1);
  else 
    return(curr_mrn_id + 1);
  end;
end;

/* Notes: return type was MRN_ID when it should be VMRN_ID
  Murphi type checker didn't catch it. Maybe
  because MRN_ID is superset of VMRN_ID?
  */
function next_vmrn_id(): VMRN_ID;
begin
  if curr_vmrn_id = MaxVecMRNID then 
    return(1);
  else 
    return(curr_vmrn_id + 1);
  end;
end;

/* Similar to above but increments curr_mrn_id */
function get_next_mrn_id(): MRN_ID;
begin
   if curr_mrn_id = MaxMRNID then 
    curr_mrn_id := 1;
  else 
    curr_mrn_id := curr_mrn_id + 1;
  end;
  return(curr_mrn_id);
end;

/* Similar to above but increments curr_mrn_id */
function get_next_vmrn_id(): VMRN_ID;
begin
   if curr_vmrn_id = MaxVecMRNID then 
    curr_vmrn_id := 1;
  else 
    curr_vmrn_id := curr_vmrn_id + 1;
  end;
  return(curr_vmrn_id);
end;

procedure increment_curr_mrn_id();
begin
   if curr_mrn_id = MaxMRNID then 
    curr_mrn_id := 1;
  else 
    curr_mrn_id := curr_mrn_id + 1;
  end;
end;

procedure increment_curr_vmrn_id();
begin
   if curr_vmrn_id = MaxVecMRNID then 
    curr_vmrn_id := 1;
  else 
    curr_vmrn_id := curr_vmrn_id + 1;
  end;
end;

function latest_mrn_id_for_pc(pc: PC): MRN_ID_Ext;
var temp: MRN_ID_Ext;
begin
  if (!isundefined(latest_mrn_id[pc])) then
    temp := latest_mrn_id[pc];
  else
    temp := 0;
  end;
  return(temp);
end;

function latest_vmrn_id_for_pc(pc: PC): VMRN_ID_Ext;
var temp: VMRN_ID_Ext;
begin
  /*put "entered latest_vmrn_id_for_pc\n";
  put pc;*/
  if (!isundefined(latest_vec_mrn_id[pc])) then
    temp:= latest_vec_mrn_id[pc];
  else
    temp := 0;
  end;
  return(temp);
end;


function pick_latest_mrn_id(id1: MRN_ID; id2: MRN_ID): MRN_ID;
var s1: MRN_ID;
var s2: MRN_ID;
var n: MRN_ID;
var l: MRN_ID;
begin
  /* At least one id has to be proper MRN ID */
  assert( !(id1 = 0 & id2 = 0));
  if(id1  = 0) then return (id2); end;
  if(id2 = 0) then return(id1); end;

  n := next_mrn_id();
  /* s1, s2 in sorted order */
  if (id1 < id2) then 
    s1 := id1;
    s2 := id2;
  else 
    s1 := id2;
    s2 := id1;
  end;
  if (n = s1) then l := s1;
  else if (n = s2) then l := s2;
  else if(n < s1 & n < s2) then l := s2;
  else if(s1 < n & n < s2) then l := s1;
  else if (s1 < n & n < s2) then l := s2;
  else assert false;
  end; end;end; end; end;
  return(l);
end;

function pick_latest_vmrn_id(id1: VMRN_ID; id2: VMRN_ID): VMRN_ID;
var s1: VMRN_ID;
var s2: VMRN_ID;
var n: VMRN_ID;
var l: VMRN_ID;
begin

  /* At least one id has to be proper MRN ID */
  assert( !(id1 = 0 & id2 = 0)); 
  if (id1 = 0) then return (id2); end;
  if(id2 = 0) then return(id1); end;

  n := next_vmrn_id();
  /* s1, s2 in sorted order */
  if (id1 < id2) then 
    s1 := id1;
    s2 := id2;
  else 
    s1 := id2;
    s2 := id1;
  end;
  if (n = s1) then l := s1;
  else if (n = s2) then l := s2;
  else if(n < s1 & n < s2) then l := s2;
  else if(s1 < n & n < s2) then l := s1;
  else if (s1 < n & n < s2) then l := s2;
  else assert false;
  end; end;end; end; end;
  return(l);
end;

/* PC here is a load pc */
function get_latest_matching_mrn_id(pc: PC): MRN_ID;
var latest_mrn: MRN_ID;
var next_mrn: MRN_ID;
var temp: MRN_ID_Ext;
begin
  --put "entered get_latest_matching_mrn_id\n pc is:\n";
  next_mrn := next_mrn_id();
  --put "next_mrn_id is:\n";
  --put next_mrn;
  latest_mrn := next_mrn;
  for i: 1..NumMRNMaps do
    if (mrn_map[i].lpc = pc) then 
      /*put "in for-loop:\n";
      put i; put mrn_map[i];*/
      temp := latest_mrn_id_for_pc(mrn_map[i].spc);
      /*put "after latest_mrn_id_for_pc\ntemp is:\n";
      put temp;*/
      if (temp != 0) then
        latest_mrn := pick_latest_mrn_id(temp, latest_mrn);
      end;
      /*put "after pick_latest_mrn_id:\n";
      put latest_mrn;*/
    end;
  end;
  /* if latest_mrn is still next_mrn_id it means no match was found. 
  * Should this be an error?? Todo: Check.
  */
  if (latest_mrn = next_mrn) then increment_curr_mrn_id(); end;
  return(latest_mrn);
end;

/* PC here is a load pc */
function get_latest_matching_vmrn_id(pc: PC): VMRN_ID;
var latest_vmrn: VMRN_ID;
var next_vmrn: VMRN_ID;
var temp: VMRN_ID_Ext;
begin
  --put "entered get_latest_matching_vmrn_id\n";
  next_vmrn := next_vmrn_id();
  
  latest_vmrn := next_vmrn;
  for i : 1 .. NumMRNMaps do
    if (mrn_map[i].lpc = pc) then 
      temp := latest_vmrn_id_for_pc(mrn_map[i].spc);  
      if(temp != 0) then    
        latest_vmrn := pick_latest_vmrn_id(temp, latest_vmrn);
      end;
    end;
  end;
  /* if latest_mrn is still next_mrn_id it means no match was found. 
  * Should this be an error?? Todo: Check.
  */
   if (latest_vmrn = next_vmrn) then increment_curr_vmrn_id(); end;
  return(latest_vmrn);
end;

procedure increment_numlines();
begin
  if (numlines = MaxLines) then 
    numlines := 1;
  else numlines := numlines + 1;
  end;
end;

procedure increment_numstrands();
begin
  if (numstrands = MaxStrands) then 
    numstrands := 1;
  else numstrands := numstrands + 1;
  end;
end;

procedure increment_stage_line_strand_nums();
begin
  if (lineindex = LineWidth) then 
      stage := FE; 
      lineindex := 1;
      increment_numlines();
      if (numlines % StrandWidth = 0) then
        increment_numstrands();
      end;
  else 
    lineindex := lineindex + 1;
  end;
end;

procedure increment_uop_count();
begin
  if (uop_count = MaxUopId) then uop_count := 1;
  else uop_count := uop_count + 1;
  end;
end;

function update_map(map: Map; l: LogicalRegs; p: PhysicalRegs; curr_pr: PhysicalRegs): Bool_Map;
var new_map: Map;
var res: boolean;
var bmap: Bool_Map;
begin
  if(isundefined(map.lreg)) then 
    new_map.lreg := l;
    new_map.new_preg := p;
    if(!isundefined(curr_pr)) then 
      new_map.old_preg := curr_pr; 
    else 
      undefine new_map.old_preg;
    end;
    res := true;
  else 
    if(map.lreg = l) then
      new_map.lreg := l;
      new_map.new_preg := p;
      if(!isundefined(curr_pr)) then 
        new_map.old_preg := curr_pr; 
      else 
        undefine new_map.old_preg;
      end;
      res := true;
      /* Old version
      new_map.new_preg := p;
      res := true;*/
    else  
      res := false;
    end;
  end;
  bmap.map := new_map;
  bmap.updated := res;
  return(bmap);
end;

/*
RAT: mapping from logical regs to physical regs.
*/
procedure update_rat(l: LogicalRegs; p: PhysicalRegs; uopid: UopID);
var curr_pr: PhysicalRegs;
var new_map: Map;
var bmap: Bool_Map;
begin
  if (!isundefined(rat[l])) then
    curr_pr := rat[l];
    undefine rrat[rat[l]];
  end;  
  rrat[p] := l;
  rat[l] := p;
  /* NumMaps = 2 is hard-coded into this */
  alias 
    maps: idq[uopid].rht.maps
  do
    bmap := update_map(maps[1], l, p, curr_pr);
    if(bmap.updated) then
      maps[1] := bmap.map;
    else
      bmap := update_map(maps[2], l, p, curr_pr);
      assert(bmap.updated);
      maps[2] := bmap.map;
    end;
  end;

end;

function update_vmap(map: VMap; l: VecLogicalRegs; p: VecPhysicalRegs; curr_pr: VecPhysicalRegs): Bool_VMap;
var res: boolean;
var new_vmap: VMap;
var bvmap: Bool_VMap;
begin
   if(isundefined(map.lreg)) then 
      new_vmap.lreg := l;
      new_vmap.new_preg := p;
      if(!isundefined(curr_pr)) then 
        new_vmap.old_preg := curr_pr; 
      else 
        undefine new_vmap.old_preg;
      end;
      res := true;
    else 
      if(map.lreg = l) then
        new_vmap.lreg := l;
        new_vmap.new_preg := p;
        if(!isundefined(curr_pr)) then 
          new_vmap.old_preg := curr_pr; 
        else 
          undefine new_vmap.old_preg;
        end;
        res := true;
        /* Old version
        new_vmap.new_preg := p;
        res := true;*/
      else  
        res := false;
      end;
    end;
    bvmap.updated := res;
    bvmap.vmap := new_vmap;
    return(bvmap);
end;

/*
RAT: mapping from logical regs to physical regs.
*/
procedure update_vrat(l: VecLogicalRegs; p: VecPhysicalRegs; uopid:UopID);
var curr_pr : VecPhysicalRegs;
var bvmap: Bool_VMap;
begin
  if (!isundefined(vrat[l])) then
    curr_pr := vrat[l];
    undefine vrrat[vrat[l]];
  end;  
  vrrat[p] := l;
  vrat[l] := p;
  alias 
    maps: idq[uopid].rht.vmaps
  do
    bvmap := update_vmap(maps[1], l, p, curr_pr);
    if(bvmap.updated) then
      maps[1] := bvmap.vmap;
    else
      bvmap := update_vmap(maps[2], l, p, curr_pr);
      assert(bvmap.updated);
      maps[2] := bvmap.vmap;
    end;
  end;
end;

/* Has side effect on idq[uop.uopid]. uop i.e. inflight[uopid] itself is not touched  */
function get_physical_reg(l: LogicalRegs; uop: Uop): PhysicalRegs;
var p: PhysicalRegs;
begin
  if !isundefined(rat[l]) then 
    return (rat[l]);
  else
    for i: PhysicalRegs do
      if freelist[i] then 
        p := i;       
      end;
    end;
    if(isundefined(p)) then
    /* Couldn't find any free physical regs */
			  assert (false);
    end;
    /* bug 3: Commented out code to set freelist */
    /*freelist[p] := false;*/
    update_rat(l, p, uop.uopid);
    if( !isundefined(lregs[l])) then
			pr[p] := lregs[l];
    else 
      undefine pr[p];
    end;
    return(p);
  end;
end;

/* Analog of above function for vec registers*/
function get_vec_physical_reg(l: VecLogicalRegs; uop: Uop): VecPhysicalRegs;
var p: VecPhysicalRegs;
begin
  if !isundefined(vrat[l]) then 
    return (vrat[l]);
  else
    for i: VecPhysicalRegs do
      if vfreelist[i] then 
        p := i;       
      end;
    end;
    if(isundefined(p)) then
    /* Couldn't find any free physical regs */
			  assert (false);
    end;
    vfreelist[p] := false;
    update_vrat(l, p, uop.uopid);
    if( !isundefined(vregs[l].high) | !isundefined(vregs[l].low)) then
				  vpr[p] := vregs[l];
    else undefine vpr[p];
    end;
    return(p);
  end;
end;

function get_fresh_physical_reg(): PhysicalRegs;
var p: PhysicalRegs;
begin
  for i: PhysicalRegs do
    if freelist[i] then 
      p := i;
    end;
  end;
  if(isundefined(p)) then assert (false); end;
  freelist[p] := false;
  return(p);
end;

function get_fresh_vec_physical_reg(): VecPhysicalRegs;
var vpr: VecPhysicalRegs;
begin
  for i: VecPhysicalRegs do
    if vfreelist[i] then 
      vpr := i;
    end;
  end;
  if(isundefined(vpr)) then assert (false); end;
  vfreelist[vpr] := false;
  return(vpr);
end;

function convert_to_ruop(uop: Uop): Ruop;
var ut: UopType;
var r: Ruop;
begin
  ut := uop.uoptype;
  r.uoptype := ut; 
  r.uopid := uop.uopid; 
  alias
    updated_uop : idq[uop.uopid]
  do
    switch ut
      case Add:
        /* check RAT or allocate a new physical reg */
        r.src1 := get_physical_reg(uop.src1, uop);
        if (uop.src1 != uop.src2) then
          r.src2 := get_physical_reg(uop.src2, uop);
        else
          r.src2 := r.src1;
        end;
        r.dst := get_fresh_physical_reg();
        update_rat(uop.src1, r.dst, uop.uopid);
        /* auxilliary vars */
        r.produced := r.dst;
        r.consumed1 := r.src1;
        r.consumed2 := r.src2;
        /* Bug 4: commenting out pr_ready[r.dst] := false
          This has the effect of allowing subsequent uops to proceed prematurely.
        */
        /*pr_ready[r.dst] := false;*/
        r.rht.maps := updated_uop.rht.maps;
        r.rht.vmaps := updated_uop.rht.vmaps;
        if (r.produced = r.consumed1) then  
          -- put "In convert_to_ruop:\n r.produced and r.consumed1 = ";
          -- put r.produced;
          -- put r.consumed1;
          -- put "\n";
          -- put "r.uottype is: ";
          -- put r.uoptype;
        end;
        if (r.produced = r.consumed2) then  
          -- put "In convert_to_ruop:\n r.produced and r.consumed2 = ";
          -- put r.produced;
          -- put r.consumed2;
          -- put "\n";
          -- put "r.uottype is: ";
          -- put r.uoptype;
        end;
        assert(r.produced != r.consumed1);
        assert(r.produced != r.consumed2);
      case Load_int:
      /* load_int dst mem_addr --> load_int dst src1, where src1 is mrn_reg */ 
        r.dst := get_physical_reg(uop.dst, uop); 
        /* auxilliary vars */
        r.produced := r.dst;
        pr_ready[r.dst] := false;
        r.rht.maps := updated_uop.rht.maps;
        r.rht.vmaps := updated_uop.rht.vmaps;
        r.mem_width := updated_uop.mem_width;
        r.mem_addr := updated_uop.mem_addr;
      case Load_int_mrn:
      /* load_int dst mem_addr --> load_int dst src1, where src1 is mrn_reg */ 
        r.src1 := get_physical_reg(uop.mrn_reg, uop); /* mrn_reg is the src of load op */
        r.dst := get_physical_reg(uop.dst, uop); 
        /* auxilliary vars */
        r.produced := r.dst;
        pr_ready[r.dst] := false;
        r.consumed1 := r.mrn_reg;
        r.rht.maps := updated_uop.rht.maps;
        r.rht.vmaps := updated_uop.rht.vmaps;
        r.mem_width := updated_uop.mem_width;
        r.mem_addr := updated_uop.mem_addr;
        if (!isundefined(uop.mrn_reg)) then
         r.mrn_reg := uop.mrn_reg;
        end;
      case Store_int:
        /* store src mem_addr --> store src dst where dst is mrn_reg */
        /* Todo: if there is no physical reg for uop.src1 then it's likely a mistake
            can we check for this?*/
        r.src1 := get_physical_reg(uop.src1, uop);
        r.dst := get_physical_reg(uop.mrn_reg, uop);
        r.consumed1 := r.src1;
        r.produced := r.dst;
        r.uoptype := Store_int_mrn;
        r.rht.maps := updated_uop.rht.maps;
        r.rht.vmaps := updated_uop.rht.vmaps;
        r.mem_width := updated_uop.mem_width;
        r.mem_addr := updated_uop.mem_addr;
        if (!isundefined(uop.mrn_reg)) then
          r.mrn_reg := uop.mrn_reg;
        end;
    end;
  end;
  return (r);
end;

/* Same function but for vec uoptypes */

function convert_to_ruop_vec(uop: Uop): Ruop;
var ut: UopType;
var r: Ruop;
begin
  ut := uop.uoptype;
  r.uoptype := ut; 
  r.uopid := uop.uopid; 
  alias
    updated_uop: idq[uop.uopid]
  do
    switch ut
      case Add:
        /* check RAT or allocate a new physical reg */
        r.vsrc1 := get_vec_physical_reg(uop.src1,uop);
        if (uop.vsrc1 != uop.vsrc2) then
          r.src2 := get_vec_physical_reg(uop.src2,uop);
        else
        r.vsrc2 := r.vsrc1;
        end;
        r.vdst := get_fresh_vec_physical_reg();
        update_vrat(uop.vsrc1, r.vdst, uop.uopid);
        /* auxilliary vars */
        r.vproduced := r.vdst;
        r.vconsumed1 := r.vsrc1;
        r.vconsumed2 := r.vsrc2;
        vpr_ready[r.vdst] := false;
        r.rht.maps := updated_uop.rht.maps;
        r.rht.vmaps := updated_uop.rht.vmaps;
        if (r.vproduced = r.vconsumed1) then  
          -- put "In convert_to_ruop_vec:\n r.vproduced and r.vconsumed1 = ";
          -- put r.vproduced;
          -- put r.vconsumed1;
          -- put "\n";
          -- put "r.uoptype is: ";
          -- put r.uoptype;
        end;
        if (r.vproduced = r.vconsumed2) then  
          -- put "In convert_to_ruop_vec:\n r.vproduced and r.vconsumed2 = ";
          -- put r.vproduced;
          -- put r.vconsumed2;
          -- put "\n";
          -- put "r.uoptype is: ";
          -- put r.uoptype;
        end;
        assert(r.vproduced != r.vconsumed1);
        assert(r.vproduced != r.vconsumed2);
      case Load_vec:
        /* load_vec vdst mem_addr --> load_vec vdst vmrn_reg */
        r.vdst := get_vec_physical_reg(uop.vdst, uop);
        /* auxilliary vars */
        r.vproduced := r.vdst;
        vpr_ready[r.vdst] := false;
        r.rht.maps := updated_uop.rht.maps;
        r.rht.vmaps := updated_uop.rht.vmaps;
        r.mem_width := updated_uop.mem_width;
        r.mem_addr := updated_uop.mem_addr;
        /* Check: required? Added later: not required but no harm*/
        if (!isundefined(uop.vmrn_reg)) then
          r.vmrn_reg := uop.vmrn_reg;
        end;
      case Load_vec_mrn:
        /* load_vec vdst mem_addr --> load_vec vdst vmrn_reg */
        r.vsrc1 := get_vec_physical_reg(uop.vmrn_reg, uop);
        r.vdst := get_vec_physical_reg(uop.vdst, uop);
        /* auxilliary vars */
        r.vproduced := r.vdst;
        vpr_ready[r.vdst] := false;
        r.vconsumed1 := r.vsrc1; 
        r.rht.maps := updated_uop.rht.maps;
        r.rht.vmaps := updated_uop.rht.vmaps;
        r.mem_width := updated_uop.mem_width;
        r.mem_addr := updated_uop.mem_addr;
        if (!isundefined(uop.vmrn_reg)) then
          r.vmrn_reg := uop.vmrn_reg;
        end;
      case Store_vec:
        /* load_vec vsrc mem_addr --> load_vec vsrc vmrn_reg */
        /* Todo: if there is no physical reg for uop.src1 then it's likely a mistake
            can we check for this?*/
        r.vsrc1 := get_physical_reg(uop.vsrc1, uop);
        r.vdst := get_physical_reg(uop.vmrn_reg, uop);
        r.vconsumed1 := r.vsrc1;
        r.vproduced := r.vdst;
        r.uoptype := Store_vec_mrn;
        r.rht.maps := updated_uop.rht.maps;
        r.rht.vmaps := updated_uop.rht.vmaps;
        r.mem_width := updated_uop.mem_width;
        r.mem_addr := updated_uop.mem_addr;
        if (!isundefined(uop.vmrn_reg)) then
          r.vmrn_reg := uop.vmrn_reg;
        end;
    end;
  end;
  return (r);
end;

procedure increment_rob_head();
begin
  if rob_head = NumROBEntries then
    rob_head := 1;
  else 
    rob_head := rob_head + 1; 
  end;
end;

procedure increment_rob_tail();
begin
  if rob_tail = NumROBEntries then
        rob_tail := 1;
  else 
        rob_tail := rob_tail + 1; 
  end;
end;

procedure apply_to_recover_rat(m: Map);
  begin
    if (!isundefined(m.lreg)) then 
      assert(!isundefined(m.new_preg));
      if(!isundefined(m.old_preg)) then
        rat[m.lreg] := m.old_preg;
      else
        undefine rat[m.lreg];
      end;
    end;
  end;

procedure apply_to_recover_rat_v(m: VMap);
  begin
    if (!isundefined(m.lreg)) then 
      assert(!isundefined(m.new_preg));
      if(!isundefined(m.old_preg)) then
        rat[m.lreg] := m.old_preg;
      else
        undefine rat[m.lreg];
      end;
    end;
  end;
 

/* Reconstruct RAT, LBID, SBID. 
   Store the deltas at RA1 stage into RHT and walk that backwards. 
   For both these start the at the bottom or latest uop and go upwards 
   towards the offend load uop */
       
procedure recover_rat(ruop: Ruop);
var i: UopID;
begin
  i := uop_count - 1; /* uop_count is assumed to be empty*/
  assert(isundefined (inflight_r[uop_count].uoptype));
  while i >= ruop.uopid do
    alias 
      prev_lbid : rht[i].prev_lbid;
      prev_sbid : rht[i].prev_sbid;
    do
      for m: 1..NumMaps do
        apply_to_recover_rat(rht[i].maps[m]);
        apply_to_recover_rat_v(rht[i].vmaps[m]);
      end;
      if (!isundefined(prev_lbid)) then
        curr_lbid := prev_lbid;
      end;
      if(!isundefined(prev_sbid)) then
          curr_sbid := prev_sbid;
      end;
    end;
    i := i - 1;
  end;
end;

/* return 0 (fail), 1(pass), 2 (try L1 check) */
function l0_check(r: Ruop): 0..2;
var str: 0..MaxStrands;
var s: Ruop; 
var l_m: UopID; /* last store using the same mrm reg */
var l_a: UopID; /* last store using the same addr */
begin
  /*
    This check is within the strand. Find the latest store using the same mrn_reg (l_m) and the latest store writing to the 
    same mem_addr (l_a) as our load. If they are the same then check passes.
     If l_m has different address than uop then l0 check fail
     Otherwise, l_m doesn't exist and l_a exists then l0 check fail
     If l_m and l_a don't exist then use L1 check.
  */
  /*the last store to use same mrn reg and the last store the mem_addr should be the same.
  If we dont find either of these then return 2 and pass the check to L1*/

  /* this doesn't seem correct. We want same mrn-reg --> same address
   If we find same mrn_reg --> different address then it should not return
   2 but actually return 0 (that is failed).
   2 should be returned only if last_Addr and last_mrn are both not found
   in the strand.
   TODO: check with Rafael.
   
   */
  str := r.strand;

  for i: UopID do
    if i < uop_count then
      s := inflight_r[i];
      if(s.strand = str) then
        if is_store(s.uoptype) then
          if (s.mem_addr = r.mem_addr) then 
            l_a := s.uopid;
            /*addr_uop := s;*/
          end;
          if (!isundefined(s.mrn_reg) & s.mrn_reg = r.mrn_reg) then  
            l_m := s.uopid;
            /*mrn_uop := s;*/
          end;
        end;
      end;
    end;
  end;
  if (!isundefined(l_a) & !isundefined(l_m)) then
    if (l_a = l_m) then return 1;
    else return 0;
    end;
  else 
    if (isundefined(l_m) & isundefined(l_a)) then  
      return 2;
    else return 0;
    end;
  end;
end;

function l1_check(r: Ruop): boolean;
var s: Slice;
begin
  s := get_slice(r.mem_addr);
  if (tsb[s][r.mrn_reg] = r.mem_addr) then
    return true
  else return false;
  end;
end;

function l1_check_vec(r: Ruop): boolean;
var s1: Slice;
var s2: Slice;
begin
  s1 := get_slice(r.mem_addr);
  s2 := get_slice(next_mem_addr(r.mem_addr)); 
  if (r.mem_width = 1) then
    if (vtsb[s1][r.vmrn_reg] = r.mem_addr) then
     return true;
    else return false;
    end;
  else
    /* Bug: should be & not | */
    if(vtsb[s1][r.vmrn_reg] = r.mem_addr | vtsb[s2][r.vmrn_reg] = next_mem_addr(r.mem_addr)) then  
      return true;
    else 
      return false;
    end;
  end;
end;

function mrn_load_check(r: Ruop): boolean;
var l0_res: 0..2;
begin
  l0_res := l0_check(r);
  if (l0_res = 2) then return l1_check(r);
  else 
    return (l0_res = 1)
  end;
end;

function mrn_load_check_vec(r: Ruop): boolean;
var l0_res: 0..2;
begin
  l0_res := l0_check(r);
  if (l0_res = 2) then return l1_check_vec(r);
  else 
    return (l0_res = 1)
  end;
end;

/* is address read by r1 covered by addresses written to be r2 */
function address_covered(r1: Ruop; r2: Ruop): boolean;
var res: boolean;
var r1a: MemLocs;
var r2a: MemLocs;
begin
  assert(is_store(r2.uoptype));
  assert(is_load(r1.uoptype));
  r1a := r1.mem_addr;
  r2a := r2.mem_addr;
  res := false;
  /* cases: r1 vec r2 scalar, r1 vec r2 vec, r1 scalar r2 scalar, r1 scalar r2 vec */
  if(r2.mem_width = 2) then
    if(r1.mem_width = 1) then
      if (r1a = r2a | r1a = r2a + 1) then res := true; end;
    else /* r1.width = 2*/
      if (r1a = r2a | r1a = r2a + 1 | r1a + 1 = r2a) then res := true; end;
    end;
  else /* r2.width = 1*/
    if(r1.mem_width = 1) then
      if (r1a = r2a) then res := true; end;
    else /* r1.width = 2*/
      if (r1a = r2a | r1a + 1 = r2a) then res := true; end;
    end;
  end;
  return(res);
end;

function mo_clear(r: Ruop): boolean;
var res: boolean;
var ruop: Ruop;
begin
  res := false;
  /* Check stores in Mem2 first*/
  for i: UopID do
    if (i < uop_count) then
      ruop := inflight_r[i];
      if is_store(ruop.uoptype) then
        if ruop.uopstage = Mem2 then
          if address_covered(r, ruop) then
            res := true;
          end;
        end;
      end;
    end;
  end;
  /* walk scb now */
  for i: SCBID do
    if (i < scb_id) then
      ruop := scb[i];
      if address_covered(r, ruop) then
        res := true
      end;
    end;
  end;
  return res;
end;

function is_ready_int_operand(c: PhysicalRegs): boolean;
begin
  if(isundefined(c)) then return (true); 
  else 
    if (!isundefined(pr_ready[c]) & pr_ready[c] | 
      !isundefined(in_bypass[c]) & in_bypass[c]) then
      return(true);
    else return(false);
    end;
  end;
end;

function is_ready_vec_operand(c: VecPhysicalRegs): boolean;
begin
  if(isundefined(c)) then return (true);
  else 
    if (!isundefined(vpr_ready[c]) & vpr_ready[c] |
       !isundefined(in_bypass_vec[c]) & in_bypass_vec[c]) then
      return(true);
    else return(false);
    end;
  end;
end;

function operands_ready(ruop: Ruop): boolean;
var c1: PhysicalRegs;
var c2: PhysicalRegs;
var vc1: VecPhysicalRegs;
var vc2: VecPhysicalRegs;
begin
  if (is_int_uop(ruop.uoptype)) then
    return (is_ready_int_operand(ruop.consumed1) & is_ready_int_operand(ruop.consumed2));

   /* c1 := ruop.consumed1;
    c2 := ruop.consumed2;
    if ((!isundefined(pr_ready[c1]) & pr_ready[c1] | !isundefined(in_bypass[c1]) & in_bypass[c1]) &
      (!isundefined(pr_ready[c2]) & pr_ready[c2] | !isundefined(in_bypass[c2]) & in_bypass[c2])) then
        return (true);
    else
      return (false);
    end;*/
  else
    return(is_ready_vec_operand(ruop.vconsumed1));
    /*vc1 := ruop.vconsumed1;
    if ((!isundefined(vpr_ready[vc1]) & vpr_ready[vc1] | !isundefined(in_bypass_vec[vc1]) & in_bypass_vec[vc1])) then
        return (true);
    else
      return (false);
    end;*/
  end;
end;


/* ruop.uoptype is assumed to be Store_* */
function all_prec_loads_stores_cleared(r: Ruop): boolean;
var prec_sbid: SBID;
var prec_lbid: LBID;
var res: boolean;
var ruop: Ruop;
begin
  res := false;
  for i: UopID do
    if (i < uop_count) then
      ruop := inflight_r[i];
if (!isundefined(ruop.uoptype) & is_store(ruop.uoptype)) then
        if(ruop.sbid < r.sbid & ruop.uopstage = Mem1) then res := false; end;
      end;
      if (!isundefined(ruop.uoptype) & is_load(ruop.uoptype)) then
        if(ruop.lbid < r.prec_lbid & ruop.uopstage = Mem1) then res := false; end;
      end;
    end;
  end;
  return res;
end;

procedure apply_to_retire_rat(m: Map);
  begin
    if (!isundefined(m.lreg)) then 
      assert(!isundefined(m.new_preg));
      retire_rat[m.lreg] := m.new_preg;
    end;
  end;

procedure apply_to_retire_rat_vec(m: VMap);
  begin
    if (!isundefined(m.lreg)) then 
      assert(!isundefined(m.new_preg));
      retire_rat_vec[m.lreg] := m.new_preg;
    end;
  end;

procedure update_in_order_mc(ruop: Ruop);
  begin
    alias
      uop : idq[ruop.uopid];
      src1: uop.src1;
      src2: uop.src2;
      vsrc1: uop.vsrc1;
      dst: uop.dst;
      vdst: uop.vdst;
      addr: uop.mem_addr;
      width: uop.mem_width;
      utype: uop.uoptype;
    do
    switch utype
      case Add:
        cregs[src1] := cregs[src1] + cregs[src2]
      case Load_int:
        cregs[dst] := cmem[addr]
      case Store_int:
        cmem[addr] := cregs[src1];
      case Load_vec:
        if width = 1 then
          cvecregs[vdst].low := cmem[addr];
          cvecregs[vdst].high:= 0; /* zero-ing out high bits */
        else
          cvecregs[vdst].high := cmem[addr];
          cvecregs[vdst].low := cmem[next_mem_addr(addr)];
        end;
      case Store_vec:
        if width = 1 then
          cmem[addr] := cvecregs[vsrc1].low;
        else
          cmem[addr] := cvecregs[vsrc1].high;
          cmem[next_mem_addr(addr)] := cvecregs[vsrc1].low;
        end;
    end;
    end;
  end;

/* Startstate */

/* 
  (1) initialize pc -> load/store/add map. This cannot change once chose initially. 
      ==> Done in is_load_pc and is_store_pc functions
  (2) initialize store pc, load pc mrn pairs. This can change during the execution.
      ==> Done in create_map_1/2. Currently hardcoded
  (3) initialize mem locs -> slice map. Have three slices for now.
      ==> Done in get_slice_of function. Hardcoded and cannot change.
*/

function get_modval_pr(i: PhysicalRegs): RegVal;
begin
  if (i % MaxRegVal) > 0 then
    return(i % MaxRegVal);
  else
    return(1);
  end;
end;

function get_modval_vpr(i: VecPhysicalRegs): RegVal;
begin
  if (i % MaxRegVal) > 0 then
    return(i % MaxRegVal);
  else
    return(1);
  end;
end;

function get_modval_lr(i: LogicalRegs): RegVal;
begin
  if (i % MaxRegVal) > 0 then
    return(i % MaxRegVal);
  else
    return(1);
  end;
end;

function get_modval_vlr(i: VecLogicalRegs): RegVal;
begin
  if (i % MaxRegVal) > 0 then
    return(i % MaxRegVal);
  else
    return(1);
  end;
end;

function get_modval_mem(i: MemLocs): RegVal;
begin
  if (i % MaxRegVal) > 0 then
    return(i % MaxRegVal);
  else
    return(1);
  end;
end;


function youngest_store_with_matching_mrn_reg(uop: Uop): UopID_Ext;
var line: 0..MaxLines;
var u: Uop;
var uopid: UopID_Ext;
begin
  line := uop.line_id;
  uopid := Invalid_Uopid;
  for i: UopID do
    if (i < uop.uopid) then
      u := inflight[i];
      if (!isundefined(u.uoptype)) &
	(u.uoptype = Store_int | u.uoptype = Store_int_mrn) &
	( u.line_id = line) then
        if(u.mrn_reg = uop.mrn_reg) then uopid := i; end;
      end;
    end;
  end;
  return (uopid);
end;

function mrn_check(uop: Uop): boolean;
var uopid: UopID_Ext;
begin
  uopid := youngest_store_with_matching_mrn_reg(uop);
  if (!(uopid = Invalid_Uopid)) then
    if( inflight[uopid].mrn_color = uop.mrn_color) then 
      return(true);
    else 
    /* Invariant about the gap between store and load uopid in
      case of failing load check in FE
    */
    assert(uop.uopid  - uopid >= NumMRNRegs*NumColors);
    return(false);
    end;
  else
    if (!isundefined(mrn_reg_color_map[uop.mrn_reg]) & 
     (mrn_reg_color_map[uop.mrn_reg] = uop.mrn_color)) then 
      return(true);
    else return(false);
    end;
  end;
end;

function youngest_store_with_matching_mrn_vreg(uop: Uop): UopID_Ext;
var line: 0..MaxLines;
var u: Uop;
var uopid: UopID_Ext;
begin
  line := uop.line_id;
  uopid := Invalid_Uopid;
  for i: UopID do
    if (i < uop.uopid) then
      u := inflight[i];
      if (!isundefined(u.uoptype)) &
	(u.uoptype = Store_vec | u.uoptype = Store_vec_mrn)
	& (u.line_id = line) then
        if(u.vmrn_reg = uop.vmrn_reg) then uopid := i; end;
      end;
    end;
  end;
  
  --put uopid;
  assert (!isundefined(uopid));
  return (uopid);
end;



function mrn_check_vec(uop: Uop): boolean;
var uopid: UopID_Ext;
begin
  
  uopid := youngest_store_with_matching_mrn_vreg(uop);
  
  if (!(uopid = Invalid_Uopid)) then
    if( inflight[uopid].mrn_color = uop.mrn_color) then 
      return(true);
    else 
      assert(uop.uopid  - uopid >= NumVecMRNRegs*NumColors);
      return(false);
    end;
  else
    if (!isundefined(mrn_reg_color_map_vec[uop.vmrn_reg]) & 
      (mrn_reg_color_map_vec[uop.vmrn_reg] = uop.mrn_color)) then 
      return(true);
    else return(false);
    end;
  end;
end;


Startstate
begin
  stage := DQ;
  uop_count := 1;
  lineindex := 1;
  numlines := 0;
  numstrands := 0;
  scb_id := 1;
  curr_sbid := 1;
  curr_lbid := 1;
  curr_mrn_id := 1;
  curr_vmrn_id := 1;
  for i : PhysicalRegs do 
    freelist[i] := true;
    pr_ready[i] := true;
    /* some init val for pr */
    pr[i] := get_modval_pr(i);
  end;
  for i : VecPhysicalRegs do 
    vfreelist[i] := true;
    vpr_ready[i] := true;
    /* some init val for pr */
    vpr[i].high := get_modval_vpr(i);
    vpr[i].low := get_modval_vpr(i);
  end;
  for i: LogicalRegs do
    lregs[i] := get_modval_lr(i); 
    cregs[i] := get_modval_lr(i); 
  end;
  for i: VecLogicalRegs do
    vregs[i].high := get_modval_vlr(i); 
    vregs[i].low := get_modval_vlr(i); 
    cvecregs[i].high := get_modval_vlr(i); 
    cvecregs[i].low := get_modval_vlr(i); 
  end;
  for i: MemLocs do
    mem[i] := get_modval_mem(i);
    cmem[i] := get_modval_mem(i);
  end;
/*
  for i : 1..NumAddUnits do 
    add_units_free[i] := true;
  end;
  for i: 1..NumLoadBufEntries do
    ld_buffer_free[i] := true;
    ld_buffer_done[i] := false;
  end;
  for i: 1..NumStBufEntries do
    st_buffer_free[i] := true;
  end;
  for i: 1..NumBrUnits do
    br_units_free[i] := true;
  end;
  rsq_tail := 1;  */
  rob_tail := 1;
  rob_head := 1;
  /* initialize mrn_map. Hardcoded currently */
  mrn_map[1] := create_map_1();
  mrn_map[2] := create_map_2();

  /* lighthouses */
  num_split_stores := 0;

/* assumes */
num_add_uops := 0;
num_load_int_uops := 0;
num_load_vec_uops := 0;
num_store_int_uops := 0;
num_store_vec_uops := 0;

end ;


/* Lighthouse
startstate
begin
  stage := RA1;
  uop_count := 5;
  idq[1].uoptype := Store_vec;
  idq[1].pc := 1;
  idq[1].mem_addr := 1;
  idq[1].vsrc1 := 1;
  idq[1].mem_width := 1;
  idq[1].uopid := 1;
  idq[1].uopstage := FE;
  idq[1].line_id := 0;
  idq[1].strand_id := 0;
  idq[1].rht.maps[1].lreg := 1;
  idq[1].rht.maps[1].new_preg := 8;
  idq[1].rht.maps[2].lreg := 4;
  idq[1].rht.maps[2].new_preg := 7;
  idq[2].uoptype := Store_vec;
  idq[2].pc := 1;
  idq[2].mem_addr := 1;
  idq[2].vsrc1 := 1;
  idq[2].mem_width := 1;
  idq[2].uopid := 2;
  idq[2].uopstage := FE;
  idq[2].line_id := 0;
  idq[2].strand_id := 0;
  idq[2].rht.maps[1].lreg := 3;
  idq[2].rht.maps[1].new_preg := 6;
  idq[3].uoptype := Load_vec;
  idq[3].pc := 2;
  idq[3].mem_addr := 1;
  idq[3].vdst := 1;
  idq[3].mem_width := 1;
  idq[3].uopid := 3;
  idq[3].uopstage := FE;
  idq[3].line_id := 1;
  idq[3].strand_id := 0;
  idq[4].uoptype := Load_vec;
  idq[4].pc := 2;
  idq[4].mem_addr := 1;
  idq[4].vdst := 1;
  idq[4].mem_width := 1;
  idq[4].uopid := 4;
  idq[4].uopstage := FE;
  idq[4].line_id := 1;
  idq[4].strand_id := 0;
  inflight[3].uoptype := Load_vec;
  inflight[3].pc := 2;
  inflight[3].mem_addr := 1;
  inflight[3].vdst := 1;
  inflight[3].mem_width := 1;
  inflight[3].uopid := 3;
  inflight[3].uopstage := RA1;
  inflight[3].line_id := 1;
  inflight[3].strand_id := 0;
  inflight[3].mrn_valid := true;
  inflight[3].vmrn_reg := 4;
  inflight[3].mrn_color := 2;
  inflight[4].uoptype := Load_vec;
  inflight[4].pc := 2;
  inflight[4].mem_addr := 1;
  inflight[4].vdst := 1;
  inflight[4].mem_width := 1;
  inflight[4].uopid := 4;
  inflight[4].uopstage := RA1;
  inflight[4].line_id := 1;
  inflight[4].strand_id := 0;
  inflight[4].mrn_valid := true;
  inflight[4].vmrn_reg := 3;
  inflight[4].mrn_color := 2;
  inflight_r[1].uoptype := Store_vec_mrn;
  inflight_r[1].vsrc1 := 8;
  inflight_r[1].vdst := 7;
  inflight_r[1].vmrn_reg := 4;
  inflight_r[1].mem_addr := 1;
  inflight_r[1].mem_width := 1;
  inflight_r[1].uopid := 1;
  inflight_r[1].uopstage := RA2;
  inflight_r[1].robid := 1;
  inflight_r[1].sbid := 1;
  inflight_r[1].prec_lbid := 1;
  inflight_r[1].vproduced := 7;
  inflight_r[1].vconsumed1 := 8;
  inflight_r[1].rht.maps[1].lreg := 1;
  inflight_r[1].rht.maps[1].new_preg := 8;
  inflight_r[1].rht.maps[2].lreg := 4;
  inflight_r[1].rht.maps[2].new_preg := 7;
  inflight_r[1].retired := true;
  inflight_r[2].uoptype := Store_vec_mrn;
  inflight_r[2].vsrc1 := 8;
  inflight_r[2].vdst := 6;
  inflight_r[2].vmrn_reg := 3;
  inflight_r[2].mem_addr := 1;
  inflight_r[2].mem_width := 1;
  inflight_r[2].uopid := 2;
  inflight_r[2].uopstage := RA2;
  inflight_r[2].robid := 2;
  inflight_r[2].sbid := 2;
  inflight_r[2].prec_lbid := 1;
  inflight_r[2].vproduced := 6;
  inflight_r[2].vconsumed1 := 8;
  inflight_r[2].rht.maps[1].lreg := 3;
  inflight_r[2].rht.maps[1].new_preg := 6;
  numlines := 2;
  lineindex := 1;
  lregs[1] := 1;
  lregs[2] := 2;
  lregs[3] := 3;
  lregs[4] := 4;
  lregs[5] := 5;
  lregs[6] := 6;
  vregs[1].high := 1;
  vregs[1].low := 1;
  vregs[2].high := 2;
  vregs[2].low := 2;
  vregs[3].high := 3;
  vregs[3].low := 3;
  vregs[4].high := 4;
  vregs[4].low := 4;
  pr[1] := 1;
  pr[2] := 2;
  pr[3] := 3;
  pr[4] := 4;
  pr[5] := 5;
  pr[6] := 3;
  pr[7] := 4;
  pr[8] := 1;
  vpr[1].high := 1;
  vpr[1].low := 1;
  vpr[2].high := 2;
  vpr[2].low := 2;
  vpr[3].high := 3;
  vpr[3].low := 3;
  vpr[4].high := 4;
  vpr[4].low := 4;
  vpr[5].high := 5;
  vpr[5].low := 5;
  vpr[6].high := 6;
  vpr[6].low := 6;
  vpr[7].high := 7;
  vpr[7].low := 7;
  vpr[8].high := 8;
  vpr[8].low := 8;
  mem[1] := 1;
  mem[2] := 2;
  mem[3] := 3;
  mem[4] := 4;
  mem[5] := 5;
  mem[6] := 6;
  mem[7] := 7;
  mem[8] := 8;
  mem[9] := 9;
  mem[10] := 1;
  cregs[1] := 1;
  cregs[2] := 2;
  cregs[3] := 3;
  cregs[4] := 4;
  cregs[5] := 5;
  cregs[6] := 6;
  cvecregs[1].high := 1;
  cvecregs[1].low := 1;
  cvecregs[2].high := 2;
  cvecregs[2].low := 2;
  cvecregs[3].high := 3;
  cvecregs[3].low := 3;
  cvecregs[4].high := 4;
  cvecregs[4].low := 4;
  cmem[1] := 1;
  cmem[2] := 2;
  cmem[3] := 3;
  cmem[4] := 4;
  cmem[5] := 5;
  cmem[6] := 6;
  cmem[7] := 7;
  cmem[8] := 8;
  cmem[9] := 9;
  cmem[10] := 1;
  latest_vec_mrn_id[1] := 3;
  curr_mrn_id := 1;
  curr_vmrn_id := 5;
  mrn_map[1].lpc := 2;
  mrn_map[1].spc := 1;
  mrn_map[2].lpc := 5;
  mrn_map[2].spc := 4;
  mrn_reg_color_map_vec[3] := 2;
  mrn_reg_color_map_vec[4] := 1;
  numstrands := 1;
  freelist[1] := true;
  freelist[2] := true;
  freelist[3] := true;
  freelist[4] := true;
  freelist[5] := true;
  freelist[6] := false;
  freelist[7] := false;
  freelist[8] := false;
  vfreelist[1] := true;
  vfreelist[2] := true;
  vfreelist[3] := true;
  vfreelist[4] := true;
  vfreelist[5] := true;
  vfreelist[6] := true;
  vfreelist[7] := true;
  vfreelist[8] := true;
  rat[1] := 8;
  rat[3] := 8;
  rat[4] := 8;
  rrat[6] := 3;
  rrat[7] := 4;
  rrat[8] := 1;
  pr_ready[1] := true;
  pr_ready[2] := true;
  pr_ready[3] := true;
  pr_ready[4] := true;
  pr_ready[5] := true;
  pr_ready[6] := true;
  pr_ready[7] := true;
  pr_ready[8] := true;
  vpr_ready[1] := true;
  vpr_ready[2] := true;
  vpr_ready[3] := true;
  vpr_ready[4] := true;
  vpr_ready[5] := true;
  vpr_ready[6] := true;
  vpr_ready[7] := true;
  vpr_ready[8] := true;
  vpr_consumers[8] := 2;
  curr_lbid := 1;
  curr_sbid := 3;
  rht[1].prev_sbid := 2;
  rht[2].prev_sbid := 3;
  rob[2].uopid := 2;
  rob[2].status := done;
  rob_tail := 3;
  rob_head := 2;
  scb_id := 1;
end;
*/

/* Rules * /
 
/*  
* MRN rules in FE.  *
* Not  modeling the predictor. Each
* uop will have an associated PC,  the program location resulting in the
* uop. There is also a uop_id the sequence number for the uop.
* 
* FE will maintain a store-load MRN pairs using PC of the stores and
* loads.  We can add and remove pairs dynamically to make things
* interesting.  Every store and load that comes into FE is assigned a
* MRN color + reg. In this model we will arbitrarily set the status to probe or valid.
* Each PC location can have multiple uops associated with it. So we will keep the last store/load uop
* associated with it. In case a load is already assigned a color+reg, a store with matching PC comes in
* it gets the same color + reg. (Check with  Rafael).
* 
* 
*/


ruleset src1: LogicalRegs do
ruleset src2: LogicalRegs do
ruleset pc: PC do
Alias
  uop : inflight[uop_count]; /* inflight holds all uops in flight. Is modified with additional info. */
  uop_m: idq[uop_count]; /* idq stores all the uop entering into the system. This is a golden copy which we don't touch */
do
  rule "Stage 0/DQ: Insert Add uop"
  stage = DQ & numstrands <= MaxStrands & is_add_pc(pc) & false
  & is_non_mrn_reg(src1) & is_non_mrn_reg(src2)
  ==>
  begin
    uop.uoptype := Add;
    uop.src1 := src1;
    uop.src2 := src2;
    uop.uopid := uop_count;
    uop.uopstage := FE; /* put it into the next stage */
    uop.line_id := numlines;
    uop.strand_id := numstrands;
    uop.pc := pc;
    uop_m := uop; /* idq also gets the same uop*/
    increment_uop_count();
    increment_stage_line_strand_nums();

num_add_uops := num_add_uops + 1;
  end;
end;
end;
end;
end;


ruleset dst: LogicalRegs do
ruleset mem: MemLocs do
ruleset pc: PC do
Alias
  uop : inflight[uop_count]; 
  uop_m: idq[uop_count]; 
do
  rule "Stage 0/DQ: Insert Load_int uop"
  stage = DQ & numstrands <= MaxStrands & is_load_pc(pc) & false
  & is_non_mrn_reg(dst) 
  ==>
  begin
    uop.uoptype := Load_int;
    uop.dst := dst; /* dst of the load */
    uop.mem_addr := mem;
    uop.mem_width := 1; /* int load. */
    uop.uopid := uop_count;
    uop.uopstage := FE; /* put it into the next stage */
    uop.line_id := numlines;
    uop.strand_id := numstrands;
    uop.pc := pc;
    uop_m := uop; /* idq also gets the same uop*/
    increment_uop_count(); 
    increment_stage_line_strand_nums();

num_load_int_uops := num_load_int_uops + 1;
  end;
end;
end;
end;
end;

ruleset vdst: VecLogicalRegs do
ruleset mem: MemLocs do
ruleset w: 1..2 do
ruleset pc: PC do
Alias
  uop : inflight[uop_count]; 
  uop_m: idq[uop_count];
do
  rule "Stage 0/DQ: Insert Load_vec uop"
  stage = DQ & numstrands <= MaxStrands & is_load_pc(pc)
  & is_non_vec_mrn_reg(vdst) 
  ==>
  begin
    uop.uoptype := Load_vec;
    uop.vdst := vdst; /* dst of the load */
    uop.mem_addr := mem;
    uop.mem_width := w; /* choose val from 1..2 */
    uop.uopid := uop_count;
    uop.uopstage := FE; /* put it into the next stage */
    uop.line_id := numlines;
    uop.strand_id := numstrands;
    uop.pc := pc;
    uop_m := uop; /* idq also gets the same uop*/
    increment_uop_count();  
    increment_stage_line_strand_nums();

num_load_vec_uops := num_load_vec_uops + 1;
  end;
end;
end;
end;
end;
end;

ruleset src1: LogicalRegs do
ruleset mem: MemLocs do
ruleset pc: PC do
Alias
  uop : inflight[uop_count]; 
  uop_m: idq[uop_count]; 
do
  rule "Stage 0/DQ: Insert Store_int uop"
  stage = DQ & numstrands <= MaxStrands & is_store_pc(pc) & false
  & is_non_mrn_reg(src1)
  ==>
  begin
    uop.uoptype := Store_int;
    uop.src1 := src1; /* source of the load */
    uop.mem_addr := mem;
    uop.mem_width := 1; /* int load. */
    uop.uopid := uop_count;
    uop.uopstage := FE; /* put it into the next stage */
    uop.line_id := numlines;
    uop.strand_id := numstrands;
    uop.pc := pc;
    uop_m := uop; /* idq also gets the same uop*/
    increment_uop_count();  
    increment_stage_line_strand_nums();

num_store_int_uops := num_store_int_uops + 1;
  end;
end;
end;
end;
end;

ruleset vsrc1: VecLogicalRegs do
ruleset mem: MemLocs do
ruleset pc: PC do
ruleset w: 1..2 do
Alias
  uop : inflight[uop_count]; 
  uop_m: idq[uop_count]; 
do
  rule "Stage 0/DQ: Insert Store_vec uop"
  stage = DQ & numstrands <= MaxStrands & is_store_pc(pc) & false
  & is_non_vec_mrn_reg(vsrc1)
  ==>
  begin
    uop.uoptype := Store_vec;
    uop.vsrc1 := vsrc1; /* src of the load */
    uop.mem_addr := mem;
    uop.mem_width := w; /* vec load. */
    uop.uopid := uop_count;
    uop.uopstage := FE; /* put it into the next stage */
    uop.line_id := numlines;
    uop.strand_id := numstrands;
    uop.pc := pc;
    uop_m := uop; /* idq also gets the same uop*/
    increment_uop_count(); 
    increment_stage_line_strand_nums();

num_store_vec_uops := num_store_vec_uops + 1;
  end;
end;
end;
end;
end;
end;




rule "Stage 1: FE"
  stage = FE
  ==>
  var mrn_id: MRN_ID;
  var vmrn_id: VMRN_ID;
  begin
	/* walk thru all the uops and process those in FE stage */
  for i : UopID do
    if (i < uop_count & !isundefined(inflight[i].uoptype)) then
      alias 
        uop : inflight[i]
      do
        if (uop.uopstage = FE & (uop.uoptype = Store_int)) then
          if (exists_in_mrn_map(uop.pc)) then
            uop.mrn_valid := true;
            mrn_id := get_next_mrn_id();
            uop.mrn_reg := reg_of(mrn_id);
            uop.mrn_color := color_of(mrn_id);
            /* We store the latest mrn_id associated with a store pc location */
            latest_mrn_id[uop.pc] := mrn_id;
            /* add to mrn_reg_color_map too */
            mrn_reg_color_map[uop.mrn_reg] := uop.mrn_color;
          end;
        end;
        if (uop.uopstage = FE & (uop.uoptype = Store_vec)) then
          
          if (exists_in_vec_mrn_map(uop.pc)) then
           
            uop.mrn_valid := true;
            vmrn_id := get_next_vmrn_id();
           
            uop.vmrn_reg := vec_reg_of(vmrn_id);
            
            
            uop.mrn_color := vec_color_of(vmrn_id); /* colors are the same in both vec and non-vec */
            /* We store the latest mrn_id associated with a store pc location */
           
           
            latest_vec_mrn_id[uop.pc] := vmrn_id;
            /* add to mrn_reg_color_map too */
            mrn_reg_color_map_vec[uop.vmrn_reg] := uop.mrn_color;
           
          end;
        end;

        if (uop.uopstage = FE & (uop.uoptype = Load_int)) then
        /* st_pcs := get_matching_store_pcs(uop.pc); 
            mrn_ids := get_mrn_ids (st_pcs);
            mrn_id  := get_latest_mrn_id(mrn_ids);*/
          if (exists_in_mrn_map(uop.pc)) then
            /* The following assumes that if a load is in mrn_map then it is preceded in uop list by a matching store. */ 
            /* Todo: Check with Rafael. 
              Checked and it doesn't have to be preceded by a store uop
            */
            mrn_id := get_latest_matching_mrn_id(uop.pc);
            uop.mrn_valid := true;
            uop.mrn_reg := reg_of(mrn_id);
            uop.mrn_color := color_of(mrn_id);
          else
            mrn_id := get_next_mrn_id();
            uop.mrn_valid := true;
            uop.mrn_reg := reg_of(mrn_id);
            uop.mrn_color := color_of(mrn_id);
          end;
        end;

        if (uop.uopstage = FE & (uop.uoptype = Load_vec)) then
         
        /* st_pcs := get_matching_store_pcs(uop.pc); 
            mrn_ids := get_mrn_ids (st_pcs);
            mrn_id  := get_latest_mrn_id(mrn_ids);*/
          if (exists_in_vec_mrn_map(uop.pc)) then
            /* The following assumes that if a load is in mrn_map then it is preceded in uop list by a matching store. */ 
            /* Todo: Check with Rafael. 
              Checked and it doesn't have to be preceded by a store uop
            */
            
           
            vmrn_id := get_latest_matching_vmrn_id(uop.pc);
            uop.mrn_valid := true;
            uop.vmrn_reg := vec_reg_of(vmrn_id);
            uop.mrn_color := vec_color_of(vmrn_id);
          else
            
            vmrn_id := get_next_vmrn_id();
            uop.mrn_valid := true;
            uop.vmrn_reg := vec_reg_of(vmrn_id);
            uop.mrn_color := vec_color_of(vmrn_id);
          end;
          
        end;
        if (uop.uopstage = FE) then uop.uopstage := RA1; end;
      end;
    end;
  end;
  stage := RA1;
end;



rule "Stage 2: Rename and Alloc"  
  stage = RA1
==>
var c1: PhysicalRegs;
var c2: PhysicalRegs;
var ruop: Ruop;
var uop: Uop;
begin
  
  /* do load checks in RA1 itself */
  for i: UopID do
   
    if (i < uop_count & !isundefined(inflight[i].uoptype)) then
      uop := inflight[i];
      if (uop.uopstage = RA1) then
        switch uop.uoptype
          case Load_int:
          /* Bug1: added extra disjunction true */
            if(mrn_check(uop) | true) then
              uop.uoptype := Load_int_mrn; /* uoptype indicates mrn can proceed */
            end;
          case Load_vec:
            if(mrn_check_vec(uop)) then
              uop.uoptype := Load_vec_mrn; /* uoptype indicates mrn can proceed */
            end;  
        end;
      end;
    end;
  end;
 
  for i: UopID do
   
    if (i < uop_count & !isundefined(inflight[i].uoptype)) then
      uop := inflight[i];
    /* Renames logical regs. 
       Updates the rat/rrat
       Updates freelist too.*/
      if uop.uopstage = RA1 then
        if (is_int_uop(uop.uoptype)) then
          ruop := convert_to_ruop(uop);
         
          if !isundefined(ruop.consumed1) then
            pr_consumers[ruop.consumed1] := uop.uopid;
          end;
          if !isundefined(ruop.consumed2) then
            pr_consumers[ruop.consumed2] := uop.uopid;
          end;
        else
          ruop := convert_to_ruop_vec(uop);
         
          if !isundefined(ruop.vconsumed1) then
            vpr_consumers[ruop.vconsumed1] := uop.uopid;
          end;
          if !isundefined(ruop.vconsumed2) then
            vpr_consumers[ruop.vconsumed2] := uop.uopid;
          end;
        end;
        /* allocate rob entry. */
        /* Bug 2: should be rob_tail. Added extra -1 */
        ruop.robid := rob_tail - 1;
        undefine rob[rob_tail].status;
        /*rob[rob_tail].done := false;
        rob[rob_tail].nuke := false;*/

        /* back pointer from rob to uop */
        rob[rob_tail].uopid := ruop.uopid;
        if rob_tail = NumROBEntries then
          rob_tail := 1;
        else rob_tail := rob_tail + 1; /* Todo: not wrapping around currently */
        end;
        /* allocate lbid, sbid as appropriate */
        if (uop.uoptype = Load_int | uop.uoptype = Load_vec) then
          ruop.lbid := curr_lbid;
          curr_lbid := next_lbid(curr_lbid); 
          ruop.prec_sbid := curr_sbid; /* all sbid strictly before prec_sbid are considered preceding */
          rht[uop.uopid].prev_lbid := curr_lbid;
          rht[uop.uopid].maps := ruop.rht.maps;
          rht[uop.uopid].vmaps := ruop.rht.vmaps;
        end;
        if (uop.uoptype = Store_int | uop.uoptype = Store_vec) then
          ruop.sbid := curr_sbid;
          curr_sbid := next_sbid(curr_sbid);
          ruop.prec_lbid := curr_lbid; /* all lbid strictly before prec_lbid are considered preceding */
          rht[uop.uopid].prev_sbid := curr_sbid;
          rht[uop.uopid].maps := ruop.rht.maps;
          rht[uop.uopid].vmaps := ruop.rht.vmaps;
        end;
        ruop.uopstage := RA2;
        inflight_r[i] := ruop;
        /*undefine inflight[i];*/
      end;
    end;
  end; 
  stage := RA2;
end;

/*
RA2: Move eliminate and put uops into Mem1
Initial reading: Stores are probably not move eliminated as the MRN reg has to live
longer and cannot be treated like a reg register. That is, Store rax
MRN1 we cannot make rat(MRN1) point to rat(rax) because prf for
MRN1 has to live longer.
Load rax MRN1 on the other hand can be move eliminated by having
rat(rax) point to rat(MRN1) because the latter entry is to going to live
till load checks pass in Mem cluster.
Correct reading: both MRN'ed loads and stores are move eliminated. Because MRN regs are never
allocated physical regs.
*/
rule "Stage 3: RA2"  
  stage = RA2
==>
var c1: PhysicalRegs;
var c2: PhysicalRegs;
begin
  for i: UopID do
    /*put "Stage 3:Outside of if-stmt and i is: "; put i; put "\n";*/
    if(i < uop_count & !isundefined(inflight[i].uoptype)) then
      /*put "Inside of if-stmt and i is: "; put i; put "\n";*/
      alias
        ruop : inflight_r[i];
        uop :  inflight[i]
      do
        if ruop.uopstage = RA2 then
          switch ruop.uoptype 
            case Store_int_mrn:
              /* store src1 mem-addr --> store src1 dst1 where dst1 is mrn_reg */
              rat[uop.mrn_reg] := rat[uop.src1];
              assert(!freelist[uop.src1]);
            case Store_vec_mrn:
              rat[uop.vmrn_reg] := rat[uop.vsrc1];
              assert(!freelist[uop.vsrc1]);
            case Load_int_mrn:
              /* load dst mem-addr --> load dst1 src1 where src1 is mrn_reg */
              rat[uop.dst] := rat[uop.mrn_reg];
              assert(!freelist[uop.mrn_reg]); /* This should follow from prev 2 asserts.*/
            case Load_vec_mrn:
              rat[uop.vdst] := rat[uop.vmrn_reg];
              assert(!freelist[uop.vmrn_reg]);
          end;
          /* Rest of the instructions pass through to Mem1 directly */
          ruop.uopstage := Mem1;
        end;
      end;    
      undefine inflight[i];
    end;
  end;
  stage := Mem1;
end;


/* MRN rules in Mem */

/* Mem1: For load wait till the address is ready then got to Mem2
For store: wait till the address is ready and all the previous loads and stores 
have got addresses as well. Why loads?
*/

rule "Stage 4: Mem1"  
  stage = Mem1
==>
var c1: PhysicalRegs;
var c2: PhysicalRegs;
var ruop: Ruop;
begin
  for i: UopID do
    if (i < uop_count ) then
      alias
        ruop : inflight_r[i]
      do
        if ruop.uopstage = Mem1 then
          switch ruop.uoptype
            /* We are assuming LB is of unbounded size and imposes no constraint on a ruop moving to Mem2 */
            case Load_int_mrn:
              if (true) then
                  ruop.uopstage := Mem2;
              end;
            case Load_int:
              if (true) then
                  ruop.uopstage := Mem2;
              end;
            case Load_vec_mrn:
              if (true) then
                  ruop.uopstage := Mem2;
              end;
            case Load_vec:
              if (true) then
                  ruop.uopstage := Mem2;
              end;
            /* We are assuming SB is of unbounded size and imposes no constraint on a ruop moving to Mem2 */
            case Store_int_mrn:
              if (operands_ready(ruop)) then
                if (all_prec_loads_stores_cleared(ruop)) then
                  ruop.uopstage := Mem2;
                end;
              end;
            case Store_vec_mrn:
              if (operands_ready(ruop)) then
                if (all_prec_loads_stores_cleared(ruop)) then
                  ruop.uopstage := Mem2;
                end;
              end;
            case Add:
              if (operands_ready(ruop)) then
                  ruop.uopstage := Mem2;
              end;
          end;
        end;
      end;
    end;
  end;
  stage := Mem2;
end;


rule "Stage 5: Mem2"  
  stage = Mem2
==>
var c1: PhysicalRegs;
var c2: PhysicalRegs;
var s1: Slice;
var s2: Slice;
begin
  for i: UopID do
    if(i < uop_count) then
      alias
        ruop : inflight_r[i]
      do
        if ruop.uopstage = Mem2 then
          switch ruop.uoptype
            /* We are assuming LB is of unbounded size and imposes no constraint on a ruop moving to Mem2 */
            case Load_int_mrn:
              if(mrn_load_check(ruop)) then 
                rob[ruop.robid].status := done;
                in_bypass[ruop.dst] := true;
              else 
                rob[ruop.robid].status := nuke;
              end;
              ruop.uopstage := RB;
            case Load_int:
              /* This can take variable number of cycles but currently single cycle */
              pr[ruop.dst] := mem[ruop.mem_addr];
              /* bypass_cache[id] := mem[ruop.mem_addr];*/
              in_bypass[ruop.dst] := true;
              
              /* TODO: Need to perform MoClear check here before going to ROB
                Make sure that none of the preceding stores write to the same address.
                Stores get address in Mem2. So check Mem2 and SCB for stores with sbid lower than
                prec_sbid of uop and check that none of them have the same address.
              */
              if mo_clear(ruop) then
                ruop.uopstage := RB;
                rob[ruop.robid].status := nuke;
              else
                rob[ruop.robid].status := done;
                ruop.uopstage := RB;
              end;
            case Load_vec_mrn:
              if(mrn_load_check_vec(ruop)) then 
                rob[ruop.robid].status := done;
                in_bypass_vec[ruop.vdst] := true;
              else 
                rob[ruop.robid].status := nuke;
              end;
              ruop.uopstage := RB;
            case Load_vec:
              /* This can take variable number of cycles but currently single cycle */
              if (ruop.mem_width = 1) then
                vpr[ruop.vdst].low := mem[ruop.mem_addr];
                vpr[ruop.vdst].high:= 0; /* zero-ing out high bits */
              else
                vpr[ruop.vdst].high := mem[ruop.mem_addr];
                vpr[ruop.vdst].low := mem[next_mem_addr(ruop.mem_addr)];
                /*mem[ruop.mem_addr + 1];*/
              end;
              in_bypass_vec[ruop.vdst] := true;
              if mo_clear(ruop) then
                ruop.uopstage := RB;
                rob[ruop.robid].status := nuke;
              else
                rob[ruop.robid].status := done;
                ruop.uopstage := RB;
              end;
            /*rob[ruop.robid].status := done;
            ruop.uopstage := RB;*/
            case Store_int_mrn:
              /* store src1 mem_addr --> store src1 mrn_reg */
              if(!isundefined(ruop.retired) & ruop.retired) then
                /* enter into TSB and SCB. */
                s1 := get_slice(ruop.mem_addr);
                tsb[s1][ruop.mrn_reg] := ruop.mem_addr;
                /* TODO: using ruop here but undefining few lines below. 
                We want scb to continue holding the correct value of ruop. Is that a problem? */
                scb[scb_id] := ruop;
                scb_id := next_scbid(scb_id);
                /* Mem should be updated only after the store retires.
                  After store retires it goes from SB to SCB and TSB. From SCB it gets 
                  written to Mem. That can be non-deterministic but in this model it happens
                  immediately upon entering SCB. Ruop is retained in SCB for Mo_clear checks.
                  At the beginning of every strand we should clear out SCB.
                */
                mem[ruop.mem_addr] := pr[ruop.src1]; 
                undefine ruop;
              else
                rob[ruop.robid].status := done;
                ruop.uopstage := RB;
              end;
            case Store_vec_mrn:
              if (!isundefined(ruop.retired) & ruop.retired) then
                s1 := get_slice(ruop.mem_addr);  
                s2 := get_slice(next_mem_addr(ruop.mem_addr));
                if !isundefined(ruop.mem_width) & ruop.mem_width = 2 then
                  vtsb[s1][ruop.vmrn_reg] := ruop.mem_addr;
                  vtsb[s2][ruop.vmrn_reg] := next_mem_addr(ruop.mem_addr);
                  mem[ruop.mem_addr] := vpr[ruop.vsrc1].high;
                  mem[next_mem_addr(ruop.mem_addr)] := vpr[ruop.vsrc1].low;
                  /* instrumentation for lighthouses */
                  if(s1 != s2) then
                    num_split_stores := num_split_stores + 1;
                  end;
                else
                  vtsb[s1][ruop.vmrn_reg] := ruop.mem_addr;
                  /* low bits get written into mem */
                  mem[ruop.mem_addr] := vpr[ruop.vsrc1].low;
                end;
                scb[scb_id] := ruop;
                scb_id := next_scbid(scb_id);
                undefine ruop;
              else
                rob[ruop.robid].status := done;
                ruop.uopstage := RB;
              end;
            case Add:
              pr[ruop.dst] := (pr[ruop.src1] + pr[ruop.src2]) % MaxRegVal;
              in_bypass[ruop.dst] := true;
              rob[ruop.robid].status := done;
              ruop.uopstage := RB;
          end;
        end;
      end;
    end;
  end;
  stage := RB;
end;

rule "Stage 6: RB" 
  stage = RB
==>
var head: ROBID;
var 
begin
  head := rob_head;
  /* Likely bug here. Fix after fixing the other one.
     Fixed the first bug -- turned out to be two related bugs, one of which lead to several
     similar errors.
     Fixed the bug here by adding head := rob_head.
  */
  while (!isundefined(rob[head].status)) do
    alias 
      ruop: inflight_r[rob[head].uopid] 
    do
    /* MoClear also will lead to nuke */
      if rob[head].status = nuke then
         recover_rat(ruop);
        /* clear out ROB and reset rob_head/rob_tail */
        for i : ROBID do
          undefine rob[i];
        end;
        /* undefine inflight_r from rob[head].uopid downwards */
        for i: UopID do
          if (i >= ruop.uopid) then 
            undefine inflight_r[i];
          end;
        end;
        rob_head := 0;
        rob_tail := 0;
        /* clear out in_bypass network */
        undefine in_bypass;
        undefine in_bypass_vec;
      else
        if (!(ruop.uoptype = Store_vec | ruop.uoptype = Store_vec_mrn 
              | ruop.uoptype = Load_vec | ruop.uoptype = Load_vec_mrn)) then
          alias 
            p: ruop.produced
          do
            if (!isundefined(p)) then pr_ready[p] := true; end;           
          end;
        else
          alias
            vp: ruop.vproduced
          do
            if(!isundefined(vp)) then vpr_ready[vp] := true; end;
          end;
        end;
        
        undefine rob[head];
        increment_rob_head();
         head := rob_head;
        /* checker stuff */
        /* replay uop in in-order machine */
        update_in_order_mc(ruop);
        /* update the retirement rat. This is used in the model for checking consistency
        *  with the in-order machine.
        */
        for m: 1..NumMaps do
          apply_to_retire_rat(ruop.rht.maps[m]);
        end;
        for m: 1..NumMaps do
          apply_to_retire_rat_vec(ruop.rht.vmaps[m]);
        end;
        /* free the inflight_r entry. Necessary?? */
        if (!is_store(ruop.uoptype)) then
          undefine ruop;
        else
          ruop.retired := true;
        end;
      end;
    end;
  end;
  stage := DQ;
end;


invariant "OOO consistent with IO-1"
forall i: 1..NumLogicalRegs do
   (stage = DQ) & (isundefined(retire_rat[i]) |
     (pr[rat[i]] = cregs[i]))
end;



invariant "OOO consistent with IO-2"
forall i: 1..NumVecLogicalRegs do
   (stage = DQ) & (isundefined(retire_rat_vec[i]) |
     (vpr[vrat[i]].high = cvecregs[i].high))
end;

/*
lighthouse "Trial-1"
stage = FE;
*/

/*
lighthouse "Trial-2"
stage = RA1;
lighthouse "Uop_count"
uop_count = 6;
lighthouse "numlines"
numlines = 4;
lighthouse "numvecstores"
numvecstores >= 1
lighthouse "numvecloads"
numvecloads >= 1
*/


-- Assume "No add_uops or load_* or store_int_uops"
-- num_add_uops = 0 & num_load_int_uops = 0 &  num_store_int_uops = 0;

-- lighthouse "numsplitstores"
-- num_split_stores > 0;

/* Trying out Assumes 
Assume "Don't go beyond RA1"
stage != RA2;*/

/* One store_vec instruction completes with tsb[0][mrn_reg] = a, tsb[1][mrn_reg] = a + 1
invariant "lighthouse 1"
forall a : MemLocs do
forall m: VecLogicalRegs do
(!isundefined(vtsb[1][m]) & !isundefined(vtsb[2][m]))
->
!(vtsb[1][m] = a & vtsb[2][m] = next_mem_addr(a))
end
end;
*/
/*
invariant "Lighthouse-start"
stage != Mem1; */
/*
invariant "Lighthouse-1"
stage != RA2 | numlines != 2;
*/

/* Gap invariant: currently embedded in mrn_load_check/vec_mrn_load_check
  Description: If load check in FE fails then uopid of the load and the
  uopid of the corresponding youngest store with matching PC value should
  differ by atleast MRN_REG * MRN_Colors. That is, we have circled through
  that many values of MRN_ID since the store till the load.
*/

/* 
Usage invariant: If a uop in SCB is using a phy reg pr then pr must not
be in the freelist in RA1
*/

/*
invariant "Usage invariant"
forall s: SCBID do
  (!isundefined(scb[s].src1) | !isundefined(scb[s].vsrc1))
  &   
  (!isundefined(scb[s].src1) -> !freelist[scb[s].src1])
  &
  (!isundefined(scb[s].vsrc1) -> !freelist[scb[s].vsrc1])
end;
*/

/* If a uop is in pipestage Mem1 or 2 then its rat entries for its logical
regs must be non-empty. That is, if a uop is in Mem then it must have
passed through RA1. Effects of RA1 are RAT table being set.
*/

/*
invariant "Ordering"
forall u: UopID do
  (!isundefined(inflight_r[u].uoptype)) ->
  (
    (inflight_r[u].uopstage = Mem1 | inflight_r[u].uopstage = Mem1) ->
    (
      (!isundefined(inflight[u].src1) -> !isundefined(rat[inflight[u].src1]))
      &
      (!isundefined(inflight[u].src2) -> !isundefined(rat[inflight[u].src2]))
    )
  )
end;
*/

/* No excess RAT entries invariant:
  If a logical reg is in RAT then some uop in inflight_r must be
  using it. Likely, this will be tough to implement in the RTL? */

 /*Added later: No true. A uop can retire and the pr it wrote to along with the mapping 
 from logical reg to phy reg can be alive in the RAT.
 
  */
/*
invariant "No excess RAT entries"
forall l: LogicicalRegs do
!isundefined(rat[l]) -> 
          (exists i. isundefined(inflight_r[i].uoptype) & (inflight_r[i].src1 = l | inflight_r[i].src2 = l))
end;
*/

/*
  Ordering between RA stages
  If a uop is in RA2 then it must have passed through RA1.
  So if a PR is being used in Move Elimination then it must not be
  free in freelist.
  Embedded invariant in RA2 rule.
*/
