--
--  German's Protocol
--
--  A caching protocol proposed by Steven German of IBM as a challenge for 
--  parameterized verification.
--
const  ---- Configuration parameters ----

  NODE_NUM : 4;
  DATA_NUM : 2;

type   ---- Type declarations ----

  NODE : 1..NODE_NUM;
  DATA : 1..DATA_NUM;

  CACHE_STATE : enum {Invalid, Shared, Exclusive};
  CACHE : record State : CACHE_STATE; Data : DATA; end;

  MSG_CMD : enum {Empty, ReqS, ReqE, Inv, InvAck, GntS, GntE};
  MSG : record Cmd : MSG_CMD; Data : DATA; end;

var   ---- State variables ----

  Cache : array [NODE] of CACHE;      -- Caches
  Chan1 : array [NODE] of MSG;        -- Channels for Req*
  Chan2 : array [NODE] of MSG;        -- Channels for Gnt* and Inv
  Chan3 : array [NODE] of MSG;        -- Channels for InvAck
  InvSet : array [NODE] of boolean;   -- Set of nodes to be invalidated
  ShrSet : array [NODE] of boolean;   -- Set of nodes having Shared or Exclusive copies
  ExGntd : boolean;                   -- Exclusive copy has been granted
  CurCmd : MSG_CMD;                   -- Current request command
  CurPtr : NODE;                      -- Current request node
  MemData : DATA;                     -- Memory data
  AuxData : DATA;                     -- Auxiliary variable for latest data

---- Initial states ----

ruleset d : DATA do
  startstate "Init"
    for i : NODE do
      Chan1[i].Cmd := Empty;
      Chan2[i].Cmd := Empty;
      Chan3[i].Cmd := Empty;
      Cache[i].State := Invalid;
      InvSet[i] := false;
      ShrSet[i] := false;
    end;
    ExGntd := false;
    CurCmd := Empty;
    MemData := d;
    AuxData := d;
  end;

  startstate "Init2"
    for i : NODE do
      Chan1[i].Cmd := Empty;
      Chan2[i].Cmd := Empty;
      Chan3[i].Cmd := Empty;
      Cache[i].State := Invalid;
      InvSet[i] := false;
      ShrSet[i] := false;
    end;
    ExGntd := false;
    CurCmd := Empty;
    MemData := d;
    AuxData := d;
  end;
end;

---- State transitions ----

ruleset i : NODE; d : DATA do
  rule "Store"
    Cache[i].State = Exclusive
  ==>
    Cache[i].Data := d;
    AuxData := d;
  end;

  rule "SendReqS"
    Chan1[i].Cmd = Empty &
    Cache[i].State = Invalid
  ==>
    Chan1[i].Cmd := ReqS;
  end;

  rule "SendReqE"
    Chan1[i].Cmd = Empty &
    (Cache[i].State = Invalid | Cache[i].State = Shared)
  ==>
    Chan1[i].Cmd := ReqE;
  end;

  rule "RecvReqS"
    CurCmd = Empty &
    Chan1[i].Cmd = ReqS
  ==>
    CurCmd := ReqS;
    CurPtr := i;
    Chan1[i].Cmd := Empty;
    for j : NODE do
      InvSet[j] := ShrSet[j]
    end;
    -- InvSet := ShrSet;
  end;

  rule "RecvReqE"
    CurCmd = Empty &
    Chan1[i].Cmd = ReqE
  ==>
    CurCmd := ReqE;
    CurPtr := i;
    Chan1[i].Cmd := Empty;
    for j : NODE do
      InvSet[j] := ShrSet[j];
      Chan1[j].Cmd := Empty;
    end;
  end;

  rule "SendInv"
    Chan2[i].Cmd = Empty &
    InvSet[i] = true &
    (CurCmd = ReqE | CurCmd = ReqS) &
    ExGntd = true
  ==>
    Chan2[i].Cmd := Inv;
    InvSet[i] := false;
  end;

  rule "SendInvAck"
    Chan2[i].Cmd = Inv &
    Chan3[i].Cmd = Empty
  ==>
    Chan2[i].Cmd := Empty;
    Chan3[i].Cmd := InvAck;
    if Cache[i].State = Exclusive then
      Chan3[i].Data := Cache[i].Data;
    end;
    Cache[i].State := Invalid;
    undefine Cache[i].Data;
  end;

  rule "RecvInvAck"
    Chan3[i].Cmd = InvAck &
    CurCmd != Empty
  ==>
    Chan3[i].Cmd := Empty;
    ShrSet[i] := false;
    if ExGntd = true then
      ExGntd := false;
      MemData := Chan3[i].Data;
      undefine Chan3[i].Data;
    end;
  end;

  rule "SendGntS"
    CurCmd = ReqS &
    CurPtr = i &
    Chan2[i].Cmd = Empty &
    ExGntd = false
  ==>
    Chan2[i].Cmd := GntS;
    Chan2[i].Data := MemData;
    ShrSet[i] := true;
    CurCmd := Empty;
    undefine CurPtr;
  end;

  rule "SendGntE"
    CurCmd = ReqE &
    CurPtr = i &
    Chan2[i].Cmd = Empty &
    ExGntd = false &
    forall j : NODE do
      ShrSet[j] = false
    end
  ==>
    Chan2[i].Cmd := GntE;
    Chan2[i].Data := MemData;
    ShrSet[i] := true;
    ExGntd := true;
    CurCmd := Empty;
    undefine CurPtr;
  end;

  rule "RecvGntS"
    Chan2[i].Cmd = GntS
  ==>
    Cache[i].State := Shared;
    Cache[i].Data := Chan2[i].Data;
    Chan2[i].Cmd := Empty;
    undefine Chan2[i].Data;
  end;

  rule "RecvGntE"
    Chan2[i].Cmd = GntE
  ==>
    Cache[i].State := Exclusive;
    Cache[i].Data := Chan2[i].Data;
    Chan2[i].Cmd := Empty;
    undefine Chan2[i].Data;
  end;
end;

---- Invariant properties ----

invariant "CntrlProp"
  forall i : NODE do
    forall j : NODE do
      i != j ->
      (Cache[i].State = Exclusive -> Cache[j].State = Invalid) &
      (Cache[i].State = Shared -> Cache[j].State = Invalid | Cache[j].State = Shared)
    end
  end;

invariant "DataProp"
  (ExGntd = false -> MemData = AuxData) &
  forall i : NODE do
    Cache[i].State != Invalid -> Cache[i].Data = AuxData
  end;
