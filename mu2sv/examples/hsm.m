-- murphi model for hierarchical state machine design pattern
-- example for pelican traffic light
--  reference: https://barrgroup.com/Embedded-Systems/How-To/Introduction-Hierarchical-State-Machines

-- can set BUG* to true to trigger various bugs
-- in jaspergold, 
--   set elab_opts { -parameter hsm_pkg.mu_bug_coding_omission_cov 1 }
--   source hsm.tcl
--   prove -bg -task {<embedded>}

const
   NUM_FLASH_CNT : 7 ;
   BUG_INIT : false;
   BUG_FLASH : false ;
   BUG_DEADLOCK: false ;
   BUG_CODING_OMISSION_AST: false ;
   BUG_CODING_OMISSION_COV: false ;

type
   PedSignal: enum {DONT_WALK, WALK, BLANK} ;
   VehSignal: enum {GREEN, YELLOW, RED } ;
   States : enum { operational, vehiclesEnabled, vehiclesGreen, vehiclesGreenInt, vehiclesYellow, pedestriansEnabled, pedestriansWalk, pedestriansFlash, off_state } ;
   Events : enum {timeout, pedestrian_waiting, switch_off, switch_on } ;
   FlashCntId : 0..NUM_FLASH_CNT ;

var
   pedestrian_light: PedSignal; 
   vehicles_light : VehSignal;
   state: States;
   timer_on : boolean;
   isPedestrianWaiting : boolean;
   flash_counter : FlashCntId;
   flash_count_odd : boolean ;
   
   function in_vehiclesEnabled ():boolean ;
   begin
     return state=vehiclesGreen | state=vehiclesGreenInt | state=vehiclesYellow ;
   end;

   function in_pedestriansEnabled ():boolean ;
   begin
     return state=pedestriansWalk | state=pedestriansFlash ;
   end;

   function in_operational ():boolean ;
   begin
     return in_vehiclesEnabled() | in_pedestriansEnabled();
   end;

   procedure enter_state(st: States);
   Begin

     if (st = operational) then
       state:= operational;
       enter_state (vehiclesEnabled) ;
       return;
     end;

     if (st=vehiclesEnabled) then
       state:=vehiclesEnabled;
       pedestrian_light := DONT_WALK ;
       enter_state ( vehiclesGreen) ;
       return ;
     end;

     if (st=vehiclesGreen) then
       state:=vehiclesGreen;
       timer_on := true ;
       vehicles_light := GREEN ;
       isPedestrianWaiting := false ;
       return;
     end;

     if (st=vehiclesGreenInt) then
       state:=vehiclesGreenInt;
       return;
     end;

     if (st=vehiclesYellow) then
       state:=vehiclesYellow ;
       if (!BUG_CODING_OMISSION_COV) then -- results in cover-not-hit
       vehicles_light := YELLOW ;
       End;
       timer_on := true ;
       return
     End;


     if (st=pedestriansEnabled) then
       state:=pedestriansEnabled;
       vehicles_light := RED ;
       enter_state(pedestriansWalk);
       return ;
     End;

     if (!BUG_CODING_OMISSION_AST) then -- results in assert
     if (st=pedestriansWalk) then
       state:=pedestriansWalk;
       pedestrian_light := WALK ;
       timer_on := true;
       return;
     End;
     End;

     if (st=pedestriansFlash) then
       state:=pedestriansFlash;
       timer_on := true;
       flash_counter := NUM_FLASH_CNT;
       flash_count_odd := true ;
       return;
     End;

     if (st=off_state) then
       state:=off_state;
       return ;
     End;

     error "enter_state should not reach here" ;

   end;

   procedure exit_state(st:States);
   Begin

     if (state = vehiclesGreen) then
       timer_on := false ;
     end;

     if (state = vehiclesYellow) then
       timer_on := false ;
     end;

     if (state = pedestriansWalk) then
       timer_on := false ;
     end;

     if (state = pedestriansFlash) then
       timer_on := false ;
     end;
     
   end;

   procedure process (ev: Events) ;
   Begin

     if (in_operational()) then 
       if ( ev = switch_off ) then
         exit_state(operational);
         enter_state(off_state) ;
         return ;
       end;
     end;

     if ( state = vehiclesGreen )  then
       if (ev = pedestrian_waiting) then
         isPedestrianWaiting:=true;
       end;

       if (ev = timeout ) then
         if ( isPedestrianWaiting ) then
          exit_state(vehiclesGreen);
          enter_state( vehiclesYellow );
         else
          exit_state(vehiclesGreen);
          enter_state(vehiclesGreenInt);
         end;
       end;
       return ;
     end;
      
     if ( state = vehiclesGreenInt) then
       if (ev = pedestrian_waiting) then
          exit_state(vehiclesGreenInt);
         enter_state ( vehiclesYellow) ;
       end;
     End;

      
     if ( state = vehiclesYellow) then
       if (ev = timeout) then
         exit_state(vehiclesYellow);
         enter_state ( pedestriansEnabled ) ;
       end;
       return ;
     End;
      
     if ( state = pedestriansWalk) then
       if (ev = timeout) then
         exit_state(pedestriansWalk);
         enter_state ( pedestriansFlash ) ;
       end;
       return ;
     End;
      
     if ( state = pedestriansFlash) then
       if (ev = timeout) then
         if (flash_counter = 0 )  then
           exit_state(pedestriansFlash);
           enter_state ( vehiclesEnabled ) ;
         else
           if (!BUG_FLASH) then
             flash_counter := flash_counter -1 ;
             flash_count_odd := !flash_count_odd ;
           end;
           --if ( pedestrian_light = DONT_WALK )  then
           if ( flash_count_odd )  then
             pedestrian_light := BLANK ;
            else
             pedestrian_light := DONT_WALK ;
            end;
         end;
       end;
       return;
     End;
   End;
  
  startstate
   if (!BUG_INIT) then
     flash_counter := 0 ;
   end;
    enter_state ( operational );
  end;

   rule "EvTimeout"
    timer_on ==> process (timeout) ;
   end;

   rule "EvSwitchOff" 
     in_operational() ==> process(switch_off);
   end;
   
   rule "EvPedestrian" 
     in_operational() ==> process(pedestrian_waiting);
   end;

   rule "EvSwitchOn"
     !BUG_DEADLOCK & state=off_state ==> enter_state( operational );
   end;


invariant "savePedestrians"
  !((pedestrian_light=WALK | pedestrian_light=BLANK) & (vehicles_light != RED)) ;

