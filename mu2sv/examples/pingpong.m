--------------------------------------------------------------------------
-- Copyright (C) 1992, 1993 by the Board of Trustees of 		  
-- Leland Stanford Junior University.					  
--									  
-- This description is provided to serve as an example of the use	  
-- of the Murphi description language and verifier, and as a benchmark	  
-- example for other verification efforts.
--	
--									  
-- License to use, copy, modify, sell and/or distribute this description  
-- and its documentation any purpose is hereby granted without royalty,   
-- subject to the following terms and conditions, provided		  
--									  
-- 1.  The above copyright notice and this permission notice must	  
-- appear in all copies of this description.				  
-- 									  
-- 2.  The Murphi group at Stanford University must be acknowledged	  
-- in any publication describing work that makes use of this example. 	  
-- 									  
-- Nobody vouches for the accuracy or usefulness of this description	  
-- for any purpose.							  
--------------------------------------------------------------------------

/* pingpong.m
 * Ralph Melton, 8/30/92.
 * Done to test Murphi and
 * because it's an important toy problem that we don't already have.
 */

const x: 0;

Type player_t : 0..1;
rec_t:  Record
			       hasball, gotball: boolean
			     End;
Var Players : Array[ player_t ] of rec_t;

Ruleset p : player_t Do
    Rule "Get ball"
      Players[p].gotball
    ==>
    Begin
      Players[p].hasball := true;
      Players[p].gotball := false;
    End;

    Rule "Keep ball"
      Players[p].hasball
    ==>
    Begin
    End;

    Rule "Pass ball"
      Players[p].hasball
    ==>
    begin
      Players[p].hasball := false;
      Players[ 1 - p ].gotball := true;
    End;

    Startstate
    Begin
      Players[p].hasball := true;
      Players[p].gotball := false;
      clear Players[ 1 - p ];
    end;
End;



Invariant "Only one ball in play."
  Forall p : player_t Do
    !(Players[p].hasball & Players[p].gotball) &
    (Players[p].hasball | Players[p].gotball) ->
    Forall q : player_t Do
      (Players[q].hasball | Players[q].gotball) -> p = q
    End
  End;
  