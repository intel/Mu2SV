--------------------------------------------------------------------------
-- Copyright (C) 1992, 1993 by the Board of Trustees of 		  
-- Leland Stanford Junior University.					  
--									  
-- This description is provided to serve as an example of the use	  
-- of the Murphi description language and verifier, and as a benchmark	  
-- example for other verification efforts.				  
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
----------------------------------------------------------------------
----------------------------------------------------------------------
-- Filename: 	counter.m
-- Content:	Counter Benchmark
-- Result: 	violate invariant when all counter is zero
----------------------------------------------------------------------

Const
	MAX_VAL: 7;
Type
	cnt_t: 0..MAX_VAL;
Var
	ctr: cnt_t;

Rule "decr by 1"
     ctr > 1
	==>
	Begin
  ctr := ctr - 1;
End;

Startstate
  Begin
   ctr := MAX_VAL -1;
  End;

Invariant "sum never 1"
	ctr != 1;
