Const
	INITIAL_VAL: 5;
	L: 1;
	H: 6;
	MAX_VAL: (H-L+1) * INITIAL_VAL;

Type
	ind_t: L..H;
	val_t: 0..INITIAL_VAL;
	arr_t: Array[ ind_t ] Of val_t;
	sum_t: 0 .. MAX_VAL;

Var
	a: arr_t;

Procedure Decrement( Var dec : val_t );
Begin
	If dec >= 1 then
		dec := dec - 1;
	End;
End;

Function Sum(  a : arr_t ): sum_t;
Var sum : sum_t;
Begin
	sum := 0;
	For i : ind_t Do
		sum := sum + a[i];
	End;
	return sum;
End;


Ruleset i: val_t Do
	Rule "hello" i > 0 ==>
	Begin

	End;
End;

Startstate
  Begin
	For i : ind_t Do
		a[ i ] := INITIAL_VAL;
	End;
  End;

Invariant "Positive sum"
	Sum(a) > 0;

