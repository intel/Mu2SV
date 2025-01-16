Const
	VAL_LIM: 1000;

Type
	val_t: 0..VAL_LIM;

Var
	v: val_t;


Rule "incBy1"
	v <= VAL_LIM - 1
==>
Begin
	v := v + 1;
End;

Rule "incBy2"
	v <= 50
==>
Begin
	v := v + 2;
End;

Startstate
Begin
	v := 1;
End;

Invariant "test"
	v <= 100;
