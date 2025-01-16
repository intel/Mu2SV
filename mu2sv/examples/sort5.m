
Const
	N: 5;

Type
	ind_t:	0..N-1;
	val_t:	0..N-1;
	arr_t:	Array[ ind_t ] Of val_t;
	bool_t: Boolean;

Var
	i:	ind_t;
	j:	ind_t;
	a:	arr_t;

Procedure IncrementMod( Var v : ind_t;  m : ind_t );
Begin
	If v = m then
		v := 0;
        Else
  		v := v + 1;
	End;
End;

Procedure Increment( Var v : ind_t );
Begin
	IncrementMod( v, N-1 );
End;

Procedure Swap( Var a : arr_t );
Var tmp : val_t;
Begin
	tmp := a[i];
	a[i] := a[j];
	a[j] := tmp;
	tmp := 0;
End;


Rule "i"
	true
==>
Begin
	IncrementMod( i, N-1 );
End;

Rule "j"
	true
==>
Begin
	Increment( j );
End;

Rule "swap"
	i < j  &  a[i] > a[j]
==>
Begin
	Swap( a );
End;


Startstate
Begin
	Clear i;
	Clear j;
	For f : ind_t Do
		a[ f ] := N-1 - f;
	End;
End;

Invariant "hi"
	Exists i : 0..N-2 Do a[i] > a[i+1] End;
