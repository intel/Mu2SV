Const
	MAX_SET_SIZE: 12;
	MAX_OPS: 1000;
	NUM_ELT: 4;

Type
	elt_t: 1..NUM_ELT;
	entry_t: 0..NUM_ELT; 

	setInd_t: 0..MAX_SET_SIZE-1;
	ar_t: Array [ setInd_t ] Of entry_t;
	a_t: 0..MAX_SET_SIZE;
	b_t: 0..MAX_OPS;
	c_t: 0..MAX_OPS;
	set_t: Record
		a: ar_t;
		n: a_t;
		numIns: b_t;
		numDel: c_t;
	End;

Var
	s: set_t;

Procedure Sort( Var s: set_t );
Var tmpElt : entry_t;
Begin
	For i: 0..MAX_SET_SIZE-2 Do

	End;
End;

Procedure Insert( Var s: set_t;   e: elt_t );
Begin
	s.a[ s.n ] := e;
	s.n := s.n + 1;
	s.numIns := s.numIns + 1;
	Sort( s );
End;

Procedure Delete( Var s: set_t;  e:elt_t );
Begin
	For i: setInd_t Do
		If s.a[ i ] = e then
			s.a[ i ] := 0;
			s.n := s.n - 1;
			s.numDel := s.numDel + 1;
		End;
	End;
	Sort( s );
End;

Ruleset e : elt_t Do
	Rule "Insert elt"
		s.n < MAX_SET_SIZE - 1  &  ! Exists i: setInd_t Do s.a[i] = e End
	==>
	Begin
		Insert( s, e );
	End;

	Rule "Delete last inserted elt"
		s.n > 0
	==>
	Begin
		Delete( s, e );
	End;

End;

Startstate
  Begin
	s.n := 0;
	For i : setInd_t Do
		s.a[ i ] := 0;
	End;
	s.numIns := 0;
	s.numDel := 0;
  End;

Invariant "hello"
	s.numIns + s.numDel < 5;
