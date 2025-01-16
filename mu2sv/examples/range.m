const
  c: 0;
type
  cnt_t: 20..21;
var
  cnt: cnt_t;

rule "a to b"
cnt = 20
==>
begin
  cnt := 21;
end;

rule "b to a"
cnt = 21
==>
begin
  cnt := 20;
end;

rule "noop"
begin
end;

startstate
begin
  cnt := 21;
end;

invariant "a or b"
  cnt = 21 | cnt = 20
