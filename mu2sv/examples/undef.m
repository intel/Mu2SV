const
  MAX: 10;
type
  cnt_t: 0..MAX;
var
  cnt: cnt_t;

rule "increment"
begin
  cnt := cnt + 1;
end;

startstate
begin
  -- cnt := 1;
end;

invariant "undefined"
  !isundefined(cnt)
