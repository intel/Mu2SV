const
  MAX: 10;
type
  cnt_t: 0..MAX;
  rec_t: record cnt: cnt_t; end;
  rec_ar_t: array[cnt_t] of rec_t;
var
  rec_ar: rec_ar_t;

procedure incr(var rec: rec_t);
var local_rec: rec_t;
begin
  local_rec := rec;
  local_rec.cnt := local_rec.cnt + 1;
  
end;

rule "increment"
begin
  for i : cnt_t do
    incr(rec_ar[i]);
  end;
end;

startstate
begin
  for i : cnt_t do
    rec_ar[i].cnt := 0;
  end;
end;

invariant "max not hit"
  forall i : cnt_t do
    rec_ar[i].cnt < MAX
  end;
