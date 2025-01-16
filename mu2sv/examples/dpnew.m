const
  N: 3;
  option: 1;

type
  Phil_ID: 0..N-1;
  side: enum {
    left, right
  };
  actions: enum {
    think, take, eat, release
  };
  phil_t: record
    Status: actions;
    Got_Forks: array [side] of boolean;
  end;

var
  Philosophers:
    array [Phil_ID] of phil_t;

ruleset ID : Phil_ID do
  rule "stop thinking and sit"
    (option = 1 | option = 2) &
    Philosophers[ID].Status = think
  ==>
  begin
    Philosophers[ID].Status := take;
  end;

  rule "stop thinking and sit if ok"
    option = 3 &
    Philosophers[ID].Status = think &
    exists ID1: Phil_ID do
      ID1 != ID & Philosophers[ID1].Status = think
    end
  ==>
  begin
    Philosophers[ID].Status := take;
  end;

  rule "take left fork"
    (option = 1 | option = 3) &
    Philosophers[ID].Status = take &
    Philosophers[ID].Got_Forks[left] = false
  ==>
  begin
    if !(Philosophers[(ID=N-1)?0:ID+1].Got_Forks[right]) then
      Philosophers[ID].Got_Forks[left] := true;
    else
      -- cannot get fork, have to wait
    end;
  end;

  rule "take right fork"
    (option = 1 | option = 3) &
    Philosophers[ID].Status = take &
    Philosophers[ID].Got_Forks[right] = false
  ==>
  begin
    if !(Philosophers[(ID=0)?N-1:ID-1].Got_Forks[left]) Then
      Philosophers[ID].Got_Forks[right] := true;
    else
      -- cannot get fork, have to wait
    end;
  end;

  rule "start eating if got both forks"
    (option = 1 | option = 3) &
    forall s: side do
      Philosophers[ID].Got_Forks[s]
    end
  ==>
  begin
    Philosophers[ID].Status := eat;
  end;

  rule "take both fork"
    option = 2 &
    Philosophers[ID].Status = take &
    forall s: side do
      !Philosophers[ID].Got_Forks[s]
    end
  ==>
  begin
    if !(Philosophers[(ID=0)?N-1:ID-1].Got_Forks[left]) &
       !(Philosophers[(ID=N-1)?0:ID+1].Got_Forks[right])
    then
      for s: side do
        Philosophers[ID].Got_Forks[s] := true;
      end;
      Philosophers[ID].Status := eat;
    else
      -- cannot get fork, have to wait
    end;
  end;

  rule "finished eating"
    Philosophers[ID].Status = eat
  ==>
  begin
    Philosophers[ID].Status := release;
  end;

  rule "release left fork"
    Philosophers[ID].Status = release
  ==>
  begin
    Philosophers[ID].Got_Forks[left] := false;
  end;

  rule "release right fork"
    Philosophers[ID].Status = release
  ==>
  begin
    Philosophers[ID].Got_Forks[right] := false;
  end;

  rule "start thinking"
    Philosophers[ID].Status = release &
    forall s: side do
      Philosophers[ID].Got_Forks[s] = false
    end
  ==>
  begin
    Philosophers[ID].Status := think;
  end;
end;

startstate
begin
  for ID: Phil_ID do
    Philosophers[ID].Status := think;
    for s: side do
      Philosophers[ID].Got_Forks[s] := false;
    end;
  end;
end;

-- invariant "All eating philosophers have both forks available."
--   forall ID : Phil_ID do
--     Philosophers[ID].Status = eat ->
--     forall s: side do
--       Philosophers[ID].Got_Forks[s]
--     end
--   end;

-- invariant "A fork is used by upto one philosophers only"
--   forall ID: Phil_ID do
--     Philosophers[ID].Got_Forks[left] ->
--     !Philosophers[(ID=N-1)?0:ID+1].Got_Forks[right]
--   end &
--   forall ID: Phil_ID do
--     Philosophers[ID].Got_Forks[right] ->
--     !Philosophers[(ID=0)?N-1:ID-1].Got_Forks[left]
--   end;
