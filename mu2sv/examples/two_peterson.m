
const
  NUM_AGENTS:      10;
  MAX_OWNERS:       3;

type
  agent_t       : 1..NUM_AGENTS;
  agent_count_t : 0..NUM_AGENTS;
  owner_t       : 1..MAX_OWNERS;

var
  owners : array[owner_t] of agent_t;
  token  : array[agent_t] of boolean;
  flag   : boolean;

function count_owners() : agent_count_t;
  var c : agent_count_t;
begin
  c := 0;
  for a : agent_t do
    if token[a] then
      c := c+1;
    end;
  end;
  return c;
end;

startstate "start"
  for a: agent_t do
    token[a] := false;
  end;
  flag := false;
end;

ruleset a: agent_t do

  ruleset i: owner_t do
    rule "grab token"
      isundefined(owners[i])
      ==>
      token[a] := true;
      owners[i] := a;
    end;
  end;

  rule "release token"
    token[a]
    ==>
    token[a] := false;
    for i : owner_t do
      if (a = owners[i]) then
        undefine owners[i];
      end;
    end;
  end;

end;

rule "cover me"
  count_owners() = MAX_OWNERS 
  ==> 
  flag := true;
end;

rule "cover me 2"
  flag & (count_owners() = 0)
  ==> 
end;

invariant "tokens are mutex"
  count_owners() <= MAX_OWNERS


