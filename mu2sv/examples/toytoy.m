const y: 1;
type t: boolean;
var x : 1 .. 5;
startstate begin x := 1; end;
rule "name" true ==> begin x := x + 1; end;
