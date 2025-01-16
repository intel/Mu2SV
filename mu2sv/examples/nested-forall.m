/* Example model for reproducing a bug with nested forall expressions. At time
 * of writing, the synthesized loop accumulators in the SV output are the same
 * variable. That is, both the inner and outer forall expressions accumulate
 * their result in a variable with the same name, quant_var_0.
 */

type
    r: 0..1;
var
    x: boolean;

startstate "hello world"
begin
    x := false;
end;

rule "world hello"
begin
    x := !x;
end;

invariant "yellow furled"
    forall a: r do
        forall b: r do
            x | !x
        end
    end;
