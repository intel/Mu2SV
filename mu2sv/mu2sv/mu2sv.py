import io, sys
import muparse
from svcodegen import SVCodeGen

def main(argv):
    path = argv[1]
    folder = '/'.join(path.split('/')[:-1])
    filename = path.split('/')[-1]
    name = filename[:-2]
    in_mu = open(path, 'r')
    out_sv_buf = io.StringIO()
    out_sv = open(f'{folder}/{name}.sv', 'w')
    out_tcl = open(f'{folder}/{name}.tcl', 'w')
    ast = muparse.parser.parse(in_mu.read())
    SVCodeGen(name, ast, out_sv_buf).codegen(ast)
    out_sv.write(out_sv_buf.getvalue())
    out_tcl.write(
        f'clear -all\n'
        f'set top {name}\n'
        f'analyze -sv12 {name}.sv\n'
        f'elaborate -top {name}\n'
        f'clock clk\n'
        f'reset reset\n'
    )
    in_mu.close()
    out_sv_buf.close()
    out_sv.close()
    out_tcl.close()

if __name__ == "__main__":
    main(sys.argv)
