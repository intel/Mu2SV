import io, sys
import muparse
from svcodegen import SVCodeGen

import os

def validate_not_symlink(filepath: str) -> bool:
    if not os.path.islink(filepath):
        return True
    else:
        return False

def validate_filepath(filepath: str) -> bool:
    """
    Validates a given file path.

    Checks if:
    - The path exists.
    - It is a file (not a directory).
    - It does not contain directory traversal sequences like '../' or './'.
    - It follows the OS-specific path format.

    :param filepath: The file path to validate.
    :return: True if the file path is valid, False otherwise.
    """
    if not isinstance(filepath, str) or not filepath.strip():
        return False  # Not a valid string
    
    # Normalize the path to remove redundant separators and relative components
    normalized_path = os.path.normpath(filepath)

    # Check for directory traversal (path should not change after normalization)
    if normalized_path != filepath or '..' in normalized_path.split(os.sep):
        return False  # Path contains traversal sequences

    # Check if the path exists and is a file
    if not os.path.isfile(normalized_path):
        return False

    return True
    

def main(argv):
    path = argv[1]
    if validate_filepath(path) == False:
        raise Exception("Invalid filepath .. or . not allowed")
    if validate_not_symlink(path) == False:
        raise Exception("Invalid filepath .. symlink not allowed")

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
