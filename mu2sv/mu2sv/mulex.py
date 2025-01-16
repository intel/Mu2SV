import ply.lex as lex

IDLEN = 256

keywords = {
    'CONST', 'TYPE', 'VAR', 'PROCEDURE', 'FUNCTION',
    'RULE', 'RULESET', 'STARTSTATE', 'INVARIANT',
    'ENUM', 'RECORD', 'ARRAY', 'OF',
    'IF', 'THEN', 'ELSIF', 'ELSE', 'SWITCH', 'CASE',
    'ALIAS', 'FORALL', 'EXISTS', 'FOR', 'WHILE', 'DO',
    'BEGIN', 'END', 'RETURN', 'ERROR', 'ASSERT',
    'CLEAR', 'UNDEFINE', 'ISUNDEFINED'
}

tokens = tuple(keywords) + (
    'ID', 'INTCONST', 'STRING',
    'ASSIGN', 'LONGARROW', 'DOTDOT',
    'IMPLIES', 'LEQ', 'NEQ', 'GEQ'
)

literals = [
    ';', ',', '.', '"', ':',
    '-', '+', '*', '/', '%',
    '{', '}', '(', ')', '[', ']',
    '<', '>', '=', '!', '|', '&', '?'
]

t_DOTDOT = r'\.\.'
t_LONGARROW = r'==>'
t_ASSIGN = r':='
t_LEQ = r'<='
t_NEQ = r'!='
t_GEQ = r'>='
t_IMPLIES = r'->'
t_ignore = ' \t'

def t_ID(t):
    r'[a-zA-Z_]\w*'
    if len(t.value) > IDLEN:
        print(f'line {t.lexer.lineno}: Identifier {t.value} too long.')
    if t.value[0] == '_':
        print(f'line {t.lexer.lineno}: Identifiers beginning with an '
              f'underscore are reserved by the system.')
    t.type = t.value.upper() if t.value.upper() in keywords else 'ID'
    return t

def t_INTCONST(t):
    r'\d+'
    t.value = int(t.value)
    return t

def t_STRING(t):
    r'\"[^"\n]*\"'
    t.value = t.value[1:-1]
    return t

def t_newline(t):
    r'\n'
    t.lexer.lineno += 1

def t_comment(t):
    r'(/\*(.|\n)*?\*/)|(--.*)'
    t.lexer.lineno += len(t.value.splitlines()) - 1

def t_error(t):
    print(f'line {t.lexer.lineno}: Bad input character "{t.value[0]}".')

lexer = lex.lex()
