import ply.yacc as yacc
from mulex import tokens
from muast import *

precedence = (
    ('nonassoc', 'ASSIGN'),
    ('nonassoc', '?', ':'),
    ('nonassoc', 'IMPLIES'),
    ('left', '|'),
    ('left', '&'),
    ('left', '!'),
    ('nonassoc', '<', 'LEQ', '=', 'NEQ', 'GEQ', '>'),
    ('left', '+', '-'),
    ('left', '*', '/', '%'),
)

def p_prog(p):
    '''
    prog : decls procdecls rules
    '''
    bool_decls = [
        ConstDecl('false', C(0)),
        ConstDecl('true', C(1)),
        TypeDecl('boolean', SubrangeType(C(0), C(1)))
    ]
    p[0] = Program(bool_decls + p[1], p[2], p[3])

def p_decls(p):
    '''
    decls : decls decl
          | empty
    '''
    if len(p) == 3:
        p[0] = p[1] + p[2]
    else:
        p[0] = []

def p_decl(p):
    '''
    decl : CONST constdecls
         | TYPE typedecls
         | VAR vardecls
    '''
    p[0] = p[2]

def p_constdecls(p):
    '''
    constdecls : constdecls constdecl ';'
               | empty
    '''
    if len(p) == 4:
        p[0] = p[1] + [p[2]]
    else:
        p[0] = []
        
def p_constdecl(p):
    '''
    constdecl : ID ':' expr
    '''
    p[0] = ConstDecl(p[1], p[3])

def p_typedecls(p):
    '''
    typedecls : typedecls typedecl ';'
              | empty
    '''
    if len(p) == 4:
        p[0] = p[1] + [p[2]]
    else:
        p[0] = []

def p_typedecl(p):
    '''
    typedecl : ID ':' typeexpr
    '''
    p[0] = TypeDecl(p[1], p[3])

def p_typeexpr(p):
    '''
    typeexpr : typeid
             | enumtype
             | subrangetype
             | recordtype
             | arraytype
    '''
    p[0] = p[1]

def p_typeid(p):
    '''
    typeid : ID
    '''
    p[0] = TypeID(p[1])

def p_enumtype(p):
    '''
    enumtype : ENUM '{' enums '}'
    '''
    p[0] = EnumType(p[3])

def p_enums(p):
    '''
    enums : enums ',' ID
          | ID
    '''
    if len(p) == 4:
        p[0] = p[1] + [p[3]]
    else:
        p[0] = [p[1]]
    
def p_subrangetype(p):
    '''
    subrangetype : expr DOTDOT expr
    '''
    p[0] = SubrangeType(p[1], p[3])

def p_recordtype(p):
    '''
    recordtype : RECORD fields END
    '''
    p[0] = RecordType(p[2])

def p_fields(p):
    '''
    fields : fields ';' field
           | fields ';'
           | field
    '''
    if len(p) == 4:
        p[0] = p[1] + p[3]
    else:
        p[0] = p[1]

def p_field(p):
    '''
    field : ID fieldtail
    '''
    tail = p[2]
    tail[-1].id = p[1]
    p[0] = tail

def p_fieldtail(p):
    '''
    fieldtail : ',' ID fieldtail
              | ':' typeexpr
    '''
    if len(p) == 4:
        tail = p[3]
        typeexpr = tail[-1].typeexpr
        p[0] = [Field(p[2], typeexpr)] + tail
    else:
        p[0] = [Field(None, p[2])]

def p_arraytype(p):
    '''
    arraytype : ARRAY '[' typeexpr ']' OF typeexpr
    '''
    p[0] = ArrayType(p[3], p[6])

def p_vardecls(p):
    '''
    vardecls : vardecls vardecl ';'
             | empty
    '''
    if len(p) == 4:
        p[0] = p[1] + p[2]
    else:
        p[0] = []

def p_vardecl(p):
    '''
    vardecl : ID vardecltail
    '''
    tail = p[2]
    tail[-1].id = p[1]
    p[0] = tail

def p_vardecltail(p):
    '''
    vardecltail : ',' ID vardecltail
                | ':' typeexpr
    '''
    if len(p) == 4:
        tail = p[3]
        typeexpr = tail[-1].typeexpr
        p[0] = [VarDecl(p[2], typeexpr)] + tail
    else:
        p[0] = [VarDecl(None, p[2])]

def p_procdecls(p):
    '''
    procdecls : procdecls procdecl
              | procdecls funcdecl
              | empty
    '''
    if len(p) == 3:
        p[0] = p[1] + [p[2]]
    else:
        p[0] = []

def p_procdecl(p):
    '''
    procdecl : PROCEDURE ID '(' optformals ')' ';' optdecls optstmts END ';'
    '''
    p[0] = ProcDecl(p[2], p[4], p[7], p[8])

def p_funcdecl(p):
    '''
    funcdecl : FUNCTION ID '(' optformals ')' ':' typeexpr ';' optdecls optstmts END ';'
    '''
    p[0] = FuncDecl(p[2], p[4], p[7], p[9], p[10])

def p_optformals(p):
    '''
    optformals : formals
               | empty
    '''
    if p[1]:
        p[0] = p[1]
    else:
        p[0] = []

def p_formals(p):
    '''
    formals : formals ';' formal
            | formals ';'
            | formal
    '''
    if len(p) == 4:
        p[0] = p[1] + p[3]
    else:
        p[0] = p[1]

def p_formal(p):
    '''
    formal : VAR formalrest
           | formalrest
    '''
    if len(p) == 3:
        for f in p[2]:
            f.byref = True
        p[0] = p[2]
    else:
        p[0] = p[1]

def p_formalrest(p):
    '''
    formalrest : ID formaltail
    '''
    tail = p[2]
    tail[-1].id = p[1]
    p[0] = tail

def p_formaltail(p):
    '''
    formaltail : ',' ID formaltail
               | ':' typeexpr
    '''
    if len(p) == 4:
        tail = p[3]
        typeexpr = tail[-1].typeexpr
        p[0] = [Formal(False, p[2], typeexpr)] + tail
    else:
        p[0] = [Formal(False, None, p[2])]

def p_optdecls(p):
    '''
    optdecls : decls BEGIN
             | empty
    '''
    if len(p) == 3:
        p[0] = p[1]
    else:
        p[0] = []

def p_designator(p):
    '''
    designator : ID
               | designator '[' expr ']'
               | designator '.' ID
    '''
    if len(p) == 2:
        p[0] = IDDes(p[1])
    elif len(p) == 5:
        p[0] = ArrDes(p[1], p[3])
    else:
        p[0] = RecDes(p[1], p[3])

def p_ternaryexpr(p):
    '''
    expr : expr '?' expr ':' expr
    '''
    p[0] = CondOp(p[1], p[3], p[5])

def p_binaryexpr(p):
    '''
    expr : expr IMPLIES expr
         | expr '|' expr
         | expr '&' expr
         | expr '<' expr
         | expr LEQ expr
         | expr '>' expr
         | expr GEQ expr
         | expr '=' expr
         | expr NEQ expr
         | expr '+' expr
         | expr '-' expr
         | expr '*' expr
         | expr '/' expr
         | expr '%' expr
    '''
    p[0] = BinOp(p[2], p[1], p[3])

def p_unaryexpr(p):
    '''
    expr : '!' expr
         | '+' expr %prec '*'
         | '-' expr %prec '*'
    '''
    p[0] = UnOp(p[1], p[2])

def p_constexpr(p):
    '''
    expr : INTCONST
    '''
    p[0] = C(p[1])

def p_desigexpr(p):
    '''
    expr : designator
    '''
    p[0] = p[1]

def p_funcexpr(p):
    '''
    expr : ID actuals
    '''
    p[0] = FuncCall(p[1], p[2])

def p_undefexpr(p):
    '''
    expr : ISUNDEFINED '(' designator ')'
    '''
    p[0] = IsUndefined(p[3])

def p_parenexpr(p):
    '''
    expr : '(' expr ')'
    '''
    p[0] = p[2]

def p_quantexpr(p):
    '''
    expr : FORALL quantifiers DO expr END
         | EXISTS quantifiers DO expr END
    '''
    match p[1].upper():
        case 'FORALL':
            p[0] = ForAll(p[2], p[4])
        case 'EXISTS':
            p[0] = Exists(p[2], p[4])

def p_actuals(p):
    '''
    actuals : '(' exprlist ')'
            | '(' ')'
    '''
    if len(p) == 4:
        p[0] = p[2]
    else:
        p[0] = []

def p_exprlist(p):
    '''
    exprlist : exprlist ',' expr
             | expr
    '''
    if len(p) == 4:
        p[0] = p[1] + [p[3]]
    else:
        p[0] = [p[1]]

def p_quantifier(p):
    '''
    quantifier : ID ':' typeexpr
    '''
    p[0] = Quantifier(p[1], p[3])

def p_quantifiers(p):
    '''
    quantifiers : quantifier ';' quantifiers
                | quantifier
    '''
    if len(p) == 4:
        p[0] = [p[1]] + p[3]
    else:
        p[0] = [p[1]]

def p_optstmts(p):
    '''
    optstmts : stmts
             | empty
    '''
    if p[1]:
        p[0] = p[1]
    else:
        p[0] = []

def p_stmts(p):
    '''
    stmts : stmts ';' stmt
          | stmts ';'
          | stmt
    '''
    if len(p) == 4:
        p[0] = p[1] + [p[3]]
    elif len(p) == 3:
        p[0] = p[1]
    else:
        p[0] = [p[1]]

def p_stmt(p):
    '''
    stmt : assignment
         | ifstmt
         | whilestmt
         | switchstmt
         | forstmt
         | proccall
         | clearstmt
         | errorstmt
         | assertstmt
         | aliasstmt
         | returnstmt
         | undefinestmt
    '''
    p[0] = p[1]

def p_assignment(p):
    '''
    assignment : designator ASSIGN expr
    '''
    p[0] = Assignment(p[1], p[3])

def p_ifstmt(p):
    '''
    ifstmt : IF expr THEN optstmts optelses END
    '''
    p[0] = If(p[2], p[4], p[5])

def p_optelses(p):
    '''
    optelses : elsif optelses
             | optelse
    '''
    if len(p) == 3:
        p[0] = [p[1]] + p[2]
    elif p[1]:
        p[0] = [p[1]]
    else:
        p[0] = []

def p_elsif(p):
    '''
    elsif : ELSIF expr THEN optstmts
    '''
    p[0] = Elsif(p[2], p[4])

def p_optelse(p):
    '''
    optelse : ELSE optstmts
            | empty
    '''
    if len(p) == 3:
        p[0] = Else(p[2])
    else:
        p[0] = None

def p_whilestmt(p):
    '''
    whilestmt : WHILE expr DO optstmts END
    '''
    p[0] = While(p[2], p[4])

def p_switchstmt(p):
    '''
    switchstmt : SWITCH expr optcases optelse END
    '''
    p[0] = Switch(p[2], p[3], p[4])

def p_optcases(p):
    '''
    optcases : optcases case
             | empty
    '''
    if len(p) == 3:
        p[0] = p[1] + [p[2]]
    else:
        p[0] = []


def p_case(p):
    '''
    case : CASE exprlist ':' optstmts
    '''
    p[0] = Case(p[2], p[4])

def p_forstmt(p):
    '''
    forstmt : FOR quantifiers DO optstmts END
    '''
    p[0] = For(p[2], p[4])

def p_proccall(p):
    '''
    proccall : ID actuals
    '''
    p[0] = ProcCall(p[1], p[2])

def p_clearstmt(p):
    '''
    clearstmt : CLEAR designator
    '''
    p[0] = Clear(p[2])

def p_undefinestmt(p):
    '''
    undefinestmt : UNDEFINE designator
    '''
    p[0] = Undefine(p[2])

def p_errorstmt(p):
    '''
    errorstmt : ERROR STRING
    '''
    p[0] = Error(p[2])

def p_assertstmt(p):
    '''
    assertstmt : ASSERT expr optstring
    '''
    p[0] = Assert(p[2], p[3])

def p_aliasstmt(p):
    '''
    aliasstmt : ALIAS aliases DO optstmts END
    '''
    p[0] = AliasStmt(p[2], p[4])

def p_aliases(p):
    '''
    aliases : aliases ';' alias
            | aliases ';'
            | alias
    '''
    if len(p) == 4:
        p[0] = p[1] + [p[3]]
    elif len(p) == 3:
        p[0] = p[1]
    else:
        p[0] = [p[1]]

def p_alias(p):
    '''
    alias : ID ':' expr
    '''
    p[0] = Alias(p[1], p[3])

def p_returnstmt(p):
    '''
    returnstmt : RETURN optretexpr
    '''
    p[0] = Return(p[2])

def p_optretexpr(p):
    '''
    optretexpr : expr
               | empty
    '''
    if p[1]:
        p[0] = p[1]
    else:
        p[0] = None

def p_rules(p):
    '''
    rules : rule ';' rules
          | rule ';'
          | rule
    '''
    if len(p) == 4:
        p[0] = [p[1]] + p[3]
    else:
        p[0] = [p[1]]

def p_rule(p):
    '''
    rule : simplerule
         | aliasrule
         | ruleset
         | startstate
         | invariant
    '''
    p[0] = p[1]

def p_simplerule(p):
    '''
    simplerule : RULE optstring optcondition optdecls optstmts END
    '''
    p[0] = SimpleRule(p[2], p[3], p[4], p[5])

def p_optcondition(p):
    '''
    optcondition : expr LONGARROW
                 | empty
    '''
    if len(p) == 3:
        p[0] = p[1]
    else:
        p[0] = None

def p_optstring(p):
    '''
    optstring : STRING
              | empty
    '''
    if p[1]:
        p[0] = p[1]
    else:
        p[0] = ''

def p_aliasrule(p):
    '''
    aliasrule : ALIAS aliases DO rules END
    '''
    p[0] = AliasRule(p[2], p[4])

def p_ruleset(p):
    '''
    ruleset : RULESET quantifiers DO rules END
    '''
    p[0] = RuleSet(p[2], p[4])

def p_startstate(p):
    '''
    startstate : STARTSTATE optstring optdecls optstmts END
    '''
    p[0] = StartState(p[2], p[3], p[4])

def p_invariant(p):
    '''
    invariant : INVARIANT optstring expr
    '''
    p[0] = Invariant(p[2], p[3])

def p_empty(p):
    '''
    empty :
    '''
    pass

def p_error(p):
    print(p)

parser = yacc.yacc()
