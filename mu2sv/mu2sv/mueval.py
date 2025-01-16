from muast import *

# =========================================================
# ENVIORNMENT
# =========================================================

class Env(dict):
    def __init__(self, astnode=None, parent=None):
        self.astnode = astnode
        self.parent = parent
    
    def lookup(self, id):
        if id in self:
            return self
        if self.parent:
            return self.parent.lookup(id)

    def lookdown(self, id):
        if id in self:
            return self
        for child in self.values():
            if isinstance(child, Env):
                res = child.lookdown(id)
                if res: return res

def muenv(node, env):
    if not isinstance(node, MuAST):
        return
    local_env = env
    if env is None:
        local_env = Env(node, None)
    if pushes_scope(node):
        local_env = Env(node, env)
        env[node] = local_env
    if isinstance(node, Declaration):
        env[node.id] = node
    if isinstance(node, EnumType):
        enum_env = nonrec_env(env)
        for enum in node.enums:
            enum_env[enum] = node
    for child in node.__dict__.values():
        if isinstance(child, MuAST):
            muenv(child, local_env)
        elif isinstance(child, list):
            for c in child:
                muenv(c, local_env)
    return local_env

def pushes_scope(node):
    return isinstance(node, (
        Program, ProcDecl, RecordType,
        Rule, For, AliasStmt, QuantExpr))

def typebase(node, env):
    if isinstance(node, TypeID):
        base_env = env.lookup(node.id)
        base_type = base_env[node.id].typeexpr
        return typebase(base_type, base_env)
    elif isinstance(node, TypeExpr):
        return node, env

def exprtypebase(node, env):
    if isinstance(node, Designator):
        return destypebase(node, env)
    elif isinstance(node, FuncCall):
        func_env = env.lookup(node.id)
        func = func_env[node.id]
        return typebase(func.returntype, func_env)
    elif isinstance(node, Expression):
        return int, env.lookup('boolean')

def destypebase(node, env):
    if isinstance(node, IDDes):
        src_env = env.lookup(node.id)
        src = src_env[node.id]
        if isinstance(src, ConstDecl):
            return 'const', src_env
        if isinstance(src, EnumType):
            return src, src_env
        elif isinstance(src, Alias):
            return exprtypebase(src.expr, src_env)
        elif isinstance(src, Quantifier):
            return typebase(src.rangetype, src_env)
        else:
            return typebase(src.typeexpr, src_env)
    elif isinstance(node, ArrDes):
        arr, arr_env = destypebase(node.array, env)
        return typebase(arr.itemtype, arr_env)
    elif isinstance(node, RecDes):
        rec, rec_env = destypebase(node.parent, env)
        field = rec_env[rec][node.child]
        return typebase(field.typeexpr, rec_env[rec])

def isquantifier(node, env):
    if isinstance(node, IDDes):
        src_env = env.lookup(node.id)
        src = src_env[node.id]
        if isinstance(src, Alias):
            return isquantifier(src.expr, src_env)
        elif isinstance(src, Quantifier):
            return typebase(src.rangetype, src_env)
    return False

def nonrec_env(env):
    res = env
    while isinstance(res.astnode, RecordType):
        res = res.parent
    return res

# =========================================================
# EVALUATION
# =========================================================

def mueval(node, env):
    cls = node.__class__
    while f'eval_{cls.__name__}' not in globals():
        cls = cls.__bases__[0]
    return globals()[f'eval_{cls.__name__}'](node, env)

def eval_MuAST(node, env):
    pass

def eval_ConstDecl(node, env):
    return mueval(node.expr, env)

def eval_C(node, env):
    return node.value

def eval_IDDes(node, env):
    src_env = env.lookup(node.id)
    return mueval(src_env[node.id], src_env)

def eval_UnOp(node, env):
    ex = mueval(node.expr, env)
    match node.op:
        case '!':
            return not ex
        case '+':
            return ex
        case '-':
            return -ex

def eval_BinOp(node, env):
    ex1 = mueval(node.expr1, env)
    ex2 = mueval(node.expr2, env)
    match node.op:
        case '->':
            return not ex1 or ex2
        case '|':
            return ex1 or ex2
        case '&':
            return ex1 and ex2
        case '<':
            return ex1 < ex2
        case '<=':
            return ex1 <= ex2
        case '=':
            return ex1 == ex2
        case '!=':
            return ex1 != ex2
        case '>=':
            return ex1 >= ex2
        case '>':
            return ex1 > ex2
        case '+':
            return ex1 + ex2
        case '-':
            return ex1 - ex2
        case '*':
            return ex1 * ex2
        case '/':
            return ex1 // ex2
        case '%':
            return ex1 % ex2
