import re

class MuAST:
    def __str__(self, t=0):
        def indent(s, n):
            return '  '*n + s
        
        def list_str(ls):
            if not ls:
                return indent('[]', t + 1)
            s = indent('[\n', t + 1)
            for i in ls:
                if isinstance(i, MuAST):
                    s += i.__str__(t + 2)
                else:
                    s += indent(repr(i), t + 2)
                s += ',\n'
            s = s[:-2] + '\n' + indent(']', t + 1)
            ns = indent(re.sub(r'\s', '', s), t + 1)
            return ns if len(ns) <= 128 else s

        res = indent(f'{self.__class__.__name__}(\n', t)
        for attr in self.__dict__.values():
            if isinstance(attr, MuAST):
                res += attr.__str__(t + 1)
            elif isinstance(attr, list):
                res += list_str(attr)
            else:
                res += indent(repr(attr), t + 1)
            res += ',\n'
        
        res = res[:-2] + '\n' + indent(')', t)
        nospace = indent(re.sub(r'\s', '', res), t)
        return nospace if len(nospace) <= 128 else res

# =============================================================================
# PROGRAM
# =============================================================================

class Program(MuAST):
    def __init__(self, decls, procdecls, rules):
        self.decls = decls
        self.procdecls = procdecls
        self.rules = rules

# =============================================================================
# DECLARATION
# =============================================================================

class Declaration(MuAST): pass

class ConstDecl(Declaration):
    def __init__(self, id, expr):
        self.id = id
        self.expr = expr

class TypeDecl(Declaration):
    def __init__(self, id, typeexpr):
        self.id = id
        self.typeexpr = typeexpr

class VarDecl(Declaration):
    def __init__(self, id, typeexpr):
        self.id = id
        self.typeexpr = typeexpr

class ProcDecl(Declaration):
    def __init__(self, id, formals, localdecls, statements):
        self.id = id
        self.formals = formals
        self.localdecls = localdecls
        self.statements = statements

class FuncDecl(ProcDecl):
    def __init__(self, id, formals, returntype, localdecls, statements):
        super().__init__(id, formals, localdecls, statements)
        self.returntype = returntype

class Formal(Declaration):
    def __init__(self, byref, id, typeexpr):
        self.byref = byref
        self.id = id
        self.typeexpr = typeexpr

class Field(Declaration):
    def __init__(self, id, typeexpr):
        self.id = id
        self.typeexpr = typeexpr

class Quantifier(Declaration):
    def __init__(self, id, rangetype):
        self.id = id
        self.rangetype = rangetype

class Alias(Declaration):
    def __init__(self, id, expr):
        self.id = id
        self.expr = expr

# =============================================================================
# RULE
# =============================================================================

class Rule(MuAST): pass

class SimpleRule(Rule):
    def __init__(self, string, condition, localdecls, statements):
        self.string = string
        self.condition = condition
        self.localdecls = localdecls
        self.statements = statements

class StartState(SimpleRule):
    def __init__(self, string, localdecls, statements):
        super().__init__(string, 'startstate', localdecls, statements)
        
class Invariant(SimpleRule):
    def __init__(self, string, condition):
        super().__init__(string, condition, [], ['invariant'])

class AliasRule(Rule):
    def __init__(self, aliases, rules):
        self.aliases = aliases
        self.rules = rules

class RuleSet(Rule):
    def __init__(self, quantifiers, rules):
        self.quantifiers = quantifiers
        self.rules = rules

class FlattenedRule(SimpleRule):
    def __init__(self, params, string, condition, localdecls, statements):
        self.params = params
        self.string = string
        self.condition = condition
        self.localdecls = localdecls
        self.statements = statements

class FlattenedInvariant(FlattenedRule):
    def __init__(self, params, string, condition):
        super().__init__(params, string, condition, [], ['invariant'])

# =============================================================================
# STATEMENT
# =============================================================================

class Statement(MuAST): pass

class Assignment(Statement):
    def __init__(self, designator, expr):
        self.designator = designator
        self.expr = expr

class For(Statement):
    def __init__(self, quantifiers, statements):
        self.quantifiers = quantifiers
        self.statements = statements

class While(Statement):
    def __init__(self, condition, statements):
        self.condition = condition
        self.statements = statements

class Return(Statement):
    def __init__(self, returnval):
        self.returnval = returnval

class ProcCall(Statement):
    def __init__(self, id, actuals):
        self.id = id
        self.actuals = actuals

class Error(Statement):
    def __init__(self, message):
        self.message = message

class Assert(Statement):
    def __init__(self, condition, message):
        self.condition = condition
        self.message = message

class Clear(Statement):
    def __init__(self, designator):
        self.designator = designator

class Undefine(Statement):
    def __init__(self, designator):
        self.designator = designator

class If(Statement):
    def __init__(self, condition, statements, elses):
        self.condition = condition
        self.statements = statements
        self.elses = elses

class Elsif(MuAST):
    def __init__(self, condition, statements):
        self.condition = condition
        self.statements = statements

class Else(MuAST):
    def __init__(self, statements):
        self.statements = statements

class Switch(Statement):
    def __init__(self, switcher, cases, elsecase):
        self.switcher = switcher
        self.cases = cases
        self.elsecase = elsecase

class Case(MuAST):
    def __init__(self, matches, statements):
        self.matches = matches
        self.statements = statements

class AliasStmt(Statement):
    def __init__(self, aliases, statements):
        self.aliases = aliases
        self.statements = statements

# =============================================================================
# TYPE EXPRESSION
# =============================================================================

class TypeExpr(MuAST): pass

class TypeID(TypeExpr):
    def __init__(self, id):
        self.id = id

class EnumType(TypeExpr):
    def __init__(self, enums):
        self.enums = enums

class SubrangeType(TypeExpr):
    def __init__(self, low, high):
        self.low = low
        self.high = high

class ArrayType(TypeExpr):
    def __init__(self, rangetype, itemtype):
        self.rangetype = rangetype
        self.itemtype = itemtype

class RecordType(TypeExpr):
    def __init__(self, fields):
        self.fields = fields

# =============================================================================
# EXPRESSION
# =============================================================================

class Expression(MuAST): pass

class C(Expression):
    def __init__(self, value):
        self.value = value

class Designator(Expression): pass

class IDDes(Designator):
    def __init__(self, id):
        self.id = id

class ArrDes(Designator):
    def __init__(self, array, index):
        self.array = array
        self.index = index

class RecDes(Designator):
    def __init__(self, parent, child):
        self.parent = parent
        self.child = child

class UnOp(Expression):
    def __init__(self, op, expr):
        self.op = op
        self.expr = expr

class BinOp(Expression):
    def __init__(self, op, expr1, expr2):
        self.op = op
        self.expr1 = expr1
        self.expr2 = expr2

class CondOp(Expression):
    def __init__(self, condition, consequent, alternative):
        self.condition = condition
        self.consequent = consequent
        self.alternative = alternative

class FuncCall(Expression):
    def __init__(self, id, actuals):
        self.id = id
        self.actuals = actuals

class IsUndefined(Expression):
    def __init__(self, designator):
        self.designator = designator

class QuantExpr(Expression):
    def __init__(self, quantifiers, condition):
        self.quantifiers = quantifiers
        self.condition = condition
class ForAll(QuantExpr): pass
class Exists(QuantExpr): pass
