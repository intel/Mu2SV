import re
import mueval
from muast import *

class SVCodeGen:
    def __init__(self, muname, muast, out):
        self.model_name = muname
        self.prog_ast = muast
        self.output_buffer = out
        self.sentinel_env = mueval.Env()
        mueval.muenv(muast, self.sentinel_env)
        self.local_env = self.sentinel_env
        self.indent = 0
        self.maxloop = 10
        self.aliases = []
        self.rule_ids = []

    def global_env(self):
        return self.sentinel_env[self.prog_ast]
    
    def codegen(self, node, *args):
        prev_env = self.local_env
        if mueval.pushes_scope(node):
            self.local_env = self.local_env[node]
        cls = node.__class__
        while not getattr(self, f'gen_{cls.__name__}', None):
            cls = cls.__bases__[0]
        res = getattr(self, f'gen_{cls.__name__}')(node, *args)
        self.local_env = prev_env
        return res

    def pp(self, line='', newl=1, ind=None):
        ind = self.indent if ind is None and line else 0
        self.output_buffer.write('  '*ind + line + '\n'*newl)

    def flatten_rules(self):
        def flatten(rule, aliases_quantifiers):
            res = []
            if isinstance(rule, Invariant):
                res.append(FlattenedInvariant(
                    aliases_quantifiers, rule.string, rule.condition))
            elif isinstance(rule, SimpleRule):
                res.append(FlattenedRule(
                    aliases_quantifiers,
                    rule.string, rule.condition,
                    rule.localdecls, rule.statements))
            if isinstance(rule, RuleSet):
                added_quantifers = aliases_quantifiers + rule.quantifiers
                for rule in rule.rules:
                    res += flatten(rule, added_quantifers)
            elif isinstance(rule, AliasRule):
                added_aliases = aliases_quantifiers + rule.aliases
                for rule in rule.rules:
                    res += flatten(rule, added_aliases)
            return res
        flattened_rules = []
        for rule in self.prog_ast.rules:
            flattened_rules += flatten(rule, [])
        self.prog_ast.rules = flattened_rules

    def lo_hi_strs(self, node, env):
        typ, typ_env = mueval.typebase(node, env)
        if isinstance(typ, EnumType):
            return '1', f'{len(typ.enums)}'
        if isinstance(typ, SubrangeType):
            prev_env = self.local_env
            self.local_env = typ_env
            lo, hi = self.codegen(typ.low), self.codegen(typ.high)
            self.local_env = prev_env
            return lo, hi

    def undef_aliases(self):
        i = 0
        while i < len(self.aliases):
            if self.aliases[i][1:] in self.local_env:
                self.pp(f'`undef {self.aliases[i]}')
                self.aliases.pop(i)
            else:
                i += 1

# =============================================================================
# PROGRAM
# =============================================================================

    def gen_MuAST(self, node): pass

    def gen_Program(self, node):
        self.flatten_rules()
        self.local_env = mueval.muenv(node, self.sentinel_env)
        self.pp('`define subrange(lo, hi) bit[$clog2((hi)-(lo)+1):0]')
        self.pp('`define subrange_arr(dim, lo, hi) bit``dim``[$clog2((hi)-(lo)+1):0]')
        self.pp()
        self.pp(f'module {self.model_name} (')
        self.pp('  input clk,')
        self.pp('  input reset')
        self.pp(');')
        self.indent += 1
        for decl in node.decls:
            self.codegen(decl)
        self.pp()
        for procdecl in node.procdecls:
            self.codegen(procdecl)
            self.pp()
        self.pp('bit rule_executed;')
        self.pp('bit initialized;')
        self.pp()
        for i, rule in enumerate(node.rules):
            self.codegen(rule, i)
            self.pp()
        self.gen_always_block()
        self.indent -= 1
        self.pp('endmodule')

    def gen_always_block(self):
        rule_selects = [
            re.sub("^r", "rs", rule_id)
            for i, rule_id in enumerate(self.rule_ids)
            if not isinstance(self.prog_ast.rules[i], FlattenedInvariant)]
        self.pp(f'enum bit[$clog2({len(rule_selects)}):0] ''{')
        self.pp(',\n  '.join(f'{"  "*self.indent}{rs}' for rs in rule_selects))
        self.pp('} rule_select;')
        self.pp('always @(posedge clk) begin')
        self.indent += 1
        self.pp('if (reset) begin')
        self.indent += 1
        self.pp('rule_executed = 1;')
        self.pp('initialized = 0;')
        for decl in self.prog_ast.decls:
            if isinstance(decl, VarDecl):
                self.codegen(Undefine(IDDes(decl.id)))
        self.indent -= 1    
        self.pp('end else begin')
        self.indent += 1
        self.pp('case (rule_select)')
        self.indent += 1
        for i in range(len(rule_selects)):
            self.pp(f'{rule_selects[i]}: {self.rule_ids[i]}();')
        self.indent -= 1
        self.pp('endcase')
        self.indent -= 1
        self.pp('end')
        self.indent -= 1
        self.pp('end')
        self.pp('assume property (@(posedge clk) rule_executed);')
        self.pp('assume property (@(posedge clk) rule_select >= 0 && '
                f'rule_select < {len(rule_selects)});')

# =============================================================================
# DECLARATION
# =============================================================================

    def gen_ConstDecl(self, node):
        self.pp(f'localparam _{node.id} = {self.codegen(node.expr)};')

    def gen_TypeDecl(self, node):
        self.pp(f'typedef {self.codegen(node.typeexpr)} _{node.id};')

    def gen_VarDecl(self, node):
        self.pp(f'{self.codegen(node.typeexpr)} _{node.id};')

    def gen_ProcDecl(self, node):
        returntype = 'void'
        if isinstance(node, FuncDecl):
            returntype = self.codegen(node.returntype)
        self.pp(f'function automatic {returntype} _{node.id}', 0)
        if node.formals:
            self.indent += 1
            formals = ',\n'.join(
                f'{"  "*self.indent}{self.codegen(f)}'
                for f in node.formals)
            self.indent -= 1
            self.pp(f'(\n{formals}\n{"  "*self.indent})', 0, 0)
        self.pp(';', 1, 0)
        self.indent += 1
        if isinstance(node, FuncDecl):
            self.local_env['_return'] = VarDecl('_return', node.returntype)
            self.codegen(self.local_env['_return'])
        for decl in node.localdecls:
            self.codegen(decl)
        if isinstance(node, FuncDecl):
            self.codegen(Undefine(IDDes('_return')))
        for decl in node.localdecls:
            if isinstance(decl, VarDecl):
                self.codegen(Undefine(IDDes(decl.id)))
        for statement in node.statements:
            self.codegen(statement)
        self.indent -= 1
        self.pp('endfunction')

    def gen_Formal(self, node):
        direction = 'ref' if node.byref else 'input'
        return f'{direction} {self.codegen(node.typeexpr)} _{node.id}'

    def gen_Field(self, node):
        return f'{self.codegen(node.typeexpr)} _{node.id};'

    def gen_Alias(self, node):
        exprtype, _ = mueval.exprtypebase(node.expr, self.local_env)
        if isinstance(node.expr, Designator) and not exprtype == 'const':
            self.pp(f'`define _{node.id} {self.codegen(node.expr, False)}')
            self.aliases.append(f'_{node.id}')
        else:
            self.pp(f'`localparam _{node.id} = {self.codegen(node.expr)};')

# =============================================================================
# RULE
# =============================================================================

    def gen_FlattenedRule(self, node, index=0):
        startstate = node.condition == 'startstate'
        rule_id = f'r{index}_{"startstate" if startstate else "rule"}'
        rule_id += f'_{str_id(node.string)}' if node.string else ''
        self.rule_ids.append(rule_id)
        self.pp(f'function void {rule_id};')
        self.indent += 1
        conditions = []
        conditions.append('!initialized' if startstate else 'initialized')
        for param in node.params:
            if isinstance(param, Alias):
                self.codegen(param)
            elif isinstance(param, Quantifier):
                lo, hi = self.lo_hi_strs(param.rangetype, self.local_env)
                conditions.append(f'_{param.id} >= {lo} && _{param.id} <= {hi}')
                self.pp(f'int _{param.id};')
        if not startstate and node.condition:
            conditions.append(f'({self.codegen(node.condition)})')
        self.pp(f'if (')
        self.pp('  ' + f' &&\n{"  "*self.indent}  '.join(conditions))
        self.pp(f') begin')
        self.indent += 1
        for decl in node.localdecls:
            self.codegen(decl)
        for decl in node.localdecls:
            if isinstance(decl, VarDecl):
                self.codegen(Undefine(IDDes(decl.id)))
        for statement in node.statements:
            self.codegen(statement)
        if startstate:
            self.pp('initialized = 1;')
        self.indent -= 1
        self.pp(f'end else begin')
        self.pp(f'  rule_executed = 0;')
        self.pp(f'end')
        self.undef_aliases()
        self.indent -= 1
        self.pp('endfunction')

    def gen_FlattenedInvariant(self, node, index=0):
        rule_id = f'r{index}_invariant'
        rule_id += f'_{str_id(node.string)}' if node.string else ''
        self.rule_ids.append(rule_id)
        self.pp(f'bit {rule_id}_condition;')
        self.pp(f'always_comb begin')
        self.indent += 1
        conditions = []
        for param in node.params:
            if isinstance(param, Alias):
                self.codegen(param)
            elif isinstance(param, Quantifier):
                lo, hi = self.lo_hi_strs(param.rangetype, self.local_env)
                conditions.append(f'_{param.id} >= {lo} && _{param.id} <= {hi}')
                self.pp(f'int _{param.id};')
        conditions.append(self.codegen(node.condition))
        self.pp(f'{rule_id}_condition =')
        self.pp('  ' + f' &&\n{"  "*self.indent}  '.join(conditions) + ';')
        self.undef_aliases()
        self.indent -= 1
        self.pp(f'end')
        self.pp(f'{rule_id}: assert property (')
        self.pp(f'  @(posedge clk) initialized -> {rule_id}_condition);')

# =============================================================================
# STATEMENT
# =============================================================================

    def gen_Assignment(self, node):
        desig = self.codegen(node.designator, False)
        expr = self.codegen(node.expr)
        typ, typ_env = mueval.exprtypebase(node.designator, self.local_env)
        if isinstance(typ, SubrangeType):
            expr = to_subrange(expr, typ, typ_env)
        if len(f'{desig} = {expr};') > 80:
            self.pp(f'{desig} =')
            self.pp(f'  {expr};')
        else:
            self.pp(f'{desig} = {expr};')

    def gen_For(self, node):
        def nestfor(i):
            q = node.quantifiers[i]
            lo, hi = self.lo_hi_strs(q.rangetype, self.local_env)
            start = f'int _{q.id} = {lo};'
            end = f'_{q.id} <= {hi};'
            incr = f'_{q.id} += 1'
            self.pp(f'for ({start} {end} {incr}) begin')
            self.indent += 1
            if i + 1 < len(node.quantifiers):
                nestfor(i + 1)
            else:
                for statement in node.statements:
                    self.codegen(statement)
            self.indent -= 1
            self.pp('end')
        nestfor(0)

    def gen_While(self, node):
        self.pp('for (int while_i = 0; '
                f'while_i < {self.maxloop}; '
                'while_i += 1) begin')
        self.indent += 1
        self.pp(f'if ({self.codegen(node.condition)}) break;')
        for statement in node.statements:
            self.codegen(statement)
        self.indent -= 1
        self.pp('end')

    def gen_Return(self, node):
        if node.returnval:
            self.codegen(Assignment(IDDes('_return'), node.returnval))
            self.pp('return __return;')
        else:
            self.pp('return;')

    def gen_ProcCall(self, node):
        proc_env = self.local_env.lookup(node.id)
        actuals = []
        for i in range(len(node.actuals)):
            ftype, ftype_env = mueval.typebase(
                proc_env[node.id].formals[i].typeexpr, proc_env)
            atype, atype_env = mueval.exprtypebase(
                node.actuals[i], self.local_env)
            isquant = mueval.isquantifier(node.actuals[i], self.local_env)
            if ftype == atype and not isquant:
                actuals.append(self.codegen(node.actuals[i], False))
            else:
                exprstr = self.codegen(node.actuals[i])
                actuals.append(to_subrange(exprstr, ftype, ftype_env))
        self.pp(f'_{node.id}({", ".join(actuals)});')

    def gen_Error(self, node):
        msg = f'_{str_id(node.message)}: ' if node.message else ''
        self.pp(f'{msg}assert(0);')

    def gen_Assert(self, node):
        msg = f'_{str_id(node.message)}: ' if node.message else ''
        self.pp(f'{msg}assert({self.codegen(node.condition)});')

    def gen_Clear(self, node):
        des = node.designator
        designator = self.codegen(des, False)
        typ, typ_env = mueval.exprtypebase(des, self.local_env)
        if isinstance(typ, SubrangeType):
            self.pp(f'{designator} = 1;')
        elif isinstance(typ, EnumType):
            self.pp(f'{designator} = type({designator})\'(1);')
        elif isinstance(typ, ArrayType):
            lo, hi = self.lo_hi_strs(typ.rangetype, typ_env)
            clear_quant = Quantifier('_clear_i', typ.rangetype)
            clear_for = For([clear_quant],
                [Clear(ArrDes(des, IDDes('_clear_i')))])
            self.local_env[clear_for] = mueval.Env(clear_for, self.local_env)
            self.local_env[clear_for]['_clear_i'] = clear_quant
            self.codegen(clear_for)
        elif isinstance(typ, RecordType):
            for field in typ.fields:
                self.codegen(Clear(RecDes(des, field.id)))

    def gen_Undefine(self, node):
        des = node.designator
        designator = self.codegen(des, False)
        typ, typ_env = mueval.exprtypebase(des, self.local_env)
        if isinstance(typ, EnumType):
            self.pp(f'{designator} = type({designator})\'(0);')
        else:
            self.pp(f'{designator} = 0;')

    def gen_If(self, node):
        cond = self.codegen(node.condition)
        self.pp(f'if ({cond}) begin')
        self.indent += 1
        for statement in node.statements:
            self.codegen(statement)
        self.indent -= 1
        self.pp(f'end', 0)
        for els in node.elses:
            if els:
                self.codegen(els)
        self.pp()

    def gen_Elsif(self, node):
        cond = self.codegen(node.condition)
        self.pp(f' else if ({cond}) begin', 1, 0)
        self.indent += 1
        for statement in node.statements:
            self.codegen(statement)
        self.indent -= 1
        self.pp(f'end', 0)

    def gen_Else(self, node):
        self.pp(f' else begin', 1, 0)
        self.indent += 1
        for statement in node.statements:
            self.codegen(statement)
        self.indent -= 1
        self.pp(f'end', 0)

    def gen_Switch(self, node):
        self.pp(f'case ({self.codegen(node.switcher)})')
        self.indent += 1
        for case in node.cases:
            self.codegen(case)
        if node.elsecase:
            self.pp(f'default: begin')
            self.indent += 1
            for statement in node.elsecase.statements:
                self.codegen(statement)
            self.indent -= 1
            self.pp(f'end')
        self.indent -= 1
        self.pp(f'endcase')

    def gen_Case(self, node):
        matches = ', '.join(self.codegen(m) for m in node.matches)
        self.pp(f'{matches}: begin')
        self.indent += 1
        for statement in node.statements:
            self.codegen(statement)
        self.indent -= 1
        self.pp(f'end')

    def gen_AliasStmt(self, node):
        self.pp(f'begin')
        self.indent += 1
        for alias in node.aliases:
            self.codegen(alias)
        for statement in node.statements:
            self.codegen(statement)
        self.undef_aliases()
        self.indent -= 1
        self.pp(f'end')

# =============================================================================
# TYPE EXPRESSION
# =============================================================================

    def gen_TypeID(self, node):
        return f'_{node.id}'

    def gen_SubrangeType(self, node):
        lo, hi = self.lo_hi_strs(node, self.local_env)
        return f'`subrange({lo}, {hi})'

    def gen_EnumType(self, node):
        env = mueval.nonrec_env(self.local_env)
        i = 0
        eundef = f'_e{i}undef'
        while env.lookup(eundef):
            i += 1
            eundef = f'_e{i}undef'
        env[eundef] = 0
        enums = [f'{"  "*self.indent}  _{eundef}']
        enums += [f'{"  "*self.indent}  _{e}' for e in node.enums]
        enums = ',\n'.join(enums)
        if len(enums) < 120:
            enums = re.sub(r'\n *', ' ', enums)
        return (f'enum bit[$clog2({len(node.enums)}):0] ''{\n'
                f'{enums}\n{"  "*self.indent}''}')

    def gen_ArrayType(self, node):
        lo, hi = self.lo_hi_strs(node.rangetype, self.local_env)
        typ, dims = node, []
        while isinstance(typ, ArrayType):
            lo, hi = self.lo_hi_strs(typ.rangetype, self.local_env)
            dims.append(f'[{lo}:{hi}]')
            typ = typ.itemtype
        dims = ''.join(dims)
        if isinstance(typ, SubrangeType):
            lo, hi = self.lo_hi_strs(typ, self.local_env)
            return f'`subrange_arr({dims}, {lo}, {hi})'
        return f'{self.codegen(typ)} {dims}'

    def gen_RecordType(self, node):
        self.indent += 1
        fields = '\n'.join(
            f'{"  "*self.indent}{self.codegen(f)}'
            for f in node.fields)
        self.indent -= 1
        return f'struct packed {{\n{fields}\n{"  "*self.indent}}}'

# =============================================================================
# EXPRESSION
# =============================================================================

    def gen_C(self, node):
        return str(node.value)

    def gen_Designator(self, node, v=True):
        res = ''
        if isinstance(node, IDDes):
            res = f'_{node.id}'
        elif isinstance(node, ArrDes):
            res = (f'{self.codegen(node.array, False)}'
                   f'[{self.codegen(node.index)}]')
        elif isinstance(node, RecDes):
            res = f'{self.codegen(node.parent, False)}._{node.child}'
        if res in self.aliases:
            res = f'`{res}'
        typ, typ_env = mueval.exprtypebase(node, self.local_env)
        isquant = mueval.isquantifier(node, self.local_env)
        if v and not isquant and isinstance(typ, SubrangeType):
            return from_subrange(res, typ, typ_env)
        return res

    def gen_UnOp(self, node):
        expr = self.codegen(node.expr)
        if sv_precedence(node.expr) <= sv_precedence(node):
            expr = f'({expr})'
        return f'{node.op}{expr}'

    def gen_BinOp(self, node):
        expr1 = self.codegen(node.expr1)
        expr2 = self.codegen(node.expr2)
        if sv_precedence(node.expr1) < sv_precedence(node):
            expr1 = f'({expr1})'
        if sv_precedence(node.expr2) < sv_precedence(node):
            expr2 =  f'({expr2})'
        op = node.op
        if op in {'|', '&', '='}:
            op += op
        if len(expr1 + op + expr2) > 60:
            return f'{expr1} {op}\n{"  "*(self.indent+2)}{expr2}'
        return f'{expr1} {op} {expr2}'

    def gen_CondOp(self, node):
        condition = self.codegen(node.condition)
        consequent = self.codegen(node.consequent)
        alternative = self.codegen(node.alternative)
        if sv_precedence(node.condition) <= sv_precedence(node):
            condition = f'({condition})'
        if sv_precedence(node.consequent) <= sv_precedence(node):
            consequent = f'({consequent})'
        if sv_precedence(node.alternative) <= sv_precedence(node):
            alternative = f'({alternative})'
        return f'{condition} ? {consequent} : {alternative}'

    def gen_FuncCall(self, node, v=True):
        func_env = self.local_env.lookup(node.id)
        actuals = []
        for i in range(len(node.actuals)):
            ftype, ftype_env = mueval.typebase(
                func_env[node.id].formals[i].typeexpr, func_env)
            atype, atype_env = mueval.exprtypebase(
                node.actuals[i], self.local_env)
            isquant = mueval.isquantifier(node.actuals[i], self.local_env)
            if ftype == atype and not isquant:
                actuals.append(self.codegen(node.actuals[i], False))
            else:
                exprstr = self.codegen(node.actuals[i])
                actuals.append(to_subrange(exprstr, ftype, ftype_env))
        res = f'_{node.id}({", ".join(actuals)})'
        typ, typ_env = mueval.exprtypebase(node, self.local_env)
        if v and isinstance(typ, SubrangeType):
            return from_subrange(res, typ, typ_env)
        return res

    def gen_IsUndefined(self, node):
        return f'({self.codegen(node.designator, False)} == 0)'

    def gen_QuantExpr(self, node):
        initial = '1' if isinstance(node, ForAll) else '0'
        junctive = '&&' if isinstance(node, ForAll) else '||'
        i = 0
        quantvar = f'_quantvar{i}'
        while self.local_env.lookup(quantvar):
            i += 1
            quantvar = f'_quantvar{i}'
        self.local_env[quantvar] = quantvar
        buffer = self.output_buffer.getvalue()
        block_begin = find_block_begin(buffer)
        self.output_buffer.seek(block_begin)
        self.pp(f'automatic bit _{quantvar} = {initial};')
        self.output_buffer.write(buffer[block_begin:])
        def nestfor(i):
            q = node.quantifiers[i]
            lo, hi = self.lo_hi_strs(q.rangetype, self.local_env)
            start = f'int _{q.id} = {lo};'
            end = f'_{q.id} <= {hi};'
            incr = f'_{q.id} += 1'
            self.pp(f'for ({start} {end} {incr}) begin')
            self.indent += 1
            if i + 1 < len(node.quantifiers):
                nestfor(i + 1)
            else:
                condition = self.codegen(node.condition)
                self.pp(f'_{quantvar} = _{quantvar} {junctive} (')
                self.pp(f'  {condition});')
            self.indent -= 1
            self.pp('end')
        nestfor(0)
        return f'_{quantvar}'

# =============================================================================
# HELPER FUNCTIONS
# =============================================================================

def str_id(string):
    return re.sub(r'\W', '_', string)

def to_subrange(exprstr, rangetype, rangeenv):
    offset = -mueval.mueval(rangetype.low, rangeenv) + 1
    if offset > 0:
        return f'(int\'({exprstr})+{offset})'
    elif offset < 0:
        return f'(int\'({exprstr})-{-offset})'
    return f'int\'({exprstr})'

def from_subrange(exprstr, rangetype, rangeenv):
    offset = mueval.mueval(rangetype.low, rangeenv) - 1
    if offset > 0:
        return f'({exprstr})+{offset}'
    elif offset < 0:
        return f'({exprstr})-{-offset}'
    return exprstr

def sv_precedence(node):
    prec_map = {
        '->': 0, '?:': 1, '|': 2, '&': 3,
        '=': 4, '!=': 4, '<': 5, '<=': 5, '>': 5, '>=': 5, 
        '+': 6, '-': 6, '*': 7, '/': 7, '%': 7,
        'un !': 8, 'un -': 8, 'un +': 8
    }
    if isinstance(node, CondOp):
        return 1
    elif isinstance(node, UnOp):
        return 8
    elif isinstance(node, BinOp):
        return prec_map[node.op]
    elif isinstance(node, Expression):
        return float('inf')
    return -1

def find_block_begin(buffer):
    start_loc = re.search(r'\smodule\s(?s:.+?);', buffer).end()
    begin_stack = [('module', start_loc)]
    ends_map = {'end': 'begin', 'endfunction': 'function'}
    ends_pat = r'\W(end|begin|endfunction|function)\W'
    ends_loc = start_loc
    ends_match = re.search(ends_pat, buffer[ends_loc:])
    while ends_match:
        ends_loc += ends_match.end(1)
        ends_str = ends_match.group(1)
        if ends_str in ends_map.values():
            begin_stack.append((ends_loc, ends_str))
        if begin_stack[-1][1] == ends_map.get(ends_str, None):
            begin_stack.pop()
        ends_match = re.search(ends_pat, buffer[ends_loc:])
    if begin_stack[-1][1] in {'module', 'function'}:
        return buffer.find(';', ends_loc) + 2
    return ends_loc + 1
