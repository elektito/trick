from collections import defaultdict
from library import AuxKeywords, SymbolKind
from machinetypes import Bool, Char, Integer, List, Nil, Pair, String, Symbol, TrickType, Vector


class _List:
    """
    This class is used internally in this module to represent a
    potentially dotted list. It will be converted to a Nil or Pair
    before being returned.
    Unlike the normal Pair class, this class allows some other internal
    types as elements.
    """
    def __init__(self,
                 proper: list['TrickType | _List | _UserSymbol | _MacroSymbol | _SplicedList'],
                 tail: '(None | TrickType | _List | _UserSymbol | _MacroSymbol)' = None,
                 *,
                 src_start,
                 src_end):
        assert isinstance(proper, list)
        assert all(isinstance(i, (TrickType,
                                  _List,
                                  _UserSymbol,
                                  _MacroSymbol,
                                  _SplicedList))
                   for i in proper)
        assert tail is None or isinstance(tail, (TrickType,
                                                 _List,
                                                 _UserSymbol,
                                                 _MacroSymbol))
        assert proper != [] or tail is None

        self.proper = proper
        self.tail = tail
        self.src_start = src_start
        self.src_end = src_end

    def to_trick_list(self, *, recursive=False):
        tail = self.tail
        if recursive and isinstance(tail, _List):
            tail = tail.to_trick_list(recursive=True)

        cur = Nil() if tail is None else tail
        for i in reversed(self.proper):
            if recursive:
                if isinstance(i, _List):
                    i = i.to_trick_list(recursive=True)
                elif isinstance(i, _Vector):
                    i = i.to_trick_vector(recursive=True)
            cur = Pair(i, cur)

        cur.src_start = self.src_start
        cur.src_end = self.src_end

        return cur

    def __repr__(self):
        if self.tail is None:
            return f'_({" ".join(str(i) for i in self.proper)})'
        else:
            return f'_({" ".join(str(i) for i in self.proper)} . {self.tail})'


class _Vector:
    """
    This class is used internally in this module to represent a
    vector. It will be converted to a Vector before being returned.
    Unlike the normal Vector class, this class allows some other internal
    types as elements.
    """
    def __init__(self,
                 items: list['TrickType | _List | _UserSymbol | _MacroSymbol | _SplicedList'],
                 *,
                 src_start,
                 src_end):
        assert isinstance(items, list)
        assert all(isinstance(i, (TrickType,
                                  _List,
                                  _UserSymbol,
                                  _MacroSymbol,
                                  _SplicedList))
                   for i in items)
        self.items = items
        self.src_start = src_start
        self.src_end = src_end

    def to_trick_vector(self, *, recursive=False) -> Vector:
        items = []
        for i in self.items:
            if recursive:
                if isinstance(i, _List):
                    i = i.to_trick_list(recursive=True)
                elif isinstance(i, _Vector):
                    i = i.to_trick_vector(recursive=True)
            items.append(i)

        result = Vector(items)
        result.src_start = self.src_start
        result.src_end = self.src_end
        return result

    def __repr__(self):
        return '_#({" ".join(str(i)) for i in self.items})'


class _SplicedList:
    def __init__(self, items: list):
        self.items = items

    def __repr__(self):
        return f'@({" ".join(str(i) for i in self.items)})'


class _UserSymbol:
    def __init__(self, symbol: Symbol):
        self.symbol = symbol

    def __repr__(self):
        return f'U[{self.symbol}]'


class _MacroSymbol:
    def __init__(self, symbol: Symbol):
        self.symbol = symbol

    def __repr__(self):
        return f'M[{self.symbol}]'


class TransformError(Exception):
    def __init__(self, msg, form=None):
        self.msg = msg
        self.form = form

    def __repr__(self):
        return self.msg


class Transformer:
    def transform(self, expr, env):
        raise NotImplementedError


class SyntaxRulesTransformer(Transformer):
    def __init__(self, definition, env):
        self.definition = definition
        self.env = env
        self.compile()

    def transform(self, expr, env):
        # use a new gensyms table on each expansion
        self.gensyms = {}
        for pattern, template in self.rules:
            result, vars = pattern.match(expr)
            if result:
                break
        else:
            raise TransformError(
                f'No transform rule matched expression: {expr}',
                form=expr)

        expanded = template.expand(vars)
        finalized = self.fix_symbols(expanded)

        if isinstance(finalized, _List):
            finalized = finalized.to_trick_list(recursive=True)
        elif isinstance(finalized, _Vector):
            finalized = finalized.to_trick_vector(recursive=True)

        return finalized

    def fix_symbols(self, expansion):
        if isinstance(expansion, _UserSymbol):
            return expansion.symbol
        elif isinstance(expansion, _MacroSymbol):
            info = self.env.lookup_symbol(expansion.symbol)
            if info.kind == SymbolKind.FREE:
                gensym = self.gensyms.get(expansion.symbol)
                if gensym is None:
                    gensym = Symbol.gensym(String(expansion.symbol.name))
                    self.gensyms[expansion.symbol] = gensym
                gensym.original = expansion.symbol
                return gensym
            else:
                expansion.symbol.env = self.env
                expansion.symbol.original = expansion.symbol
                return expansion.symbol
        elif isinstance(expansion, Vector):
            items = []
            for i in expansion:
                if isinstance(i, _SplicedList):
                    items.extend(
                        [self.fix_symbols(j) for j in i.items])
                else:
                    items.append(self.fix_symbols(i))
            result = Vector(items)
            result.src_start = expansion.src_start
            result.src_end = expansion.src_end
            return result
        elif isinstance(expansion, _List):
            proper_items = []
            for i in expansion.proper:
                if isinstance(i, _SplicedList):
                    proper_items.extend(
                        [self.fix_symbols(j) for j in i.items])
                else:
                    proper_items.append(self.fix_symbols(i))

            tail = None
            if expansion.tail is not None:
                tail = self.fix_symbols(expansion.tail)

            return _List(
                proper=proper_items,
                tail=tail,
                src_start=expansion.src_start,
                src_end=expansion.src_end,
            )
        else:
            return expansion

    def compile(self):
        # (transform literals *rules)
        # OR
        # (transform ellipses literals *rules)

        d = self.definition

        if len(d) < 2:
            raise TransformError('Too few arguments for transformer')

        self.custom_ellipses = None
        if isinstance(d[1], Symbol):
            self.custom_ellipses = d[1]
            if len(d) < 3:
                raise TransformError('Too few arguments for transformer')
            self.literals = d[2]
            rules = d.cdr.cdr.cdr
        else:
            self.literals = d[1]
            rules = d.cdr.cdr

        if not isinstance(self.literals, List):
            raise TransformError(
                f'Transformer literals not a list: {self.literals}',
                form=self.literals)

        for lit in self.literals:
            if not isinstance(lit, Symbol):
                raise TransformError(
                    f'Invalid transformer literal: {lit}', form=lit)

        for r in rules:
            if not isinstance(r, Pair) or \
               not isinstance(r.car, Pair) or \
               len(r) != 2:
                raise TransformError(
                    f'Invalid transform rule: {r}', form=r)

        self.rules = []
        for r in rules:
            pattern, template = r[0], r[1]
            pattern = self.compile_pattern(pattern)
            variables = pattern.get_vars()
            self.check_for_duplicate_variables(variables)
            template = self.compile_template(template, variables)
            self.rules.append((pattern, template))

    def compile_template(self, template, variables, no_ellipses=False):
        if isinstance(template, List):
            proper, tail = template.split_improper_tail()

            if tail is None and len(proper) == 2 and self.is_ellipsis(proper[0]):
                return self.compile_template(
                    proper[1], variables, no_ellipses=True)

            compiled_proper = []
            i = 0
            while i < len(proper):
                if not no_ellipses and \
                   i < len(proper) - 1 and \
                   self.is_ellipsis(proper[i+1]):
                    compiled_proper.append(
                        RepeatedTemplate(
                            self.compile_template(
                                proper[i], variables,
                                no_ellipses=no_ellipses)))
                    i += 2
                else:
                    if self.is_ellipsis(proper[i]) and not no_ellipses:
                        raise TransformError(
                            f'Lone ellipsis in transform template: {template}',
                            form=proper[i])
                    compiled_proper.append(
                        self.compile_template(
                            proper[i], variables,
                            no_ellipses=no_ellipses))
                    i += 1
            compiled_tail = None
            if tail is not None:
                compiled_tail = self.compile_template(
                    tail, variables, no_ellipses=no_ellipses)
            return ListTemplate(
                compiled_proper, compiled_tail,
                src_start=template.src_start,
                src_end=template.src_end)
        elif isinstance(template, Vector):
            items = []
            i = 0
            while i < len(template):
                if not no_ellipses and \
                   i < len(template) - 1 and \
                   self.is_ellipsis(template[i+1]):
                    items.append(
                        RepeatedTemplate(
                            self.compile_template(
                                template[i], variables,
                                no_ellipses=no_ellipses)))
                    i += 2
                else:
                    items.append(self.compile_template(
                        template[i], variables,
                        no_ellipses=no_ellipses))
                    i += 1
            return VectorTemplate(
                items,
                src_start=template.src_start,
                src_end=template.src_end)
        elif isinstance(template, Symbol):
            info = self.env.lookup_symbol(template)
            if info.is_aux(AuxKeywords.ELLIPSIS) and not no_ellipses:
                raise TransformError(
                    f'A single ellipsis as template is not allowed.',
                    form=template)
            if template in variables:
                return VariableTemplate(template)
            else:
                return SymbolTemplate(template)
        else:
            return ConstantTemplate(template)

    def compile_pattern(self, pattern):
        is_underscore = False
        if isinstance(pattern, Symbol):
            info = self.env.lookup_symbol(pattern)
            is_underscore = info.is_aux(AuxKeywords.UNDERSCORE)

        if is_underscore:
            return MatchAll()
        elif pattern in self.literals:
            return MatchLiteral(pattern)
        elif isinstance(pattern, Symbol):
            return MatchVariable(pattern)
        elif isinstance(pattern, List):
            proper, tail = pattern.split_improper_tail()
            if tail is not None and self.is_ellipsis(tail):
                raise TransformError(
                    f'Ellipses not valid in list pattern tail',
                    form=pattern)
            ellipsis_idx = None
            for i, p in enumerate(proper):
                if self.is_ellipsis(p):
                    if ellipsis_idx is not None:
                        raise TransformError(
                            f'Multiple ellipses in pattern: {pattern}',
                            form=pattern)
                    ellipsis_idx = i
            if ellipsis_idx is None:
                return MatchList(
                    [self.compile_pattern(p) for p in proper],
                    self.compile_pattern(tail) if tail is not None else None)
            else:
                return MatchListWithEllipsis(
                    [
                        MatchRepeated(self.compile_pattern(p))
                        if i == ellipsis_idx - 1
                        else self.compile_pattern(p)
                        for i, p in enumerate(proper)
                        if i != ellipsis_idx
                    ],
                    self.compile_pattern(tail) if tail is not None else None,
                    ellipsis_idx - 1)
        elif isinstance(pattern, Vector):
            ellipsis_idx = None
            for i, p in enumerate(pattern):
                if self.is_ellipsis(p):
                    if ellipsis_idx is not None:
                        raise TransformError(
                            f'Multiple ellipses in pattern: {pattern}',
                            form=pattern)
                    ellipsis_idx = i
            if ellipsis_idx is None:
                return MatchVector(
                    [self.compile_pattern(p) for p in pattern])
            else:
                return MatchVectorWithEllipsis(
                    [
                        MatchRepeated(self.compile_pattern(p))
                        if i == ellipsis_idx - 1
                        else self.compile_pattern(p)
                        for i, p in enumerate(pattern)
                        if i != ellipsis_idx
                    ],
                    ellipsis_idx - 1)
        else:
            return MatchConstant(pattern)

    def is_ellipsis(self, sym: Symbol):
        if self.custom_ellipses is not None:
            return sym == self.custom_ellipses
        elif isinstance(sym, Symbol):
            info = self.env.lookup_symbol(sym)
            return info.is_aux(AuxKeywords.ELLIPSIS)
        else:
            return False

    def check_for_duplicate_variables(self, vars):
        seen = set()
        for v in vars:
            if v in seen:
                raise TransformError(
                    f'Duplicate variable: {v}', form=v)
            seen.add(v)


class UninitializedTransformer(Transformer):
    def transform(self, expr, env):
        return TransformError(
            f'Use of uninitialized transformer')


class Matcher:
    def get_vars(self):
        return []


class MatchAll(Matcher):
    def match(self, value):
        return True, {}

    def __repr__(self):
        return '<MatchAll>'


class MatchLiteral(Matcher):
    def __init__(self, literal: Symbol):
        self.literal = literal

    def match(self, value):
        return value == self.literal, {}

    def __repr__(self):
        return f'<MatchLiteral {self.literal}>'


class MatchVariable(Matcher):
    def __init__(self, variable: Symbol):
        self.variable = variable

    def match(self, value):
        return True, {self.variable: value}

    def get_vars(self):
        return [self.variable]

    def __repr__(self):
        return f'<MatchVariable {self.variable}>'


class MatchList(Matcher):
    def __init__(self, proper, tail):
        assert is_valid_matcher(proper)
        assert tail is None or is_valid_matcher(tail)

        self.proper = proper
        self.tail = tail

    def match(self, value):
        if not isinstance(value, List):
            return False, {}

        vars = {}
        value_proper, value_tail = value.split_improper_tail()

        if len(value_proper) != len(self.proper):
            return False, {}

        if (value_tail is not None and self.tail is None) or \
           (value_tail is None and self.tail is not None):
            return False, {}

        for p, v in zip(self.proper, value):
            result, p_vars = p.match(v)
            if not result:
                return False, {}
            merge_vars(vars, p_vars)

        if self.tail is not None:
            result, tail_vars = self.tail.match(value_tail)
            if not result:
                return False, {}
            merge_vars(vars, tail_vars)

        return True, vars

    def get_vars(self):
        vars = []
        for p in self.proper:
            vars += p.get_vars()
        if self.tail:
            vars += self.tail.get_vars()
        return vars

    def __repr__(self):
        if self.tail:
            return f'<MatchList {self.proper} . {self.tail}>'
        else:
            return f'<MatchList {self.proper}>'


class MatchListWithEllipsis(Matcher):
    def __init__(self, proper, tail, repeated_idx):
        assert is_valid_matcher(proper)
        assert tail is None or is_valid_matcher(tail)

        self.proper = proper
        self.tail = tail
        self.repeated_idx = repeated_idx

        # number of patterns before the pattern with ellipsis
        self.n_before = repeated_idx - 1

        # number of patterns after ellipsis
        self.n_after = len(proper) - repeated_idx

    def match(self, value):
        if not isinstance(value, List):
            return False, {}

        vars = {}
        value_proper, value_tail = value.split_improper_tail()

        if len(value_proper) < self.n_before + self.n_after:
            return False, {}

        if (value_tail is not None and self.tail is None) or \
           (value_tail is None and self.tail is not None):
            return False, {}

        i = 0
        for j, v in enumerate(value_proper):
            result, p_vars = self.proper[i].match(v)
            if not result:
                return False, {}
            merge_vars(vars, p_vars)

            if j <= self.n_before and j >= len(value_proper) - self.n_after:
                # empty match on repeated. proceed to the next pattern.
                i += 2
            elif j <= self.n_before or j >= len(value_proper) - self.n_after:
                i += 1

        if self.tail is not None:
            result, tail_vars = self.tail.match(value_tail)
            if not result:
                return False, {}
            merge_vars(vars, tail_vars)

        return True, vars

    def get_vars(self):
        vars = []
        for p in self.proper:
            vars += p.get_vars()
        if self.tail:
            vars += self.tail.get_vars()
        return vars

    def __repr__(self):
        if self.tail:
            return f'<MatchList... {self.proper} . {self.tail}>'
        else:
            return f'<MatchList... {self.proper}>'


class MatchVector(Matcher):
    def __init__(self, items):
        assert is_valid_matcher(items)

        self.items = items

    def match(self, value):
        if not isinstance(value, Vector):
            return False, {}

        vars = {}
        for p, v in zip(self.items, value):
            result, p_vars = p.match(v)
            if not result:
                return False, {}
            merge_vars(vars, p_vars)

        return True, vars

    def get_vars(self):
        vars = []
        for p in self.items:
            vars += p.get_vars()
        return vars

    def __repr__(self):
        return f'<MatchVector {self.items}>'


class MatchVectorWithEllipsis(Matcher):
    def __init__(self, items, repeated_idx):
        assert is_valid_matcher(items)

        self.items = items
        self.repeated_idx = repeated_idx

        # number of patterns before the pattern with ellipsis
        self.n_before = repeated_idx - 1

        # number of patterns after ellipsis
        self.n_after = len(self.items) - repeated_idx

    def match(self, value):
        if not isinstance(value, Vector):
            return False, {}

        vars = {}
        if len(value) < self.n_before + self.n_after:
            return False, {}

        i = 0
        for j, v in enumerate(value):
            result, p_vars = self.items[i].match(v)
            if not result:
                return False, {}
            merge_vars(vars, p_vars)

            if j < self.n_before or j >= len(value) - self.n_after:
                i += 1

        return True, vars

    def get_vars(self):
        vars = []
        for p in self.items:
            vars += p.get_vars()
        return vars

    def __repr__(self):
        return f'<MatchVector... {self.items}>'


class MatchRepeated(Matcher):
    def __init__(self, matcher):
        assert is_valid_matcher(matcher)
        self.matcher = matcher

    def match(self, value):
        result, vars = self.matcher.match(value)
        output_vars = {}
        if result:
            for name, value in vars.items():
                if name in output_vars:
                    output_vars[name].append(value)
                else:
                    output_vars[name] = [value]
        return result, output_vars

    def get_vars(self):
        return self.matcher.get_vars()

    def __repr__(self):
        return f'<MatchRepeated {self.matcher}>'


class MatchConstant(Matcher):
    def __init__(self, constant):
        assert isinstance(constant, TrickType) and \
            not isinstance(constant, (List, Vector))
        self.constant = constant

    def match(self, value):
        return equal(value, self.constant), {}

    def __repr__(self):
        return f'<MatchConstant {self.constant}>'


class Template:
    def expand(self, vars: dict):
        raise NotImplementedError

    def get_vars(self) -> list[Symbol]:
        raise NotImplementedError


class ConstantTemplate(Template):
    def __init__(self, constant):
        assert isinstance(constant, TrickType)
        assert not isinstance(constant, (List, Vector))
        self.constant = constant

    def expand(self, vars):
        return self.constant

    def get_vars(self):
        return []

    def __repr__(self):
        return f'<ConstantTemplate {self.constant}>'


class VariableTemplate(Template):
    def __init__(self, variable: Symbol):
        assert isinstance(variable, Symbol)
        self.variable = variable

    def expand(self, vars):
        value = vars[self.variable]
        if isinstance(value, list):
            raise TransformError(
                f'Expanding repeated variable without ellipsis: {self.variable}',
                form=self.variable)
        return self.convert_to_user_env(value)

    def convert_to_user_env(self, value):
        if isinstance(value, Pair):
            proper, tail = value.split_improper_tail()
            new_value = _List(
                proper=[self.convert_to_user_env(i) for i in proper],
                tail=None if tail is None else self.convert_to_user_env(tail),
                src_start=value.src_start,
                src_end=value.src_end)
            return new_value
        elif isinstance(value, Vector):
            result = Vector([
                self.convert_to_user_env(i) for i in value
            ])
            result.src_start = value.src_start
            result.src_end = value.src_end
            return result
        elif isinstance(value, Symbol):
            return _UserSymbol(value)
        else:
            return value

    def get_vars(self):
        return [self.variable]

    def __repr__(self):
        return f'<VariableTemplate {self.variable}>'


class SymbolTemplate(Template):
    def __init__(self, symbol: Symbol):
        assert isinstance(symbol, Symbol)
        self.symbol = symbol

    def expand(self, vars):
        return _MacroSymbol(self.symbol)

    def get_vars(self):
        return []

    def __repr__(self):
        return f'<SymbolTemplate {self.symbol}>'


class ListTemplate(Template):
    def __init__(self,
                 proper: list[Template],
                 tail: (None | Template),
                 *,
                 src_start,
                 src_end):
        assert is_valid_template(proper)
        assert tail is None or is_valid_template(tail)
        self.proper = proper
        self.tail = tail
        self.src_start = src_start
        self.src_end = src_end

    def expand(self, vars):
        return _List(
            proper=[i.expand(vars) for i in self.proper],
            tail=None if self.tail is None else self.tail.expand(vars),
            src_start=self.src_start,
            src_end=self.src_end,
        )

    def get_vars(self):
        vars = sum((i.get_vars() for i in self.proper), [])
        if self.tail is not None:
            vars += self.tail.get_vars()
        return vars

    def __repr__(self):
        if self.tail is None:
            return f'<ListTemplate {self.proper}>'
        else:
            return f'<ListTemplate {self.proper} . {self.tail}>'


class VectorTemplate(Template):
    def __init__(self, items: list[Template], *, src_start=None, src_end=None):
        assert is_valid_template(items)
        self.items = items
        self.src_start = src_start
        self.src_end = src_end

    def expand(self, vars):
        result = Vector(i.expand(vars) for i in self.items)
        result.src_start = self.src_start
        result.src_end = self.src_end
        return result

    def get_vars(self):
        return sum((i.get_vars() for i in self.items), [])

    def __repr__(self):
        return f'<VectorTemplate {self.items}>'


class RepeatedTemplate(Template):
    def __init__(self, template: Template):
        assert isinstance(template, Template)
        self.template = template

    def expand(self, vars):
        # find all variables used in base template
        used_vars = self.template.get_vars()

        # of those, find the ones that are lists, that is repeated. notice that
        # any missing variables are repeated, because only repeated patterns can
        # never be matched (zero repetitions).
        repeated_vars = [
            i for i in used_vars
            if isinstance(vars.get(i, []), list)]
        non_repeated_vars = [i for i in vars if i not in repeated_vars]

        # make sure there's at least one repeated variable
        if repeated_vars == []:
            raise TransformError(
                'Ellipsis used without any repeated variables.')

        # make sure all list variables are of the same size
        sizes = {len(vars.get(v, [])) for v in repeated_vars}
        if len(sizes) != 1:
            raise TransformError(
                f'Lists supposed to be expanded together are not of '
                f'the same size: {", ".join(v.name for v in repeated_vars)}')

        # expand now
        expanded = []
        for values in zip(*[vars.get(v, []) for v in repeated_vars]):
            # values is now a list with a value for each repeated variable.
            # convert that to a dictionary.
            new_vars = {
                var_name: value
                for var_name, value in zip(repeated_vars, values)
            }

            # add the non-repeated values
            merge_vars(new_vars,
                       {v: vars[v] for v in non_repeated_vars})

            # now expand using this new set of variables
            expanded.append(self.template.expand(new_vars))

        return _SplicedList(expanded)

    def get_vars(self):
        return self.template.get_vars()

    def __repr__(self):
        return f'<RepeatedTemplate {self.template}>'


def merge_vars(old, new):
    for name, value in new.items():
        if isinstance(value, list):
            if name in old:
                old[name] += value
            else:
                old[name] = value
        else:
            assert name not in old
            old[name] = value


def is_valid_matcher(m):
    if isinstance(m, Matcher):
        return True
    elif isinstance(m, list):
        return all(is_valid_matcher(i) for i in m)
    else:
        return False


def is_valid_template(t):
    if isinstance(t, Template):
        return True
    elif isinstance(t, list):
        return all(is_valid_template(i) for i in t)
    else:
        return False


def equal(v1, v2):
    if type(v1) != type(v2):
        return False

    if isinstance(v1, (Integer, Symbol, Bool, Char)):
        return v1 == v2
    elif isinstance(v1, String):
        return v1.value == v2.value
    else:
        return v1 == v2
