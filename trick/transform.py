from .exceptions import CompileError
from .machinetypes import Bool, Bytevector, Char, Integer, List, Nil, Pair, String, Symbol, TrickType, Vector
from .serialization import Serializable
from .symbolinfo import AuxKeywords, SpecialForms, SymbolKind


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


class TransformError(CompileError):
    pass


class Transformer(Serializable):
    def transform(self, expr, env):
        raise NotImplementedError

    @staticmethod
    def _get_serializable_subclasses():
        return [
            SyntaxRulesTransformer,
        ]


class SyntaxRulesTransformer(Transformer):
    serialization_id = 1

    def __init__(self, definition, env, *, _loading=False):
        if _loading:
            return

        self.definition = definition
        self.env = env
        self.compile()

    def _dump(self, output):
        # the only things that we need to serialize are the compiled patterns
        # and templates. We won't need the custom ellipsis and the literals,
        # because they will be compiled into the patterns and templates. env is
        # also not needed anymore. it's only used when expanding, and whoever
        # loads the transformer should associated with the library's top-level
        # environment (local transformers cannot be serialized).
        self._dump_uint4(len(self.rules), output)
        for p, t in self.rules:
            p.dump(output)
            t.dump(output)

    @classmethod
    def _load(cls, input):
        nrules = cls._load_uint4(input)
        rules = [
            (Matcher.load(input), Template.load(input))
            for _ in range(nrules)
        ]
        t = SyntaxRulesTransformer(None, None, _loading=True)
        t.definition = 'unavailable'
        t.env = 'must-be-set-later-to-lib-toplevel-env'
        t.rules = rules
        return t

    def transform(self, expr, env):
        # use a new gensyms table on each expansion
        self.gensyms = {}
        for pattern, template in self.rules:
            result, vars = pattern.match(expr, env)
            if result:
                break
        else:
            raise TransformError(
                f'No transform rule matched expression: {expr}',
                form=expr)

        expanded = template.expand(vars)

        # convert _UserSymbol and _MacroSymbol to appropriate symbols. Expand
        # splices in-place.
        finalized = self.fix_up(expanded, env)

        if isinstance(finalized, _List):
            finalized = finalized.to_trick_list(recursive=True)
        elif isinstance(finalized, _Vector):
            finalized = finalized.to_trick_vector(recursive=True)

        self.check_syntax_error(finalized)

        return finalized

    def check_syntax_error(self, form):
        if not isinstance(form, Pair):
            return
        if not isinstance(form.car, Symbol):
            return
        if form.car.info is None:
            return
        if not form.car.info.is_special(SpecialForms.SYNTAX_ERROR):
            return

        if not form.is_proper():
            raise TransformError(f'Invalid syntax-error form: {form}')

        error = ' '.join(str(i) for i in form.cdr)
        raise TransformError(f'Syntax Error: {error}')


    def fix_up(self, expansion, env):
        if isinstance(expansion, _UserSymbol):
            return expansion.symbol
        elif isinstance(expansion, _MacroSymbol):
            gensym = self.gensyms.get(expansion.symbol)
            if gensym is None:
                gensym = Symbol.gensym(
                    expansion.symbol,
                    no_shortname_prefix=True)
                self.gensyms[expansion.symbol] = gensym

            if expansion.symbol.original:
                gensym.original = expansion.symbol.original
            else:
                gensym.original = expansion.symbol
            gensym.info = self.env.lookup_symbol(expansion.symbol)
            gensym.src_start = expansion.symbol.src_start
            gensym.src_end = expansion.symbol.src_end
            gensym.transform_env = self.env
            return gensym
        elif isinstance(expansion, _Vector):
            items = []
            for i in expansion.items:
                if isinstance(i, _SplicedList):
                    # convert _SplicedList to a _List, recurse on that
                    # first to make sure any _SplicedList inside that is
                    # expanded, then add the results to current results.
                    i = _List(i.items, src_start=None, src_end=None)
                    i = self.fix_up(i, env)
                    items.extend(i.proper)
                else:
                    items.append(self.fix_up(i, env))
            return _Vector(
                items=items,
                src_start=expansion.src_start,
                src_end=expansion.src_end)

        elif isinstance(expansion, Vector):
            items = []
            for i in expansion:
                if isinstance(i, _SplicedList):
                    # convert _SplicedList to a _List, recurse on that first to
                    # make sure any _SplicedList inside that is expanded, then
                    # add the results to current results.
                    i = _List(i.items, src_start=None, src_end=None)
                    i = self.fix_up(i, env)
                    items.extend(i.proper)
                else:
                    fixed = self.fix_up(i, env)
                    # If fix_up returns internal transformer objects, convert them
                    if isinstance(fixed, _List):
                        fixed = fixed.to_trick_list(recursive=True)
                    elif isinstance(fixed, _Vector):
                        fixed = fixed.to_trick_vector(recursive=True)
                    items.append(fixed)
            result = Vector(items)
            result.src_start = expansion.src_start
            result.src_end = expansion.src_end
            return result
        elif isinstance(expansion, _List):
            proper_items = []
            for i in expansion.proper:
                if isinstance(i, _SplicedList):
                    # convert _SplicedList to a _List, recurse on that first to
                    # make sure any _SplicedList inside that is expanded, then
                    # add the results to current results.
                    if any(isinstance(j, list) for j in i.items):
                        raise TransformError('Variable not fully expanded')
                    i = _List(i.items, src_start=None, src_end=None)
                    i = self.fix_up(i, env)
                    proper_items.extend(i.proper)
                else:
                    proper_items.append(self.fix_up(i, env))

            tail = None
            if expansion.tail is not None:
                tail = self.fix_up(expansion.tail, env)

            return _List(
                proper=proper_items,
                tail=tail,
                src_start=expansion.src_start,
                src_end=expansion.src_end,
            )
        elif isinstance(expansion, list):
            raise TransformError('Variable not fully expanded')
        else:
            return expansion

    def compile(self):
        # (transform literals *rules)
        # OR
        # (transform ellipsis-symbol literals *rules)

        d = self.definition

        if len(d) < 2:
            raise TransformError('Too few arguments for transformer')

        self.custom_ellipsis = None
        if isinstance(d[1], Symbol):
            self.custom_ellipsis = d[1]
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

            # the first item in the top-level pattern list is ignored (since
            # it's the place for the macro keyword)
            pattern.proper[0] = MatchAll()

            variables = pattern.get_vars()

            self.check_for_duplicate_variables(variables)
            template = self.compile_template(template, variables)
            self.rules.append((pattern, template))

    def compile_template(self, template, variables, no_ellipses=False):
        if isinstance(template, List):
            proper, tail = template.split_improper_tail()

            # (... sub-template) form allows having a template in which ellipsis
            # is just a literal symbol
            if tail is None and len(proper) == 2 and self.is_ellipsis(proper[0]):
                return self.compile_template(
                    proper[1], variables, no_ellipses=True)

            compiled_proper = []
            for i in range(len(proper)):
                if self.is_ellipsis(proper[i]) and not no_ellipses:
                    if len(compiled_proper) == 0:
                        raise TransformError(
                            f'Lone ellipsis in transform template: {template}',
                            form=proper[i])
                    compiled_proper[-1] = RepeatedTemplate(
                        compiled_proper[-1])
                else:
                    compiled_proper.append(
                        self.compile_template(
                            proper[i], variables,
                            no_ellipses=no_ellipses))

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
            if not no_ellipses and self.is_ellipsis(template):
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

    def is_ellipsis(self, sym):
        if self.custom_ellipsis is not None:
            return sym == self.custom_ellipsis
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


class Matcher(Serializable):
    def get_vars(self):
        return []

    @staticmethod
    def _get_serializable_subclasses():
        return [
            MatchAll,
            MatchLiteral,
            MatchVariable,
            MatchList,
            MatchListWithEllipsis,
            MatchVector,
            MatchVectorWithEllipsis,
            MatchRepeated,
            MatchConstant,
        ]


class MatchAll(Matcher):
    serialization_id = 1

    def match(self, value, env):
        return True, {}

    def __repr__(self):
        return '<MatchAll>'

    def _dump(self, output):
        # no state to serialize
        pass

    @classmethod
    def _load(cls, input):
        return MatchAll()


class MatchLiteral(Matcher):
    serialization_id = 2

    def __init__(self, literal: Symbol):
        self.literal = literal

    def match(self, value, env):
        if not isinstance(value, Symbol):
            return False, {}

        # if the symbol is bound in the current context, it won't match.
        # e.g. (let ((else #f)) (cond (else 10))) will not result in 10,
        # but in #<void>.
        if env.lookup_symbol(value).kind in (SymbolKind.LOCAL, SymbolKind.LOCAL_MACRO):
            return False, {}

        return value.short_name == self.literal.short_name, {}

    def __repr__(self):
        return f'<MatchLiteral {self.literal}>'

    def _dump(self, output):
        self._dump_string(self.literal.name, output)

    @classmethod
    def _load(cls, input):
        literal = Symbol(cls._load_string(input))
        return MatchLiteral(literal)


class MatchVariable(Matcher):
    serialization_id = 3

    def __init__(self, variable: Symbol):
        self.variable = variable

    def match(self, value, env):
        return True, {self.variable: value}

    def get_vars(self):
        return [self.variable]

    def __repr__(self):
        return f'<MatchVariable {self.variable}>'

    def _dump(self, output):
        self._dump_string(self.variable.name, output)

    @classmethod
    def _load(cls, input):
        var = Symbol(cls._load_string(input))
        return MatchVariable(var)


class MatchList(Matcher):
    serialization_id = 4

    def __init__(self, proper, tail):
        assert is_valid_matcher(proper)
        assert tail is None or is_valid_matcher(tail)

        self.proper = proper
        self.tail = tail

    def match(self, value, env):
        if not isinstance(value, List):
            return False, {}

        vars = {}
        cur = value
        i = 0
        while isinstance(cur, Pair) and i < len(self.proper):
            result, p_vars = self.proper[i].match(cur.car, env)
            if not result:
                return False, {}
            merge_vars(vars, p_vars)
            i += 1
            cur = cur.cdr

        if i != len(self.proper):
            return False, {}

        if self.tail is None:
            result = isinstance(cur, Nil)
        else:
            result, p_vars  = self.tail.match(cur, env)
            merge_vars(vars, p_vars)

        return result, vars if result else {}

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

    def _dump(self, output):
        self._dump_list(self.proper, output)
        self._dump_optional(self.tail, output)

    @classmethod
    def _load(cls, input):
        proper = cls._load_list(Matcher, input)
        tail = cls._load_optional(Matcher, input)
        return MatchList(proper, tail)


class MatchListWithEllipsis(Matcher):
    serialization_id = 5

    def __init__(self, proper, tail, repeated_idx):
        assert is_valid_matcher(proper)
        assert tail is None or is_valid_matcher(tail)

        self.proper = proper
        self.tail = tail
        self.repeated_idx = repeated_idx

        # number of patterns before the pattern with ellipsis
        self.n_before = repeated_idx

        # number of patterns after ellipsis
        self.n_after = len(proper) - repeated_idx - 1

    def match(self, value, env):
        if not isinstance(value, List):
            return False, {}

        vars = {v: [] for v in self.get_vars()}
        value_proper, value_tail = value.split_improper_tail()

        if len(value_proper) < self.n_before + self.n_after:
            return False, {}

        if (value_tail is not None and self.tail is None) or \
           (value_tail is None and self.tail is not None):
            return False, {}

        i = 0
        for j, v in enumerate(value_proper):
            result, p_vars = self.proper[i].match(v, env)
            if not result:
                return False, {}
            merge_vars(vars, p_vars)

            if j < self.n_before and j >= len(value_proper) - self.n_after - 1:
                # since the repeated pattern is being skipped, make sure all
                # variables in it are initialized to an empty list. the python
                # list is used as a value for repeated variables, and in this
                # case, an empty list means no values collected.
                p_vars = self.proper[i+1].get_vars()
                merge_vars(vars, {
                    v: [] for v in p_vars
                })

                # empty match on repeated. proceed to the next pattern.
                i += 2
            elif j < self.n_before or j >= len(value_proper) - self.n_after - 1:
                i += 1

        if self.tail is not None:
            result, tail_vars = self.tail.match(value_tail, env)
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

    def _dump(self, output):
        self._dump_list(self.proper, output)
        self._dump_optional(self.tail, output)
        self._dump_uint4(self.repeated_idx, output)

    @classmethod
    def _load(cls, input):
        proper = cls._load_list(Matcher, input)
        tail = cls._load_optional(Matcher, input)
        repeated_idx = cls._load_uint4(input)
        return MatchListWithEllipsis(proper, tail, repeated_idx)


class MatchVector(Matcher):
    serialization_id = 6

    def __init__(self, items):
        assert is_valid_matcher(items)

        self.items = items

    def match(self, value, env):
        if not isinstance(value, Vector):
            return False, {}

        vars = {}
        for p, v in zip(self.items, value):
            result, p_vars = p.match(v, env)
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

    def _dump(self, output):
        self._dump_list(self.items, output)

    @classmethod
    def _load(cls, input):
        return MatchVector(cls._load_list(Matcher, input))


class MatchVectorWithEllipsis(Matcher):
    serialization_id = 7

    def __init__(self, items, repeated_idx):
        assert is_valid_matcher(items)

        self.items = items
        self.repeated_idx = repeated_idx

        # number of patterns before the pattern with ellipsis
        self.n_before = repeated_idx - 1

        # number of patterns after ellipsis
        self.n_after = len(self.items) - repeated_idx

    def match(self, value, env):
        if not isinstance(value, Vector):
            return False, {}

        vars = {}
        if len(value) < self.n_before + self.n_after:
            return False, {}

        i = 0
        for j, v in enumerate(value):
            result, p_vars = self.items[i].match(v, env)
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

    def _dump(self, output):
        self._dump_list(self.items, output)
        self._dump_uint4(self.repeated_idx, output)

    @classmethod
    def _load(cls, input):
        items = cls._load_list(Matcher, input)
        repeated_idx = cls._load_uint4(input)
        return MatchVectorWithEllipsis(items, repeated_idx)


class MatchRepeated(Matcher):
    serialization_id = 8

    def __init__(self, matcher):
        assert is_valid_matcher(matcher)
        self.matcher = matcher

    def match(self, value, env):
        result, vars = self.matcher.match(value, env)
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

    def _dump(self, output):
        self.matcher.dump(output)

    @classmethod
    def _load(cls, input):
        return MatchRepeated(Matcher.load(input))


class MatchConstant(Matcher):
    serialization_id = 9

    def __init__(self, constant):
        assert isinstance(constant, TrickType) and \
            not isinstance(constant, (List, Vector))
        self.constant = constant

    def match(self, value, env):
        return equal(value, self.constant), {}

    def __repr__(self):
        return f'<MatchConstant {self.constant}>'

    def _dump(self, output):
        self.constant.dump(output)

    @classmethod
    def _load(cls, input):
        return MatchConstant(TrickType.load(input))


class Template(Serializable):
    def expand(self, vars: dict):
        raise NotImplementedError

    def get_vars(self) -> list[Symbol]:
        raise NotImplementedError

    @staticmethod
    def _get_serializable_subclasses():
        return [
            ConstantTemplate,
            VariableTemplate,
            SymbolTemplate,
            ListTemplate,
            VectorTemplate,
            RepeatedTemplate,
        ]

class ConstantTemplate(Template):
    serialization_id = 1

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

    def _dump(self, output):
        self.constant.dump(output)

    @classmethod
    def _load(cls, input):
        return ConstantTemplate(TrickType.load(input))


class VariableTemplate(Template):
    serialization_id = 2

    def __init__(self, variable: Symbol):
        assert isinstance(variable, Symbol)
        self.variable = variable

    def expand(self, vars):
        value = vars[self.variable]
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

    def _dump(self, output):
        self.variable.dump(output)

    @classmethod
    def _load(cls, input):
        var = Symbol.load(input)
        return VariableTemplate(var)


class SymbolTemplate(Template):
    serialization_id = 3

    def __init__(self, symbol: Symbol):
        assert isinstance(symbol, Symbol)
        self.symbol = symbol

    def expand(self, vars):
        return _MacroSymbol(self.symbol)

    def get_vars(self):
        return []

    def __repr__(self):
        return f'<SymbolTemplate {self.symbol}>'

    def _dump(self, output):
        self.symbol.dump(output)

    @classmethod
    def _load(cls, input):
        sym = Symbol.load(input)
        return SymbolTemplate(sym)


class ListTemplate(Template):
    serialization_id = 4

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

    def _dump(self, output):
        self._dump_list(self.proper, output)
        self._dump_optional(self.tail, output)

    @classmethod
    def _load(cls, input):
        proper = cls._load_list(Template, input)
        tail = cls._load_optional(Template, input)
        return ListTemplate(proper, tail, src_start=None, src_end=None)


class VectorTemplate(Template):
    serialization_id = 5

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

    def _dump(self, output):
        self._dump_list(self.items, output)

    @classmethod
    def _load(cls, input):
        return VectorTemplate(cls._load_list(Template, input))


class RepeatedTemplate(Template):
    serialization_id = 6

    def __init__(self, template: Template):
        assert isinstance(template, Template)
        self.template = template

    def expand(self, vars):
        repeated_vars = self.find_repeated_vars(self.template, 1, vars)
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

    def find_repeated_vars(self, template, level, vars):
        if isinstance(template, RepeatedTemplate):
            return self.find_repeated_vars(
                template.template, level + 1, vars)
        elif isinstance(template, VariableTemplate):
            value = vars.get(template.variable, [])
            if not isinstance(value, list):
                return []
            depth = self.get_list_depth(value)
            if depth >= level:
                return [template.variable]
            else:
                return []
        elif isinstance(template, SymbolTemplate):
            return []
        elif isinstance(template, ListTemplate):
            return \
                sum((self.find_repeated_vars(t, level, vars)
                     for t in template.proper), []) + \
                self.find_repeated_vars(template.tail, level, vars)
        elif isinstance(template, VectorTemplate):
            return \
                sum((self.find_repeated_vars(t, level, vars)
                     for t in template.items), [])
        else:
            return []

    def get_list_depth(self, l: list):
        if l == []:
            return 1

        depth = 0
        while True:
            if not isinstance(l, list):
                break
            depth += 1
            if l == []:
                break
            l = l[0]

        return depth

    def __repr__(self):
        return f'<RepeatedTemplate {self.template}>'

    def _dump(self, output):
        self.template.dump(output)

    @classmethod
    def _load(cls, input):
        return RepeatedTemplate(cls.load(input))


def merge_vars(old, new):
    for name, value in new.items():
        if isinstance(value, list):
            if name in old:
                old[name] += value
            else:
                old[name] = value
        else:
            assert name not in old or old[name] == []
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
    elif isinstance(v1, Bytevector):
        return v1.bytes == v2.bytes
    elif isinstance(v1, String):
        return v1.value == v2.value
    else:
        return v1 == v2
