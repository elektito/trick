from enum import Enum
from machinetypes import Symbol
from serialization import Serializable


class SymbolKind(Enum):
    SPECIAL = 1
    AUX = 2
    PRIMCALL = 3
    LOCAL = 4
    DEFINED_NORMAL = 5
    DEFINED_UNHYGIENIC_MACRO = 6
    DEFINED_MACRO = 7
    LOCAL_MACRO = 8
    FREE = 9


class AuxKeywords(Serializable, Enum):
    UNQUOTE = 'unquote'
    UNQUOTE_SPLICING = 'unquote-splicing'
    ELSE = 'else'
    ARROW = '=>'
    UNDERSCORE = '_'
    ELLIPSIS = '...'

    def dump(self, output):
        idx = list(AuxKeywords).index(self)
        self._dump_uint4(idx, output)

    @classmethod
    def load(cls, input):
        idx = cls._load_uint4(input)
        return list(AuxKeywords)[idx]


class SpecialForms(Serializable, Enum):
    DEFINE = 'define'
    DEFINE_MACRO = 'define-macro'
    DEFINE_LIBRARY = 'define-library'
    DEFINE_SYNTAX = 'define-syntax'
    BEGIN = 'begin'
    SET = 'set!'
    IF = 'if'
    LAMBDA = 'lambda'
    LET = 'let'
    LETREC = 'letrec'
    LET_SYNTAX = 'let-syntax'
    LETREC_SYNTAX = 'letrec-syntax'
    QUOTE = 'quote'
    INCLUDE = 'include'
    INCLUDE_CI = 'include-ci'
    COND_EXPAND = 'cond-expand'
    SYNTAX_RULES = 'syntax-rules'

    def dump(self, output):
        idx = list(SpecialForms).index(self)
        self._dump_uint4(idx, output)

    @classmethod
    def load(cls, input):
        idx = cls._load_uint4(input)
        return list(SpecialForms)[idx]


class SymbolInfo:
    def __init__(self, symbol: Symbol, kind: SymbolKind, *,
                 primcall_nargs=None,
                 primcall_code=None,
                 local_frame_idx=None,
                 local_var_idx=None,
                 special_type=None,
                 aux_type=None,
                 transformer=None,
                 library_name=None,
                 immutable=False):
        assert isinstance(symbol, Symbol)
        match kind:
            case SymbolKind.PRIMCALL:
                assert primcall_nargs is not None
                assert primcall_code is not None
            case SymbolKind.LOCAL:
                assert local_frame_idx is not None
                assert local_var_idx is not None
            case SymbolKind.SPECIAL:
                assert special_type is not None
            case SymbolKind.AUX:
                assert aux_type is not None
            case SymbolKind.DEFINED_MACRO:
                assert transformer is not None
            case SymbolKind.LOCAL_MACRO:
                assert transformer is not None

        self.symbol = symbol
        self.kind = kind
        self.primcall_nargs = primcall_nargs
        self.primcall_code = primcall_code
        self.local_frame_idx = local_frame_idx
        self.local_var_idx = local_var_idx
        self.special_type = special_type
        self.aux_type = aux_type
        self.transformer = transformer
        self.library_name = library_name
        self.immutable = immutable

    def is_special(self, special_type: SpecialForms) -> bool:
        return self.kind == SymbolKind.SPECIAL and \
            self.special_type == special_type

    def is_aux(self, aux_type: AuxKeywords) -> bool:
        return self.kind == SymbolKind.AUX and \
            self.aux_type == aux_type

    def is_macro(self):
        return self.kind in (SymbolKind.DEFINED_MACRO,
                             SymbolKind.LOCAL_MACRO)

    def __repr__(self):
        return f'<SymbolInfo {self.symbol} kind={self.kind.name}>'
