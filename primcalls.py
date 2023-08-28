from features import get_features
from machinetypes import Symbol


S = Symbol


def asm_code_for_features():
    features = get_features()

    code = [S('nil')]
    for feature in reversed(features):
        code += [S('ldsym'), S(feature), S('cons')]

    return code


primcalls = {
    'apply': {
        'nargs': 2,
        'code': [S('ap')],
        'exported': False,
    },
    'void': {
        'nargs': 0,
        'code': [S('void')],
        'exported': False,
    },
    'call/cc': {
        'nargs': 1,
        'code': [S('ccc')],
        'exported': False,
    },
    'values->list': {
        'nargs': 1,
        'code': [S('m2l')],
        'exported': False,
    },
    'list->values': {
        'nargs': 1,
        'code': [S('l2m')],
        'exported': False,
    },
    'iadd': {
        'nargs': 2,
        'code': [S('iadd')],
        'exported': False,
    },
    'isub': {
        'nargs': 2,
        'code': [S('isub')],
        'exported': False,
    },
    'imul': {
        'nargs': 2,
        'code': [S('imul')],
        'exported': False,
    },
    'idiv': {
        'nargs': 2,
        'code': [S('idiv')],
        'exported': False,
    },
    'irem': {
        'nargs': 2,
        'code': [S('irem')],
        'exported': False,
    },
    'ilt': {
        'nargs': 2,
        'code': [S('ilt')],
        'exported': False,
    },
    'ile': {
        'nargs': 2,
        'code': [S('ile')],
        'exported': False,
    },
    'shr': {
        'nargs': 2,
        'code': [S('shr')],
        'exported': False,
    },
    'shl': {
        'nargs': 2,
        'code': [S('shl')],
        'exported': False,
    },
    'asr': {
        'nargs': 2,
        'code': [S('asr')],
        'exported': False,
    },
    'bnot': {
        'nargs': 1,
        'code': [S('bnot')],
        'exported': False,
    },
    'band': {
        'nargs': 2,
        'code': [S('band')],
        'exported': False,
    },
    'bor': {
        'nargs': 2,
        'code': [S('bor')],
        'exported': False,
    },
    'bxor': {
        'nargs': 2,
        'code': [S('bxor')],
        'exported': False,
    },
    'cons': {
        'nargs': 2,
        'code': [S('cons')],
        'exported': True,
    },
    'car': {
        'nargs': 1,
        'code': [S('car')],
        'exported': True,
    },
    'cdr': {
        'nargs': 1,
        'code': [S('cdr')],
        'exported': True,
    },
    'set-car!': {
        'nargs': 2,
        'code': [S('setcar'), S('void')],
        'exported': True,
    },
    'set-cdr!': {
        'nargs': 2,
        'code': [S('setcdr'), S('void')],
        'exported': True,
    },
    'type': {
        'nargs': 1,
        'code': [S('type')],
        'exported': True,
    },
    'eq?': {
        'nargs': 2,
        'code': [S('eq')],
        'exported': True,
    },
    'gensym': {
        'nargs': 1,
        'code': [S('gensym')],
        'exported': False,
    },
    'char->integer': {
        'nargs': 1,
        'code': [S('ch2i')],
        'exported': True,
    },
    'integer->char': {
        'nargs': 1,
        'code': [S('i2ch')],
        'exported': True,
    },
    'char-general-category': {
        'nargs': 1,
        'code': [S('ugcat')],
        'exported': True,
    },
    'char-upcase': {
        'nargs': 1,
        'code': [S('chup')],
        'exported': True,
    },
    'char-downcase': {
        'nargs': 1,
        'code': [S('chdn')],
        'exported': True,
    },
    'char-foldcase': {
        'nargs': 1,
        'code': [S('chfd')],
        'exported': True,
    },
    'digit-value': {
        'nargs': 1,
        'code': [S('chdv')],
        'exported': True,
    },
    'make-string': {
        'nargs': 2,
        'code': [S('mkstr')],
        'exported': False,
    },
    'string-ref': {
        'nargs': 2,
        'code': [S('strref')],
        'exported': True,
    },
    'string-set!': {
        'nargs': 3,
        'code': [S('strset'), S('void')],
        'exported': True,
    },
    'string-length': {
        'nargs': 1,
        'code': [S('strlen')],
        'exported': True,
    },
    'symbol->string': {
        'nargs': 1,
        'code': [S('sym2str')],
        'exported': True,
    },
    'string->symbol': {
        'nargs': 1,
        'code': [S('str2sym')],
        'exported': True,
    },
    'make-vector': {
        'nargs': 2,
        'code': [S('mkvec')],
        'exported': False,
    },
    'vector-ref': {
        'nargs': 2,
        'code': [S('vecref')],
        'exported': True,
    },
    'vector-set!': {
        'nargs': 3,
        'code': [S('vecset'), S('void')],
        'exported': False,
    },
    'vector-length': {
        'nargs': 1,
        'code': [S('veclen')],
        'exported': True,
    },
    'wrap': {
        'nargs': 2,
        'code': [S('wrap')],
        'exported': False,
    },
    'unwrap': {
        'nargs': 1,
        'code': [S('unwrap')],
        'exported': False,
    },
    'set-system-exception-handler': {
        'nargs': 1,
        'code': [S('seh'), S('void')],
        'exported': False,
    },
    'abort': {
        'nargs': 2,
        'code': [S('abort')],
        'exported': False,
    },
    'features': {
        'nargs': 0,
        'code': asm_code_for_features(),
        'exported': True,
    },
    'make-bytevector': {
        'nargs': 2,
        'code': [S('mkbvec')],
        'exported': False,
    },
    'bytevector-u8-ref': {
        'nargs': 2,
        'code': [S('bvecref')],
        'exported': True,
    },
    'bytevector-u8-set!': {
        'nargs': 3,
        'code': [S('bvecset'), S('void')],
        'exported': False,
    },
    'bytevector-length': {
        'nargs': 1,
        'code': [S('bveclen')],
        'exported': True,
    },
}
