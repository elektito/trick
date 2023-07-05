from compile import compile_form
from assemble import assemble
from secd import Secd


class EvalError(Exception):
    pass


def eval(form):
    assembly_code = compile_form(form)
    machine_code = assemble(assembly_code)
    machine = Secd(machine_code)
    machine.run()
    if machine.halt_code is not None:
        raise EvalError('Evaluated code halted')
    if len(machine.s) == 0:
        raise EvalError('Evaluated code did not return anything')
    if len(machine.s) > 1:
        raise EvalError('Evaluated code returned more than one thing')
    return machine.s[0]
