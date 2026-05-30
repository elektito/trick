from .machinetypes import Symbol, List

class Optimizer:
    def __init__(self):
        self.op_args = {
            'ldc': 1, 'ld': 1, 'st': 1, 'trap': 1, 'ldstr': 1, 'ldsym': 1,
            'set': 1, 'get': 1, 'unset': 1, 'ldcf': 1, 'ldcq': 1,
            'ap': 1, 'tap': 1, 'rap': 1,
            'ldf': 2, 'sel': 2,
            ':expr-start': 1, ':expr-end': 1,
            ':define-start': 2, ':define-end': 1,
            ':filename-start': 1, ':filename-end': 0,
        }

        # Instructions that push 1 value, consume 0, and have no side effects
        self.pure_push_0 = {'nil', 'true', 'false', 'void', 'dup'}
        self.pure_push_1 = {'ldc', 'ldcf', 'ldcq', 'ldstr', 'ldsym', 'ld', 'get'}
        self.pure_push_2 = {'ldf'}

    def get_arg_count(self, name):
        return self.op_args.get(name, 0)

    def is_debug_op(self, instr):
        return isinstance(instr, Symbol) and instr.name.startswith(':')

    def find_next_real_instruction(self, code, start_idx):
        idx = start_idx
        while idx < len(code):
            item = code[idx]
            if isinstance(item, Symbol):
                if self.is_debug_op(item):
                    # Skip args of debug op
                    nargs = self.get_arg_count(item.name)
                    idx += 1 + nargs
                    continue
                else:
                    return idx
            else:
                # Should not happen if we are aligned
                return -1
        return -1

    def copy_debug_ops(self, code, start_idx, end_idx, dest_list):
        idx = start_idx
        while idx < end_idx:
            item = code[idx]
            dest_list.append(item)
            if isinstance(item, Symbol) and self.is_debug_op(item):
                 nargs = self.get_arg_count(item.name)
                 for _ in range(nargs):
                     idx += 1
                     dest_list.append(code[idx])
            idx += 1

    def optimize(self, code):
        if isinstance(code, List):
            code = code.to_list_recursive()

        if not isinstance(code, list):
            return code

        new_code = []
        i = 0
        while i < len(code):
            instr = code[i]

            if not isinstance(instr, Symbol):
                new_code.append(instr)
                i += 1
                continue

            name = instr.name

            # Check for PUSH 0 + DROP
            if name in self.pure_push_0:
                drop_idx = self.find_next_real_instruction(code, i + 1)
                if drop_idx != -1 and code[drop_idx].name == 'drop':
                     # Found drop.
                     self.copy_debug_ops(code, i + 1, drop_idx, new_code)
                     i = drop_idx + 1
                     continue

            # Check for PUSH 1 + DROP
            elif name in self.pure_push_1:
                drop_idx = self.find_next_real_instruction(code, i + 2)
                if drop_idx != -1 and code[drop_idx].name == 'drop':
                    self.copy_debug_ops(code, i + 2, drop_idx, new_code)
                    i = drop_idx + 1
                    continue

            # Check for PUSH 2 + DROP (ldf)
            elif name in self.pure_push_2:
                 drop_idx = self.find_next_real_instruction(code, i + 3)
                 if drop_idx != -1 and code[drop_idx].name == 'drop':
                    self.copy_debug_ops(code, i + 3, drop_idx, new_code)
                    i = drop_idx + 1
                    continue

            # If we are here, we are keeping the instruction.
            # We need to optimize its args if they are code blocks.

            if name == 'ldf':
                # ldf nargs body
                nargs = code[i+1]
                body = code[i+2]
                new_code.append(instr)
                new_code.append(nargs)
                new_code.append(self.optimize(body))
                i += 3
                continue
            elif name == 'sel':
                true_body = code[i+1]
                false_body = code[i+2]
                new_code.append(instr)
                new_code.append(self.optimize(true_body))
                new_code.append(self.optimize(false_body))
                i += 3
                continue

            # Default copy
            nargs = self.get_arg_count(name)
            if i + nargs >= len(code):
                raise ValueError(f'truncated instruction {name!r}: expected {nargs} args')
            new_code.append(instr)
            for _ in range(nargs):
                i += 1
                new_code.append(code[i])
            i += 1

        return new_code
