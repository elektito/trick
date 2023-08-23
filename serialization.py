import struct

from utils import STR_ENCODING


class SerializationError(Exception):
    pass


class Serializable:
    serialization_id = None

    def dump(self, output):
        output.write(struct.pack('<I', self.serialization_id))
        self._dump(output)

    def _dump(self, output):
        raise NotImplementedError

    @classmethod
    def load(cls, input):
        subclasses = cls._get_serializable_subclasses()
        map = {
            s.serialization_id: s for s in subclasses
        }

        assert len(map) == len(subclasses), \
            f'Duplicate serialization ids for {cls.__name__}'

        sid = input.read(4)
        sid, = struct.unpack('<I', sid)
        subclass = map.get(sid)
        if subclass is None:
            raise SerializationError(
                f'Unknown serialization id {sid} for {cls.__name__}')

        try:
            return subclass._load(input)
        except NotImplementedError:
            raise SerializationError(
                f'Class {subclass.__name__} does not implement _load')

    @staticmethod
    def _load(input):
        raise NotImplementedError

    @classmethod
    def _get_serializable_subclasses(cls):
        raise NotImplementedError

    def _dump_byte(self, b: int, output):
        output.write(struct.pack('B', b))

    @staticmethod
    def _load_byte(input) -> int:
        return input.read(1)[0]

    def _dump_uint4(self, n: int, output):
        output.write(struct.pack('<I', n))

    @staticmethod
    def _load_uint4(input) -> int:
        n = input.read(4)
        n, = struct.unpack('<I', n)
        return n

    def _dump_int8(self, n: int, output):
        output.write(struct.pack('<q', n))

    @staticmethod
    def _load_int8(input) -> int:
        n = input.read(8)
        n, = struct.unpack('<q', n)
        return n

    def _dump_string(self, s: str, output):
        self._dump_uint4(len(s), output)
        output.write(s.encode(STR_ENCODING))

    @staticmethod
    def _load_string(input) -> str:
        length = Serializable._load_uint4(input)
        str_bytes = input.read(length)
        return str_bytes.decode(STR_ENCODING)

    def _dump_list(self, l: list, output):
        self._dump_uint4(len(l), output)
        for i in l:
            i.dump(output)

    @staticmethod
    def _load_list(klass, input) -> list:
        size = Serializable._load_uint4(input)
        return [klass.load(input) for _ in range(size)]

    def _dump_optional(self, value, output):
        if value is None:
            self._dump_byte(0, output)
        else:
            self._dump_byte(1, output)
            value.dump(output)

    @staticmethod
    def _load_optional(klass, input):
        present = Serializable._load_byte(input)
        if present == 0:
            return None
        return klass.load(input)
