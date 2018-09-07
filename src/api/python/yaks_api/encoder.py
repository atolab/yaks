class VLEEncoder(object):

    def __init__(self):
        self.MORE_BYTES_FLAG = 0x80
        self.BYTE_MASK = 0x7F
        self.SHIFT_LEN = 7
        self.MAX_BITS = 64
        self.MAX_BYTES = 10

    def encode(self, value):

        def actual_encoding(v, buff):
            if v <= self.BYTE_MASK:
                buff.append(v.to_bytes(1, byteorder='big'))
            else:
                mv = self.MORE_BYTES_FLAG | (v & self.BYTE_MASK)
                buff.append(mv.to_bytes(1, byteorder='big'))
                actual_encoding((v >> self.SHIFT_LEN), buff)

        buff = []
        actual_encoding(value, buff)
        return buff

    def decode(self, buff):
        value = 0
        for i in range(0, len(buff)):
            v = int.from_bytes(buff[i], byteorder='big', signed=False)
            value = value | (v << (8 * i))

        def merge(v, char, n):
            return v | (char << (n * self.SHIFT_LEN))

        def masked(c):
            return self.BYTE_MASK & c

        def actual_decoding(v, n, buf):
            if n < self.MAX_BYTES:
                c = int.from_bytes(buf[0], byteorder='big', signed=False)
                nbuf = buf[1:]
                if c <= self.BYTE_MASK:
                    return merge(v, c, n)
                else:
                    return actual_decoding(merge(v, masked(c), n), n + 1, nbuf)

        return actual_decoding(0, 0, buff)
