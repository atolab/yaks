import messages


def main():
    raw = b'\x01\x00' \
          b'\x07\x00' \
          b'\xAA\x00\x00\x00' \
          b'\x0A\x00\x00\x00' \
          b'\x01\x00' \
          b'\x04\x00' \
          b'\x4B\x65' \
          b'\x79\x00' \
          b'\x06\x00' \
          b'\x56\x61' \
          b'\x6C\x75' \
          b'\x65\x00' \
          b'\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xAA'
    m = messages.Message(raw)
    #m.unpack()
    #m.pprint()
    #m.dump()

    #packed = m.pack()
    #new_msg = messages.Message(packed)
    #new_msg.unpack()
    #new_msg.pprint()

    open = messages.OpenMessage('user', 'pwd')
    open.pprint()
    open.pack()
    open.dump()

    open = messages.CreateMessage(messages.CreationType.STORAGE, open.corr_id, "//afos/0")
    open.pprint()
    open.pack()
    open.dump()

    open.remove_property('yaks.path')
    open.pprint()
    open.pack()
    open.dump()

if __name__=='__main__':
    main()