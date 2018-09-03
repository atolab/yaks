
import messages


msg1 = messages.Message()
msg1.message_code = 0xFE
msg1.generate_corr_id()

cid = msg1.corr_id
msg1.add_property('key1', 'value1')
msg1.add_property('key2', 'value2')

packed_msg1 = msg1.pack()

msg2 = messages.Message(packed_msg1)

msg2.unpack()

msg1.pprint()

msg2.pprint()


msg1.dump()
msg2.dump()
