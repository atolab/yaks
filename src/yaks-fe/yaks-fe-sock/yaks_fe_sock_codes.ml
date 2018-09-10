[%%cenum
type message_id = 
| OPEN [@id 0x01]
| CREATE [@id 0x02]
| DELETE [@id 0x03]
| PUT [@id 0xA0]
| PATCH [@id 0xA1]
| GET [@id 0xA2]
| SUB [@id 0xB0]
| UNSUB [@id 0xB1]
| NOTIFY [@id 0xB2]
| EVAL [@id 0xB3]
| OK [@id 0xD0]
| VALUE [@id 0xD1]
| VALUES [@id 0xD2]
| ERROR [@id 0xE0]
[@@uint8_t]]

[%%cenum 
type value_encoding = 
| RAW [@id  0x08]
| JSON [@id 0x10]
| PROTOBUF [@id 0x18]
| ENCODING_INVALID [@id 0x0]
[@@uint8_t]]

[%%cenum
type message_flags = 
| PROPERTY [@id 0x01]
| STORAGE [@id 0x02]
| ACCESS [@id 0x04]
| ENCODING [@id 0x38]
| ENCODING_RAW [@id 0x08]
| ENCODING_JSON [@id 0x10]
| ENCODING_PROTO [@id 0x18]

[@@uint8_t]]

[%%cenum
type error_code = 
| BAD_REQUEST [@id 400]
| FORBIDDEN [@id 403]
| NOT_FOUND [@id 404]
| PRECONDITION_FAILED [@id 412]
| NOT_IMPLEMENTED [@id 501]
| INSUFFICIENT_STORAGE [@id 507]
[@@uint16_t]]