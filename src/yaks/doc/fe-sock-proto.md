# Socket Front-end Protocol

Clients accessing YAKS through the socket front end will have to implement 
the simple protocol defined below. 

## Message Format
The front-end protocol messages have the following structure:

    type property = { key : string; value: string }

    type header = { 
      id : int16; 
      flags: int16; 
      corr_id : int64; 
      length : int64;
      properties : property list;
    }

    type create = {
      h : header;
      res : string;
    }

    type put = {
      h : header;
      key : string;
      value : Lwt_bytes.t
    }
    
               1
     5 4 3 2 1 0 9 8 7 6 5 4 3 2 1 0
    +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
    |           MESSAGE CODE        |
    +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
    |              Flags      |A|S|P|
    +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
    |                               |
    +             Corr. ID          +
    |                               |
    +-------------------------------+    
    |                               |
    +             Length            +
    |                               |
    +-------------------------------+    
    ~          Properties           ~
    +-------------------------------+
    ~               Body            ~
    +-------------------------------+


- The **Message Code** identifies univoqually the message.

- **Flags**: provides message specific flags, if defined. Any 
         undefined flag should be set to zero

- **Lenght**: the length of remaining portion of the message

- **Properties**: a list of <string, string> pairs. Properties are as follows:

    +-------------------------------+    
    |                               |
    +        Properties Number      +
    |                               |
    +-------------------------------+    
    ~           Property            ~
    +-------------------------------+
                    .
                    .
                    .
    +-------------------------------+         
    ~            Property           ~
    +-------------------------------+


Where property is encoded as: 

    +-------------------------------+    
    |                               |
    +             length            +
    |                               |
    +-------------------------------+    
    ~              key              ~
    +-------------------------------+
    |                               |
    +            length             +
    |                               |
    +-------------------------------+    
    ~              Value            ~
    +-------------------------------+

### Session Establishment
The first message that to be sent to a YAKS service is the authentication message. This allows a service to authenticate the client using the credentials provided on the properties. If the authentication fail. then the connection is immediately terminated.


               1
     5 4 3 2 1 0 9 8 7 6 5 4 3 2 1 0
    +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
    |              OPEN             |
    +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
    |                         |0|0|1|
    +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
    |                               |
    +             Corr. ID          +
    |                               |
    +-------------------------------+    
    |                               |
    +             Length            +
    |                               |
    +-------------------------------+    
    ~            Properties         ~
    +-------------------------------+


The properties may contain the following authentication mechanisms:

#### User Id and Password
In this case the following properties have to be defined:

key: "yaks.login" 
value: user-name:password

#### Return Message
If the authentication is successful the initiation will receive a message OK with the same correlation ID 

### Entity Creation

               1
     5 4 3 2 1 0 9 8 7 6 5 4 3 2 1 0
    +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
    |             CREATE            |
    +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
    |                         |A|S|1|
    +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
    |                               |
    +             Corr. ID          +
    |                               |
    +-------------------------------+    
    |                               |
    +            Length             +
    |                               |
    +-------------------------------+    
    ~           Properties          ~
    +-------------------------------+
    |           Resource            |
    +-------------------------------+


Where A=1 when creating an access and S=1 when creating a storage.

#### Properties for controlling access creation:

The **compulsory** properties are: 

    name: "yaks.path"
    value: the path for the access

The  **optional** properties are:

    name: "yaks.id"
    value: the **string** representing the access identifier

and 

    name: "yaks.cache.size"
    value: an integer representing the access cache 
           size in number of entries

#### Properties for controlling storage creation:
The **compulsory** properties are: 

    name: "yaks.path"
    value: the path for the access

The  **optional** properties are:

    name: "yaks.id"
    value: the **string** representing the storage identifier

and 

    name: "yaks.storage.config"
    value: a JSON representing the configuration of the storage.

#### Return message
An OK is returned that provides the yaks.id of the access/storage created if the creation succeded. Otherwise an ERROR is returned with the proper code.

### Entity Deletion

## Message Codes

|   Message  |   Id   |
|------------|:------:|
|    AUTH    |  0x01  |
|   CREATE   |  0x02  |
|   DELETE   |  0x03  |
|     PUT    |  0xA0  |
|    PATCH   |  0XA1  |
|     GET    |  0xA2  |
|     SUB    |  0XB0  |
|    UNSUB   |  0XB1  |
|    EVAL    |  0xB2  |
|     OK     |  0xC0  |
|    NOTIFY  |  0xD0  |
|    ERROR   |  0xE0  |




## TEMP
value.
  
- Message properties

- Body: the message body, which depends on the kind of message.

### Value Types 
One byte is used to identify the type of a value. The table below reports these types:

    0x01 : byte
    0x02 : UTF-8 char 
    0x03 : int64
    0x04 : string
    0xa0 t : sequence of t
    0xb0 t q : tuple of type t * q
    
Based on this the following types are expressed as: 

    bytes = sequence byte : 0xa0 0x01

Notice that strings are represented as a different type, as they are commonly used and it is a bit 
more convenient to supporte them as primive type.

A **string** is encoded using its code 0x04 followed by 16-bit lenght and the the 
characters representing the string.

Finally, all encoding is **little-endian**.

### Primitive Types Encoding

- **byte**: Encoded as a byte 

- **char**: encoded as an UTF-8 chararcter 

- **int64** : Encoded as a VLE  in zenoh

- **string** : Encoded as a VLE length plus the characters that make the string.

- **sequence** : encoded as a VLE length and then the 
                 sequence of elements individually encoded. 
