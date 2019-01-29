# **YAKS Changelog**
## **v0.2.3**
 - Fix *get* behaviour when the selector is not fully covered by a local storage. Now it also issues a remote query via Zenoh.
 - Fix socket frontend to use a variable-length buffer for outgoing message, allowing to send values with size >65kb

## **v0.2.2**
 - Fix *eval* behaviour when called remotelly via Zenoh

## **v0.2.1**
 - Added -y option to yaksd.exe (allows to specify the Yaks ID).
 - Added prefix operators `~//`, `~/*` and `~$` in yaks-ocaml API
   (respectively for creation of Path, Selector and string Value).
 - Changed the way Properties are specified in Selector: now enclosed with `()` instead of `[]`. <br>
   _Example of Selector with properties:_ `/a/b/**?(p1=v1;p2=v2)` .
 - Updated the REST frontend to add the `Access-Control-Allow-Origin=*` header in HTTP responses to a GET. <br>
   This allows any Javascript running in a browser to get data from Yaks.
 - Updated the Socket frontend to possibly send multiple messages in resonse to a `get`, in case the
   resulting list of keys/values is too large for a single message.
 - Fixed various issues with eval, in Yaks service, yaks-ocaml and yaks-python APIs
   (failing call through Zenoh, possible deadlock, errors management).
 - Fixed implementation of `get` when the selector is not fully covered by a storage.
 - Fixed in yaks-ocaml API a deadlock when the listener's callback queries Yaks again.
 

## **v0.2.0**
 - First public release
