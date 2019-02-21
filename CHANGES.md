## 0.2.4 (2019-02-11)
 - Added storage alignement: when a storage is created, it first retrieves already existing key/values from remote storages (#16).
 - Fixed warning *InternalError (Msg ("No frontend with id: Yaks"))* displayed when a subscriber is not reachable (#15).
 - Fixed the automatic session/subscriptions/eval removal whenever a TCP client is disconnected (#17).
 - Fixed calls to a remote *eval* function (via Zenoh) that returned nothing (#19).
 - Updated Zenoh to 0.2.2 which brings these changes
    - Fix bug when multiple storage declarations in the same client
    - Fix bugs in high throughput situations

## 0.2.3 (2019-01-29)
 - Fix *get* behaviour when the selector is not fully covered by a local storage. Now it also issues a remote query via Zenoh (#13).
 - Fix socket frontend to use a variable-length buffer for outgoing message, allowing to send values with size >65kb (#14).

## 0.2.2 (2019-01-28)
 - Fix *eval* behaviour when called remotelly via Zenoh (#11)

## 0.2.1 (2019-01-25)
 - Added -y option to yaksd.exe (allows to specify the Yaks ID) (#4).
 - Added prefix operators `~//`, `~/*` and `~$` in yaks-ocaml API
   (respectively for creation of Path, Selector and string Value) (#6).
 - Changed the way Properties are specified in Selector: now enclosed with `()` instead of `[]`. <br>
   _Example of Selector with properties:_ `/a/b/**?(p1=v1;p2=v2)` (#8).
 - Updated the REST frontend to add the `Access-Control-Allow-Origin=*` header in HTTP responses to a GET. <br>
   This allows any Javascript running in a browser to get data from Yaks.
 - Updated the Socket frontend to possibly send multiple messages in resonse to a `get`, in case the
   resulting list of keys/values is too large for a single message (#10).
 - Fixed various issues with eval, in Yaks service, yaks-ocaml and yaks-python APIs
   (failing call through Zenoh, possible deadlock, errors management).
 - Fixed implementation of `get` when the selector is not fully covered by a storage.
 - Fixed in yaks-ocaml API a deadlock when the listener's callback queries Yaks again.

## 0.2.0 (2019-01-11)
 - First public release