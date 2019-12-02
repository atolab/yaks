## 0.3.0 (2019-12-02)
 - Updated Zenoh to 0.3.0
 - Re-designed Yaks as a Zenoh plugin
 - Removed socket and web-socket frontends (as now Zenoh is the only transport used by Yaks APIs)
 - Removed REST frontends (replaced by the Zenoh HTTP plugin)
 - Updated Admin Space to integrate with Zenoh's (now Yaks' is /@/_zenoh-id_/plugins/yaks/...)
 - Added InfluxDB backend and support of "starttime" and "stoptime" in selectors properties

## 0.2.7 (2019-04-18)
 - Added for key /@/_yaksid_ a JSON value representing the full admin view of a Yaks service
 - Fixed bug in admin space occuring at creation of worksapce or subscription
 - Fixed transcoding from String to JSON encoding
 - In yaks-ocaml and yaks-python API:
   - Added latency and throughput examples
   - Performance improvements
 - Updated Zenoh to 0.2.5 which brings these changes:
   - Management : zenoh routers answer to management queries.
   - Bug fixes in the discovery engine.

## 0.2.6 (2019-03-25)
 - Added the management of removal with timestamps. Thus, in case for a same key, a *put* message with an old timestamp
   arrives after a *remove* message with more recent timestamp, the key will not be re-inserted and will be kept as removed. (#20)
 - In yaks-ocaml and yaks-python API:
    - Changed the signature of the listener callback passed to the subscribe operation. 
      Now the listener receives a list pof `(path * change)` instead of a list of `(path * value)`.
      A **change** can correspond to either a *put*, and *update* or a *remove*. (#21)
 - Updated Zenoh to 0.2.4 which brings these changes:
    - Better performances (throughput) via batching of small messages and other I/O improvements

## 0.2.5 (2019-02-21)
 - Fixed client hang when it invokes a remote eval function using a selector with wildcards (#22)
 - Fixed cleanup of Zenoh subscriptions made for a client when this clients logtous (#23)
 - Fixed Yaks to update its HLC for each received timestamp
 - Fixed admin space to not answer remote (via Zenoh) 'get' operations made on '/**' 
   (the admin space should only answer to selectors starting with '/@/')
 - In yaks-python API:
    - Fixed encoding of RAW Value when created with bytes
    - Fixed decoding of an empty list of properties
    - Fixed concurrency issue when sending messages to the Socket frontend
 - Updated Zenoh to 0.2.3 which brings these changes:
    - Better performances (throughput) via changes in buffers and I/O management

## 0.2.4 (2019-02-11)
 - Added storage alignement: when a storage is created, it first retrieves already existing key/values from remote storages (#16).
 - Fixed warning *InternalError (Msg ("No frontend with id: Yaks"))* displayed when a subscriber is not reachable (#15).
 - Fixed the automatic session/subscriptions/eval removal whenever a TCP client is disconnected (#17).
 - Fixed calls to a remote *eval* function (via Zenoh) that returned nothing (#19).
 - Updated Zenoh to 0.2.2 which brings these changes:
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
