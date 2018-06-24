(* Auto-generated from "fecmd.atd" *)
              [@@@ocaml.warning "-27-32-35-39"]

type tuple = { key: string; value: string }

type storage_info = { id: string option; path: string }

type storage_control = [
    `Create of storage_info
  | `Close of string
  | `Dispose of string
]

type access_operation = [
    `Put of tuple
  | `Get of string
  | `DPut of tuple
  | `Sub of string
  | `Unsub of string
]

type access_info = { id: string option; path: string; cache_size: int }

type access_control = [
    `Create of access_info
  | `Close of string
  | `Dispose of string
]

type request = [
    `SCtrl of storage_control
  | `ACtrl of access_control
  | `AOp of access_operation
]

type notification = { sid: int; elems: tuple list }

type reply = [
    `Ok
  | `Error of int
  | `Values of tuple list
  | `Notice of notification
  | `SubId of int
]

type message = { mid: int; msg: [ `Request of request | `Reply of reply ] }

type access_operator = { id: string; op: access_operation }

let tuple_tag = Bi_io.record_tag
let write_untagged_tuple : Bi_outbuf.t -> tuple -> unit = (
  fun ob x ->
    Bi_vint.write_uvint ob 2;
    Bi_outbuf.add_char4 ob '\128' 'Q' '\137' '\159';
    (
      Bi_io.write_string
    ) ob x.key;
    Bi_outbuf.add_char4 ob '\177' '\184' '\127' 'q';
    (
      Bi_io.write_string
    ) ob x.value;
)
let write_tuple ob x =
  Bi_io.write_tag ob Bi_io.record_tag;
  write_untagged_tuple ob x
let string_of_tuple ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write_tuple ob x;
  Bi_outbuf.contents ob
let get_tuple_reader = (
  fun tag ->
    if tag <> 21 then Atdgen_runtime.Ob_run.read_error () else
      fun ib ->
        let field_key = ref (Obj.magic (Sys.opaque_identity 0.0)) in
        let field_value = ref (Obj.magic (Sys.opaque_identity 0.0)) in
        let bits0 = ref 0 in
        let len = Bi_vint.read_uvint ib in
        for i = 1 to len do
          match Bi_io.read_field_hashtag ib with
            | 5343647 ->
              field_key := (
                (
                  Atdgen_runtime.Ob_run.read_string
                ) ib
              );
              bits0 := !bits0 lor 0x1;
            | 834174833 ->
              field_value := (
                (
                  Atdgen_runtime.Ob_run.read_string
                ) ib
              );
              bits0 := !bits0 lor 0x2;
            | _ -> Bi_io.skip ib
        done;
        if !bits0 <> 0x3 then Atdgen_runtime.Ob_run.missing_fields [| !bits0 |] [| "key"; "value" |];
        (
          {
            key = !field_key;
            value = !field_value;
          }
         : tuple)
)
let read_tuple = (
  fun ib ->
    if Bi_io.read_tag ib <> 21 then Atdgen_runtime.Ob_run.read_error_at ib;
    let field_key = ref (Obj.magic (Sys.opaque_identity 0.0)) in
    let field_value = ref (Obj.magic (Sys.opaque_identity 0.0)) in
    let bits0 = ref 0 in
    let len = Bi_vint.read_uvint ib in
    for i = 1 to len do
      match Bi_io.read_field_hashtag ib with
        | 5343647 ->
          field_key := (
            (
              Atdgen_runtime.Ob_run.read_string
            ) ib
          );
          bits0 := !bits0 lor 0x1;
        | 834174833 ->
          field_value := (
            (
              Atdgen_runtime.Ob_run.read_string
            ) ib
          );
          bits0 := !bits0 lor 0x2;
        | _ -> Bi_io.skip ib
    done;
    if !bits0 <> 0x3 then Atdgen_runtime.Ob_run.missing_fields [| !bits0 |] [| "key"; "value" |];
    (
      {
        key = !field_key;
        value = !field_value;
      }
     : tuple)
)
let tuple_of_string ?pos s =
  read_tuple (Bi_inbuf.from_string ?pos s)
let _1_tag = Bi_io.num_variant_tag
let write_untagged__1 = (
  Atdgen_runtime.Ob_run.write_untagged_option (
    Bi_io.write_string
  )
)
let write__1 ob x =
  Bi_io.write_tag ob Bi_io.num_variant_tag;
  write_untagged__1 ob x
let string_of__1 ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write__1 ob x;
  Bi_outbuf.contents ob
let get__1_reader = (
  fun tag ->
    if tag <> 22 then Atdgen_runtime.Ob_run.read_error () else
      fun ib ->
        match Char.code (Bi_inbuf.read_char ib) with
          | 0 -> None
          | 0x80 ->
            Some (
              (
                Atdgen_runtime.Ob_run.read_string
              )
                ib
            )
          | _ -> Atdgen_runtime.Ob_run.read_error_at ib
)
let read__1 = (
  fun ib ->
    if Bi_io.read_tag ib <> 22 then Atdgen_runtime.Ob_run.read_error_at ib;
    match Char.code (Bi_inbuf.read_char ib) with
      | 0 -> None
      | 0x80 ->
        Some (
          (
            Atdgen_runtime.Ob_run.read_string
          )
            ib
        )
      | _ -> Atdgen_runtime.Ob_run.read_error_at ib
)
let _1_of_string ?pos s =
  read__1 (Bi_inbuf.from_string ?pos s)
let storage_info_tag = Bi_io.record_tag
let write_untagged_storage_info : Bi_outbuf.t -> storage_info -> unit = (
  fun ob x ->
    Bi_vint.write_uvint ob 2;
    Bi_outbuf.add_char4 ob '\128' '\000' '[' '\219';
    (
      write__1
    ) ob x.id;
    Bi_outbuf.add_char4 ob '\202' 'Q' '\229' '\165';
    (
      Bi_io.write_string
    ) ob x.path;
)
let write_storage_info ob x =
  Bi_io.write_tag ob Bi_io.record_tag;
  write_untagged_storage_info ob x
let string_of_storage_info ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write_storage_info ob x;
  Bi_outbuf.contents ob
let get_storage_info_reader = (
  fun tag ->
    if tag <> 21 then Atdgen_runtime.Ob_run.read_error () else
      fun ib ->
        let field_id = ref (Obj.magic (Sys.opaque_identity 0.0)) in
        let field_path = ref (Obj.magic (Sys.opaque_identity 0.0)) in
        let bits0 = ref 0 in
        let len = Bi_vint.read_uvint ib in
        for i = 1 to len do
          match Bi_io.read_field_hashtag ib with
            | 23515 ->
              field_id := (
                (
                  read__1
                ) ib
              );
              bits0 := !bits0 lor 0x1;
            | -900602459 ->
              field_path := (
                (
                  Atdgen_runtime.Ob_run.read_string
                ) ib
              );
              bits0 := !bits0 lor 0x2;
            | _ -> Bi_io.skip ib
        done;
        if !bits0 <> 0x3 then Atdgen_runtime.Ob_run.missing_fields [| !bits0 |] [| "id"; "path" |];
        (
          {
            id = !field_id;
            path = !field_path;
          }
         : storage_info)
)
let read_storage_info = (
  fun ib ->
    if Bi_io.read_tag ib <> 21 then Atdgen_runtime.Ob_run.read_error_at ib;
    let field_id = ref (Obj.magic (Sys.opaque_identity 0.0)) in
    let field_path = ref (Obj.magic (Sys.opaque_identity 0.0)) in
    let bits0 = ref 0 in
    let len = Bi_vint.read_uvint ib in
    for i = 1 to len do
      match Bi_io.read_field_hashtag ib with
        | 23515 ->
          field_id := (
            (
              read__1
            ) ib
          );
          bits0 := !bits0 lor 0x1;
        | -900602459 ->
          field_path := (
            (
              Atdgen_runtime.Ob_run.read_string
            ) ib
          );
          bits0 := !bits0 lor 0x2;
        | _ -> Bi_io.skip ib
    done;
    if !bits0 <> 0x3 then Atdgen_runtime.Ob_run.missing_fields [| !bits0 |] [| "id"; "path" |];
    (
      {
        id = !field_id;
        path = !field_path;
      }
     : storage_info)
)
let storage_info_of_string ?pos s =
  read_storage_info (Bi_inbuf.from_string ?pos s)
let storage_control_tag = Bi_io.variant_tag
let write_untagged_storage_control = (
  fun ob x ->
    match x with
      | `Create x ->
        Bi_outbuf.add_char4 ob '\176' '\163' '\219' '\028';
        (
          write_storage_info
        ) ob x
      | `Close x ->
        Bi_outbuf.add_char4 ob '\219' '\144' '\135' '\152';
        (
          Bi_io.write_string
        ) ob x
      | `Dispose x ->
        Bi_outbuf.add_char4 ob '\136' '\157' '\168' '\127';
        (
          Bi_io.write_string
        ) ob x
)
let write_storage_control ob x =
  Bi_io.write_tag ob Bi_io.variant_tag;
  write_untagged_storage_control ob x
let string_of_storage_control ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write_storage_control ob x;
  Bi_outbuf.contents ob
let get_storage_control_reader = (
  fun tag ->
    if tag <> 23 then Atdgen_runtime.Ob_run.read_error () else
      fun ib ->
        Bi_io.read_hashtag ib (fun ib h has_arg ->
          match h, has_arg with
            | 816044828, true -> (`Create (
                (
                  read_storage_info
                ) ib
              ))
            | -611285096, true -> (`Close (
                (
                  Atdgen_runtime.Ob_run.read_string
                ) ib
              ))
            | 144550015, true -> (`Dispose (
                (
                  Atdgen_runtime.Ob_run.read_string
                ) ib
              ))
            | _ -> Atdgen_runtime.Ob_run.unsupported_variant h has_arg
        )
)
let read_storage_control = (
  fun ib ->
    if Bi_io.read_tag ib <> 23 then Atdgen_runtime.Ob_run.read_error_at ib;
    Bi_io.read_hashtag ib (fun ib h has_arg ->
      match h, has_arg with
        | 816044828, true -> (`Create (
            (
              read_storage_info
            ) ib
          ))
        | -611285096, true -> (`Close (
            (
              Atdgen_runtime.Ob_run.read_string
            ) ib
          ))
        | 144550015, true -> (`Dispose (
            (
              Atdgen_runtime.Ob_run.read_string
            ) ib
          ))
        | _ -> Atdgen_runtime.Ob_run.unsupported_variant h has_arg
    )
)
let storage_control_of_string ?pos s =
  read_storage_control (Bi_inbuf.from_string ?pos s)
let access_operation_tag = Bi_io.variant_tag
let write_untagged_access_operation = (
  fun ob x ->
    match x with
      | `Put x ->
        Bi_outbuf.add_char4 ob '\128' '=' '\026' '\175';
        (
          write_tuple
        ) ob x
      | `Get x ->
        Bi_outbuf.add_char4 ob '\128' '6' '8' 'v';
        (
          Bi_io.write_string
        ) ob x
      | `DPut x ->
        Bi_outbuf.add_char4 ob '\173' '/' '\156' '\235';
        (
          write_tuple
        ) ob x
      | `Sub x ->
        Bi_outbuf.add_char4 ob '\128' '?' 'a' '`';
        (
          Bi_io.write_string
        ) ob x
      | `Unsub x ->
        Bi_outbuf.add_char4 ob '\186' '\028' 'p' '\167';
        (
          Bi_io.write_string
        ) ob x
)
let write_access_operation ob x =
  Bi_io.write_tag ob Bi_io.variant_tag;
  write_untagged_access_operation ob x
let string_of_access_operation ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write_access_operation ob x;
  Bi_outbuf.contents ob
let get_access_operation_reader = (
  fun tag ->
    if tag <> 23 then Atdgen_runtime.Ob_run.read_error () else
      fun ib ->
        Bi_io.read_hashtag ib (fun ib h has_arg ->
          match h, has_arg with
            | 4004527, true -> (`Put (
                (
                  read_tuple
                ) ib
              ))
            | 3553398, true -> (`Get (
                (
                  Atdgen_runtime.Ob_run.read_string
                ) ib
              ))
            | 758095083, true -> (`DPut (
                (
                  read_tuple
                ) ib
              ))
            | 4153696, true -> (`Sub (
                (
                  Atdgen_runtime.Ob_run.read_string
                ) ib
              ))
            | 974942375, true -> (`Unsub (
                (
                  Atdgen_runtime.Ob_run.read_string
                ) ib
              ))
            | _ -> Atdgen_runtime.Ob_run.unsupported_variant h has_arg
        )
)
let read_access_operation = (
  fun ib ->
    if Bi_io.read_tag ib <> 23 then Atdgen_runtime.Ob_run.read_error_at ib;
    Bi_io.read_hashtag ib (fun ib h has_arg ->
      match h, has_arg with
        | 4004527, true -> (`Put (
            (
              read_tuple
            ) ib
          ))
        | 3553398, true -> (`Get (
            (
              Atdgen_runtime.Ob_run.read_string
            ) ib
          ))
        | 758095083, true -> (`DPut (
            (
              read_tuple
            ) ib
          ))
        | 4153696, true -> (`Sub (
            (
              Atdgen_runtime.Ob_run.read_string
            ) ib
          ))
        | 974942375, true -> (`Unsub (
            (
              Atdgen_runtime.Ob_run.read_string
            ) ib
          ))
        | _ -> Atdgen_runtime.Ob_run.unsupported_variant h has_arg
    )
)
let access_operation_of_string ?pos s =
  read_access_operation (Bi_inbuf.from_string ?pos s)
let access_info_tag = Bi_io.record_tag
let write_untagged_access_info : Bi_outbuf.t -> access_info -> unit = (
  fun ob x ->
    Bi_vint.write_uvint ob 3;
    Bi_outbuf.add_char4 ob '\128' '\000' '[' '\219';
    (
      write__1
    ) ob x.id;
    Bi_outbuf.add_char4 ob '\202' 'Q' '\229' '\165';
    (
      Bi_io.write_string
    ) ob x.path;
    Bi_outbuf.add_char4 ob '\174' '\239' '\179' '\030';
    (
      Bi_io.write_svint
    ) ob x.cache_size;
)
let write_access_info ob x =
  Bi_io.write_tag ob Bi_io.record_tag;
  write_untagged_access_info ob x
let string_of_access_info ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write_access_info ob x;
  Bi_outbuf.contents ob
let get_access_info_reader = (
  fun tag ->
    if tag <> 21 then Atdgen_runtime.Ob_run.read_error () else
      fun ib ->
        let field_id = ref (Obj.magic (Sys.opaque_identity 0.0)) in
        let field_path = ref (Obj.magic (Sys.opaque_identity 0.0)) in
        let field_cache_size = ref (Obj.magic (Sys.opaque_identity 0.0)) in
        let bits0 = ref 0 in
        let len = Bi_vint.read_uvint ib in
        for i = 1 to len do
          match Bi_io.read_field_hashtag ib with
            | 23515 ->
              field_id := (
                (
                  read__1
                ) ib
              );
              bits0 := !bits0 lor 0x1;
            | -900602459 ->
              field_path := (
                (
                  Atdgen_runtime.Ob_run.read_string
                ) ib
              );
              bits0 := !bits0 lor 0x2;
            | 787460894 ->
              field_cache_size := (
                (
                  Atdgen_runtime.Ob_run.read_int
                ) ib
              );
              bits0 := !bits0 lor 0x4;
            | _ -> Bi_io.skip ib
        done;
        if !bits0 <> 0x7 then Atdgen_runtime.Ob_run.missing_fields [| !bits0 |] [| "id"; "path"; "cache_size" |];
        (
          {
            id = !field_id;
            path = !field_path;
            cache_size = !field_cache_size;
          }
         : access_info)
)
let read_access_info = (
  fun ib ->
    if Bi_io.read_tag ib <> 21 then Atdgen_runtime.Ob_run.read_error_at ib;
    let field_id = ref (Obj.magic (Sys.opaque_identity 0.0)) in
    let field_path = ref (Obj.magic (Sys.opaque_identity 0.0)) in
    let field_cache_size = ref (Obj.magic (Sys.opaque_identity 0.0)) in
    let bits0 = ref 0 in
    let len = Bi_vint.read_uvint ib in
    for i = 1 to len do
      match Bi_io.read_field_hashtag ib with
        | 23515 ->
          field_id := (
            (
              read__1
            ) ib
          );
          bits0 := !bits0 lor 0x1;
        | -900602459 ->
          field_path := (
            (
              Atdgen_runtime.Ob_run.read_string
            ) ib
          );
          bits0 := !bits0 lor 0x2;
        | 787460894 ->
          field_cache_size := (
            (
              Atdgen_runtime.Ob_run.read_int
            ) ib
          );
          bits0 := !bits0 lor 0x4;
        | _ -> Bi_io.skip ib
    done;
    if !bits0 <> 0x7 then Atdgen_runtime.Ob_run.missing_fields [| !bits0 |] [| "id"; "path"; "cache_size" |];
    (
      {
        id = !field_id;
        path = !field_path;
        cache_size = !field_cache_size;
      }
     : access_info)
)
let access_info_of_string ?pos s =
  read_access_info (Bi_inbuf.from_string ?pos s)
let access_control_tag = Bi_io.variant_tag
let write_untagged_access_control = (
  fun ob x ->
    match x with
      | `Create x ->
        Bi_outbuf.add_char4 ob '\176' '\163' '\219' '\028';
        (
          write_access_info
        ) ob x
      | `Close x ->
        Bi_outbuf.add_char4 ob '\219' '\144' '\135' '\152';
        (
          Bi_io.write_string
        ) ob x
      | `Dispose x ->
        Bi_outbuf.add_char4 ob '\136' '\157' '\168' '\127';
        (
          Bi_io.write_string
        ) ob x
)
let write_access_control ob x =
  Bi_io.write_tag ob Bi_io.variant_tag;
  write_untagged_access_control ob x
let string_of_access_control ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write_access_control ob x;
  Bi_outbuf.contents ob
let get_access_control_reader = (
  fun tag ->
    if tag <> 23 then Atdgen_runtime.Ob_run.read_error () else
      fun ib ->
        Bi_io.read_hashtag ib (fun ib h has_arg ->
          match h, has_arg with
            | 816044828, true -> (`Create (
                (
                  read_access_info
                ) ib
              ))
            | -611285096, true -> (`Close (
                (
                  Atdgen_runtime.Ob_run.read_string
                ) ib
              ))
            | 144550015, true -> (`Dispose (
                (
                  Atdgen_runtime.Ob_run.read_string
                ) ib
              ))
            | _ -> Atdgen_runtime.Ob_run.unsupported_variant h has_arg
        )
)
let read_access_control = (
  fun ib ->
    if Bi_io.read_tag ib <> 23 then Atdgen_runtime.Ob_run.read_error_at ib;
    Bi_io.read_hashtag ib (fun ib h has_arg ->
      match h, has_arg with
        | 816044828, true -> (`Create (
            (
              read_access_info
            ) ib
          ))
        | -611285096, true -> (`Close (
            (
              Atdgen_runtime.Ob_run.read_string
            ) ib
          ))
        | 144550015, true -> (`Dispose (
            (
              Atdgen_runtime.Ob_run.read_string
            ) ib
          ))
        | _ -> Atdgen_runtime.Ob_run.unsupported_variant h has_arg
    )
)
let access_control_of_string ?pos s =
  read_access_control (Bi_inbuf.from_string ?pos s)
let request_tag = Bi_io.variant_tag
let write_untagged_request = (
  fun ob x ->
    match x with
      | `SCtrl x ->
        Bi_outbuf.add_char4 ob '\246' '\227' '\218' '\158';
        (
          write_storage_control
        ) ob x
      | `ACtrl x ->
        Bi_outbuf.add_char4 ob '\153' '\173' 'i' '\140';
        (
          write_access_control
        ) ob x
      | `AOp x ->
        Bi_outbuf.add_char4 ob '\128' '1' '\151' '\194';
        (
          write_access_operation
        ) ob x
)
let write_request ob x =
  Bi_io.write_tag ob Bi_io.variant_tag;
  write_untagged_request ob x
let string_of_request ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write_request ob x;
  Bi_outbuf.contents ob
let get_request_reader = (
  fun tag ->
    if tag <> 23 then Atdgen_runtime.Ob_run.read_error () else
      fun ib ->
        Bi_io.read_hashtag ib (fun ib h has_arg ->
          match h, has_arg with
            | -152839522, true -> (`SCtrl (
                (
                  read_storage_control
                ) ib
              ))
            | 430795148, true -> (`ACtrl (
                (
                  read_access_control
                ) ib
              ))
            | 3250114, true -> (`AOp (
                (
                  read_access_operation
                ) ib
              ))
            | _ -> Atdgen_runtime.Ob_run.unsupported_variant h has_arg
        )
)
let read_request = (
  fun ib ->
    if Bi_io.read_tag ib <> 23 then Atdgen_runtime.Ob_run.read_error_at ib;
    Bi_io.read_hashtag ib (fun ib h has_arg ->
      match h, has_arg with
        | -152839522, true -> (`SCtrl (
            (
              read_storage_control
            ) ib
          ))
        | 430795148, true -> (`ACtrl (
            (
              read_access_control
            ) ib
          ))
        | 3250114, true -> (`AOp (
            (
              read_access_operation
            ) ib
          ))
        | _ -> Atdgen_runtime.Ob_run.unsupported_variant h has_arg
    )
)
let request_of_string ?pos s =
  read_request (Bi_inbuf.from_string ?pos s)
let _2_tag = Bi_io.array_tag
let write_untagged__2 = (
  Atdgen_runtime.Ob_run.write_untagged_list
    tuple_tag
    (
      write_untagged_tuple
    )
)
let write__2 ob x =
  Bi_io.write_tag ob Bi_io.array_tag;
  write_untagged__2 ob x
let string_of__2 ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write__2 ob x;
  Bi_outbuf.contents ob
let get__2_reader = (
  Atdgen_runtime.Ob_run.get_list_reader (
    get_tuple_reader
  )
)
let read__2 = (
  Atdgen_runtime.Ob_run.read_list (
    get_tuple_reader
  )
)
let _2_of_string ?pos s =
  read__2 (Bi_inbuf.from_string ?pos s)
let notification_tag = Bi_io.record_tag
let write_untagged_notification : Bi_outbuf.t -> notification -> unit = (
  fun ob x ->
    Bi_vint.write_uvint ob 2;
    Bi_outbuf.add_char4 ob '\128' 'W' '\159' '\014';
    (
      Bi_io.write_svint
    ) ob x.sid;
    Bi_outbuf.add_char4 ob '\239' '(' '\165' '\004';
    (
      write__2
    ) ob x.elems;
)
let write_notification ob x =
  Bi_io.write_tag ob Bi_io.record_tag;
  write_untagged_notification ob x
let string_of_notification ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write_notification ob x;
  Bi_outbuf.contents ob
let get_notification_reader = (
  fun tag ->
    if tag <> 21 then Atdgen_runtime.Ob_run.read_error () else
      fun ib ->
        let field_sid = ref (Obj.magic (Sys.opaque_identity 0.0)) in
        let field_elems = ref (Obj.magic (Sys.opaque_identity 0.0)) in
        let bits0 = ref 0 in
        let len = Bi_vint.read_uvint ib in
        for i = 1 to len do
          match Bi_io.read_field_hashtag ib with
            | 5742350 ->
              field_sid := (
                (
                  Atdgen_runtime.Ob_run.read_int
                ) ib
              );
              bits0 := !bits0 lor 0x1;
            | -282548988 ->
              field_elems := (
                (
                  read__2
                ) ib
              );
              bits0 := !bits0 lor 0x2;
            | _ -> Bi_io.skip ib
        done;
        if !bits0 <> 0x3 then Atdgen_runtime.Ob_run.missing_fields [| !bits0 |] [| "sid"; "elems" |];
        (
          {
            sid = !field_sid;
            elems = !field_elems;
          }
         : notification)
)
let read_notification = (
  fun ib ->
    if Bi_io.read_tag ib <> 21 then Atdgen_runtime.Ob_run.read_error_at ib;
    let field_sid = ref (Obj.magic (Sys.opaque_identity 0.0)) in
    let field_elems = ref (Obj.magic (Sys.opaque_identity 0.0)) in
    let bits0 = ref 0 in
    let len = Bi_vint.read_uvint ib in
    for i = 1 to len do
      match Bi_io.read_field_hashtag ib with
        | 5742350 ->
          field_sid := (
            (
              Atdgen_runtime.Ob_run.read_int
            ) ib
          );
          bits0 := !bits0 lor 0x1;
        | -282548988 ->
          field_elems := (
            (
              read__2
            ) ib
          );
          bits0 := !bits0 lor 0x2;
        | _ -> Bi_io.skip ib
    done;
    if !bits0 <> 0x3 then Atdgen_runtime.Ob_run.missing_fields [| !bits0 |] [| "sid"; "elems" |];
    (
      {
        sid = !field_sid;
        elems = !field_elems;
      }
     : notification)
)
let notification_of_string ?pos s =
  read_notification (Bi_inbuf.from_string ?pos s)
let reply_tag = Bi_io.variant_tag
let write_untagged_reply = (
  fun ob x ->
    match x with
      | `Ok -> Bi_outbuf.add_char4 ob '\000' '\000' 'E' '<'
      | `Error x ->
        Bi_outbuf.add_char4 ob '\134' 'W' ';' '\168';
        (
          Bi_io.write_svint
        ) ob x
      | `Values x ->
        Bi_outbuf.add_char4 ob '\132' 'K' 'x' '\002';
        (
          write__2
        ) ob x
      | `Notice x ->
        Bi_outbuf.add_char4 ob '\230' 'M' 'L' '8';
        (
          write_notification
        ) ob x
      | `SubId x ->
        Bi_outbuf.add_char4 ob '\151' '\226' '\185' '[';
        (
          Bi_io.write_svint
        ) ob x
)
let write_reply ob x =
  Bi_io.write_tag ob Bi_io.variant_tag;
  write_untagged_reply ob x
let string_of_reply ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write_reply ob x;
  Bi_outbuf.contents ob
let get_reply_reader = (
  fun tag ->
    if tag <> 23 then Atdgen_runtime.Ob_run.read_error () else
      fun ib ->
        Bi_io.read_hashtag ib (fun ib h has_arg ->
          match h, has_arg with
            | 17724, false -> `Ok
            | 106380200, true -> (`Error (
                (
                  Atdgen_runtime.Ob_run.read_int
                ) ib
              ))
            | 72054786, true -> (`Values (
                (
                  read__2
                ) ib
              ))
            | -431141832, true -> (`Notice (
                (
                  read_notification
                ) ib
              ))
            | 400734555, true -> (`SubId (
                (
                  Atdgen_runtime.Ob_run.read_int
                ) ib
              ))
            | _ -> Atdgen_runtime.Ob_run.unsupported_variant h has_arg
        )
)
let read_reply = (
  fun ib ->
    if Bi_io.read_tag ib <> 23 then Atdgen_runtime.Ob_run.read_error_at ib;
    Bi_io.read_hashtag ib (fun ib h has_arg ->
      match h, has_arg with
        | 17724, false -> `Ok
        | 106380200, true -> (`Error (
            (
              Atdgen_runtime.Ob_run.read_int
            ) ib
          ))
        | 72054786, true -> (`Values (
            (
              read__2
            ) ib
          ))
        | -431141832, true -> (`Notice (
            (
              read_notification
            ) ib
          ))
        | 400734555, true -> (`SubId (
            (
              Atdgen_runtime.Ob_run.read_int
            ) ib
          ))
        | _ -> Atdgen_runtime.Ob_run.unsupported_variant h has_arg
    )
)
let reply_of_string ?pos s =
  read_reply (Bi_inbuf.from_string ?pos s)
let message_tag = Bi_io.record_tag
let write_untagged_message : Bi_outbuf.t -> message -> unit = (
  fun ob x ->
    Bi_vint.write_uvint ob 2;
    Bi_outbuf.add_char4 ob '\128' 'S' '\017' '\136';
    (
      Bi_io.write_svint
    ) ob x.mid;
    Bi_outbuf.add_char4 ob '\128' 'S' '\026' 'A';
    (
      fun ob x ->
        Bi_io.write_tag ob Bi_io.variant_tag;
        match x with
          | `Request x ->
            Bi_outbuf.add_char4 ob '\145' '\214' '\007' '/';
            (
              write_request
            ) ob x
          | `Reply x ->
            Bi_outbuf.add_char4 ob '\249' '\243' 'y' '\n';
            (
              write_reply
            ) ob x
    ) ob x.msg;
)
let write_message ob x =
  Bi_io.write_tag ob Bi_io.record_tag;
  write_untagged_message ob x
let string_of_message ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write_message ob x;
  Bi_outbuf.contents ob
let get_message_reader = (
  fun tag ->
    if tag <> 21 then Atdgen_runtime.Ob_run.read_error () else
      fun ib ->
        let field_mid = ref (Obj.magic (Sys.opaque_identity 0.0)) in
        let field_msg = ref (Obj.magic (Sys.opaque_identity 0.0)) in
        let bits0 = ref 0 in
        let len = Bi_vint.read_uvint ib in
        for i = 1 to len do
          match Bi_io.read_field_hashtag ib with
            | 5443976 ->
              field_mid := (
                (
                  Atdgen_runtime.Ob_run.read_int
                ) ib
              );
              bits0 := !bits0 lor 0x1;
            | 5446209 ->
              field_msg := (
                (
                  fun ib ->
                    if Bi_io.read_tag ib <> 23 then Atdgen_runtime.Ob_run.read_error_at ib;
                    Bi_io.read_hashtag ib (fun ib h has_arg ->
                      match h, has_arg with
                        | 299239215, true -> (`Request (
                            (
                              read_request
                            ) ib
                          ))
                        | -101484278, true -> (`Reply (
                            (
                              read_reply
                            ) ib
                          ))
                        | _ -> Atdgen_runtime.Ob_run.unsupported_variant h has_arg
                    )
                ) ib
              );
              bits0 := !bits0 lor 0x2;
            | _ -> Bi_io.skip ib
        done;
        if !bits0 <> 0x3 then Atdgen_runtime.Ob_run.missing_fields [| !bits0 |] [| "mid"; "msg" |];
        (
          {
            mid = !field_mid;
            msg = !field_msg;
          }
         : message)
)
let read_message = (
  fun ib ->
    if Bi_io.read_tag ib <> 21 then Atdgen_runtime.Ob_run.read_error_at ib;
    let field_mid = ref (Obj.magic (Sys.opaque_identity 0.0)) in
    let field_msg = ref (Obj.magic (Sys.opaque_identity 0.0)) in
    let bits0 = ref 0 in
    let len = Bi_vint.read_uvint ib in
    for i = 1 to len do
      match Bi_io.read_field_hashtag ib with
        | 5443976 ->
          field_mid := (
            (
              Atdgen_runtime.Ob_run.read_int
            ) ib
          );
          bits0 := !bits0 lor 0x1;
        | 5446209 ->
          field_msg := (
            (
              fun ib ->
                if Bi_io.read_tag ib <> 23 then Atdgen_runtime.Ob_run.read_error_at ib;
                Bi_io.read_hashtag ib (fun ib h has_arg ->
                  match h, has_arg with
                    | 299239215, true -> (`Request (
                        (
                          read_request
                        ) ib
                      ))
                    | -101484278, true -> (`Reply (
                        (
                          read_reply
                        ) ib
                      ))
                    | _ -> Atdgen_runtime.Ob_run.unsupported_variant h has_arg
                )
            ) ib
          );
          bits0 := !bits0 lor 0x2;
        | _ -> Bi_io.skip ib
    done;
    if !bits0 <> 0x3 then Atdgen_runtime.Ob_run.missing_fields [| !bits0 |] [| "mid"; "msg" |];
    (
      {
        mid = !field_mid;
        msg = !field_msg;
      }
     : message)
)
let message_of_string ?pos s =
  read_message (Bi_inbuf.from_string ?pos s)
let access_operator_tag = Bi_io.record_tag
let write_untagged_access_operator : Bi_outbuf.t -> access_operator -> unit = (
  fun ob x ->
    Bi_vint.write_uvint ob 2;
    Bi_outbuf.add_char4 ob '\128' '\000' '[' '\219';
    (
      Bi_io.write_string
    ) ob x.id;
    Bi_outbuf.add_char4 ob '\128' '\000' 'a' '!';
    (
      write_access_operation
    ) ob x.op;
)
let write_access_operator ob x =
  Bi_io.write_tag ob Bi_io.record_tag;
  write_untagged_access_operator ob x
let string_of_access_operator ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write_access_operator ob x;
  Bi_outbuf.contents ob
let get_access_operator_reader = (
  fun tag ->
    if tag <> 21 then Atdgen_runtime.Ob_run.read_error () else
      fun ib ->
        let field_id = ref (Obj.magic (Sys.opaque_identity 0.0)) in
        let field_op = ref (Obj.magic (Sys.opaque_identity 0.0)) in
        let bits0 = ref 0 in
        let len = Bi_vint.read_uvint ib in
        for i = 1 to len do
          match Bi_io.read_field_hashtag ib with
            | 23515 ->
              field_id := (
                (
                  Atdgen_runtime.Ob_run.read_string
                ) ib
              );
              bits0 := !bits0 lor 0x1;
            | 24865 ->
              field_op := (
                (
                  read_access_operation
                ) ib
              );
              bits0 := !bits0 lor 0x2;
            | _ -> Bi_io.skip ib
        done;
        if !bits0 <> 0x3 then Atdgen_runtime.Ob_run.missing_fields [| !bits0 |] [| "id"; "op" |];
        (
          {
            id = !field_id;
            op = !field_op;
          }
         : access_operator)
)
let read_access_operator = (
  fun ib ->
    if Bi_io.read_tag ib <> 21 then Atdgen_runtime.Ob_run.read_error_at ib;
    let field_id = ref (Obj.magic (Sys.opaque_identity 0.0)) in
    let field_op = ref (Obj.magic (Sys.opaque_identity 0.0)) in
    let bits0 = ref 0 in
    let len = Bi_vint.read_uvint ib in
    for i = 1 to len do
      match Bi_io.read_field_hashtag ib with
        | 23515 ->
          field_id := (
            (
              Atdgen_runtime.Ob_run.read_string
            ) ib
          );
          bits0 := !bits0 lor 0x1;
        | 24865 ->
          field_op := (
            (
              read_access_operation
            ) ib
          );
          bits0 := !bits0 lor 0x2;
        | _ -> Bi_io.skip ib
    done;
    if !bits0 <> 0x3 then Atdgen_runtime.Ob_run.missing_fields [| !bits0 |] [| "id"; "op" |];
    (
      {
        id = !field_id;
        op = !field_op;
      }
     : access_operator)
)
let access_operator_of_string ?pos s =
  read_access_operator (Bi_inbuf.from_string ?pos s)
let create_tuple 
  ~key
  ~value
  () : tuple =
  {
    key = key;
    value = value;
  }
let create_storage_info 
  ~id
  ~path
  () : storage_info =
  {
    id = id;
    path = path;
  }
let create_access_info 
  ~id
  ~path
  ~cache_size
  () : access_info =
  {
    id = id;
    path = path;
    cache_size = cache_size;
  }
let create_notification 
  ~sid
  ~elems
  () : notification =
  {
    sid = sid;
    elems = elems;
  }
let create_message 
  ~mid
  ~msg
  () : message =
  {
    mid = mid;
    msg = msg;
  }
let create_access_operator 
  ~id
  ~op
  () : access_operator =
  {
    id = id;
    op = op;
  }
