open Result

module String      = Helpers.String
module Http_client = Nethttp_client

let sprintf = Printf.sprintf

module type CONFIG = sig
  val server : string
  val port : int
  val auth : (string * string) option
end

module type API =
sig
  type path = string

  type 'a call

  type error =
    | Error_rsp of Nethttp.http_status * Yojson.Safe.json option
    | Invalid_json of string
    | Malformed_rsp of string
    | Unexpected of string

  type 'a result = ('a, error) Result.t

  val and_then : ('a -> 'b result) -> 'a call -> 'b call

  val batch : (int * 'a call) list -> (int * 'a) list call

  val execute : 'a call -> ('a, error) Result.t

  val version : string call

  module Node :
  sig
    type id = int

    type t = {
      id : id;
      labels : string list;
      properties : (string * string) list;
    }

    val create : ?properties:(string * string) list -> unit -> t call

    val get : int -> t call

    val delete : int -> t call
  end

  module Label :
  sig
    type t = string

    val add : int -> t list -> unit call

    val replace : int -> t list -> unit call

    val remove : int -> t -> unit call

    val get : int -> t list call

    val get_nodes : ?properties:(string * string) list -> t -> Node.t list call

    val list : t list call
  end

  module Relationship :
  sig
    type id = int
    type typ = string

    type t = {
      id : id;
      typ : typ
    }

    val get : id -> t call

    val create : ?properties:(string * string) list -> from:Node.id -> Node.id -> typ
      -> t call

    val delete : id -> unit call

    val properties : id -> (string * string) list call

    val set_properties : id -> (string * string) list -> unit call

    val property : id -> string -> string call

    val set_property : id -> string -> string -> unit call

    val of_node : ?types:string list -> Node.id -> [`In_out | `In | `Out] -> t list call
  end

  module Cypher :
  sig
    type stmt = string
    type param = (string * Yojson.Safe.json) list
    type query = stmt * param * [`Stats | `REST | `Row | `Graph] list

    type stats = {
      contains_updates : bool;
      nodes_created : int;
      nodes_deleted : int;
      properties_set : int;
      relationships_created : int;
      relationship_deleted : int;
      labels_added : int;
      labels_removed : int;
      indexes_added : int;
      indexes_removed : int;
      constraints_added : int;
      constraints_removed : int;
    }

    type data = {
      row : Yojson.Safe.json list option;
      graph : Yojson.Safe.json option;
      rest : Yojson.Safe.json option;
    }

    type result = {
      columns : string list;
      data : data list;
      stats : stats option;
    }

    type error = {
      code : string;
      message : string
    }

    type rsp = result list * error list

    type transaction

    val execute : query list -> rsp call

    val begin_transaction : query list -> (rsp * transaction) call

    val execute_in_transaction :
      query list -> transaction -> (rsp * transaction) call

    val reset_timeout : transaction -> transaction call

    val commit : query list -> transaction -> rsp call

    val rollback : transaction -> rsp call

    val delete_all_relationships : query

    val delete_all_nodes : query
  end

  val _get : path
    -> (string option -> Yojson.Safe.json option -> 'a result) -> 'a call

  val _delete : path
    -> (string option -> Yojson.Safe.json option -> 'a result) -> 'a call

  val _post :
    path -> ?data:Yojson.Safe.json
    -> (string option -> Yojson.Safe.json option -> 'a result) -> 'a call

  val _put :
    path -> ?data:Yojson.Safe.json
    -> (string option -> Yojson.Safe.json option -> 'a result) -> 'a call
end

module Make(Cfg: CONFIG) : API =
struct
  (* Paths should be relative to data-API URI. For instance, the path string of
     "http://localhost:7474/db/data/node" should be "node" (no leading '/'). *)
  type path = string

  type error =
    | Error_rsp of Nethttp.http_status * Yojson.Safe.json option
    | Invalid_json of string
    | Malformed_rsp of string
    | Unexpected of string

  type 'a result = ('a, error) Result.t

  module Json = struct
    include Yojson.Safe

    let drop_assoc = function `Assoc xs -> xs | _ -> failwith "Bad argument"
    let drop_string = function `String s -> s | _ -> failwith "Bad argument"
    let unwrap_res x = x |> drop_assoc |> List.assoc "data"

    let from_string json =
      try
        OK (from_string json)
      with
      | Yojson.Json_error m -> Error (Malformed_rsp m)

    let some_of = function
      | Some x -> OK x
      | None -> Error (Invalid_json "expects something")

    let int = function
      | `Int i -> OK i
      | _ -> Error (Invalid_json "expects int")

    let bool = function
      | `Bool b -> OK b
      | _ -> Error (Invalid_json "expects bool")

    let string : json -> string result = function
      | `String s -> OK s
      | _ -> Error (Invalid_json "expects string")

    let list : json -> json list result = function
      | `List l -> OK l
      | _ -> Error (Invalid_json "expects list")

    let assoc = function
      | `Assoc assoc -> OK assoc
      | _ -> Error (Invalid_json "expects object")

    let field ?def f obj : json result =
      try
        OK (List.assoc f obj)
      with
      | Not_found ->
          match def with
          | None   -> Error (Invalid_json ("field not found: " ^ f))
          | Some d -> OK d

    let opt_field f obj : json option result =
      try
        OK (Some (List.assoc f obj))
      with
      | Not_found -> OK None

    let nil = `List []

    let empty = `Assoc []

    let lmap : ('a -> 'b result) -> 'a list -> 'b list result =
      let rec aux revl f = function
        | []     -> OK (List.rev revl)
        | h :: t -> f h >>= (fun x -> aux (x :: revl) f t)
      in
      fun f l -> aux [] f l

    let vmap f l = lmap (fun (k, v) -> f v >>= (fun v -> OK (k, v))) l

    let omap f = function
      | None   -> OK None
      | Some x -> f x >>= (fun y -> OK (Some y))
  end

  type 'a callback = string option -> Json.json option -> ('a, error) Result.t

  type 'a call =
    | Get of path * 'a callback
    | Delete of path * 'a callback
    | Post of path * Json.json option * 'a callback
    | Put of path * Json.json option * 'a callback

  let and_then m = function
    | Get (p, f)     -> Get (p, (fun l j -> f l j >>= m))
    | Delete (p, f)  -> Delete (p, (fun l j -> f l j >>= m))
    | Post (p, d, f) -> Post (p, d, (fun l j -> f l j >>= m))
    | Put (p, d, f)  -> Put (p, d, (fun l j -> f l j >>= m))

  let _get path f = Get (path, f)
  let _delete path f = Delete (path, f)
  let _post path ?data f = Post (path, data, f)
  let _put path ?data f = Put (path, data, f)

  let batch l =
    let (rev_jsons, callbacks) =
      List.fold_left (fun (rev_jsons, callbacks) (i, call) ->
          let (mthd, path, body_opt, callback) =
            match call with
            | Get (p, f)     -> ("GET", p, None, f)
            | Delete (p, f)  -> ("DELETE", p, None, f)
            | Post (p, b, f) -> ("POST", p, b, f)
            | Put (p, b, f)  -> ("PUT", p, b, f)
          in
          let json =
            `Assoc (
              [("method", `String mthd); ("to", `String ("/" ^ path))] @ (
                match body_opt with
                | None   -> []
                | Some b -> [("body", b)]
              ) @ [("id"), `Int i]
            )
          in
          (json :: rev_jsons, (i, callback) :: callbacks)
        )
        ([], [])
        l
    in
    let callback l json =
      Json.(some_of json >>= list >>= lmap (fun rsp ->
          assoc rsp
          >>= (fun rsp -> field "id" rsp >>= int
          >>= (fun id -> field "body" rsp
          >>= (fun body ->
                (List.assoc id callbacks) l (Some body) >>= (fun x -> OK (id, x))
        )))))
    in
    Post ("batch", Some (`List (List.rev rev_jsons)), callback)

  let auth =
    match Cfg.auth with
    | None              -> None
    | Some (user, pass) -> Some (Netencoding.Base64.encode (user ^ ":" ^ pass))

  let get_uri path = sprintf "http://%s:%d/db/data/%s" Cfg.server Cfg.port path

  let set_req_header call =
    call#set_req_header "Accept"       "application/json; charset=UTF8";
    call#set_req_header "Content-type" "application/json";
    match auth with
    | None         -> ()
    | Some payload -> call#set_req_header "Authorization" ("Basic " ^ payload)

  let execute r =
    let (req, f) =
      match r with
      | Get (path, f) ->
          let req = new Http_client.get (get_uri path) in
          (req, f)

      | Delete (path, f) ->
          let req = new Http_client.delete (get_uri path) in
          (req, f)

      | Post (path, json, f) ->
          let data =
            match json with
            | None      -> ""
            | Some json -> Json.to_string json
          in
          let req = new Http_client.post_raw (get_uri path) data in
          (req, f)

      | Put (path, json, f) ->
          let data =
            match json with
            | None      -> ""
            | Some json -> Json.to_string json
          in
          let req = new Http_client.put (get_uri path) data in
          (req, f)
    in
    set_req_header req;
    let pipeline = new Http_client.pipeline in
    pipeline#add req;
    pipeline#run ();
    let v = req#response_body#value in
    let l = try Some (req#response_header#field "Location") with Not_found -> None in
    match req#response_status with
    | `Ok | `Created | `No_content ->
        if String.length v = 0 then
          f l None
        else
          Json.from_string v >>= (fun json -> f l (Some json))

    | s ->
        if String.length v = 0 then
          Error (Error_rsp (s, None))
        else
          Json.from_string v >>= (fun json -> Error (Error_rsp (s, Some json)))

  let version =
    _get "" (fun _ json ->
        Json.(some_of json >>= assoc >>= field "neo4j_version" >>= string >>= return)
      )

  module Node =
  struct
    type id = int

    type t = {
      id : id;
      labels : string list;
      properties : (string * string) list;
    }

    let from_json json =
      let open Json in
      assoc json
      >>= (fun top -> field "metadata" top >>= assoc
      >>= (fun metadata -> field "id" metadata >>= int
      >>= (fun id -> field ~def:nil "labels" metadata >>= list >>= lmap string
      >>= (fun labels -> field ~def:empty "data" top >>= assoc >>= vmap string
      >>= (fun properties ->
            OK { id; labels; properties }
      )))))

    let from_rsp _ json =
      Json.some_of json >>= from_json

    let create ?(properties = []) () =
      let data =
        match properties with
        | [] -> None
        | ps -> Some (`Assoc (List.map (fun (k, v) -> (k, `String v)) ps))
      in
      _post "node" ?data from_rsp

    let get id =
      _get (sprintf "node/%d/" id) from_rsp

    let delete id =
      _delete (sprintf "node/%d/" id) from_rsp
  end

  module Label =
  struct
    type t = string

    let labels_from_rsp =
      Json.(fun _ json -> some_of json >>= list >>= lmap string)

    let labels_to_json labels =
        `List (
          List.map (fun l ->
              if String.length l = 0 then invalid_arg "empty label" else `String l
            ) labels
        )

    let add id labels =
      _post (sprintf "node/%d/labels" id) ~data:(labels_to_json labels)
        (fun _ _ -> OK ())

    let replace id labels =
      _put (sprintf "node/%d/labels" id) ~data:(labels_to_json labels)
        (fun _ _ -> OK ())

    let remove id label =
      _delete (sprintf "node/%d/labels/%s" id label)
        (fun _ _ -> OK ())

    let get id =
      _get (sprintf "node/%d/labels" id) labels_from_rsp

    let get_nodes ?properties label = (* TODO: with properties *)
      _get (sprintf "label/%s/nodes" label)
        Json.(fun _ json -> some_of json >>= list >>= lmap Node.from_json)

    let list =
      _get (sprintf "labels") labels_from_rsp
  end

  module Relationship =
  struct
    type id = int
    type typ = string

    type t = {
      id : id;
      typ : typ
    }

    let from_json json =
      let open Json in
      assoc json
      >>= (fun top -> field "metadata" top >>= assoc
      >>= (fun metadata -> field "id" metadata >>= int
      >>= (fun id -> field "type" metadata >>= string
      >>= (fun typ ->
            OK { id; typ }
      ))))

    let from_rsp _ json =
      Json.some_of json >>= from_json

    let get id =
      _get (sprintf "relationship/%d" id) from_rsp

    let create ?properties ~from _to typ =
      let data =
        `Assoc ([
            ("to", `String (get_uri (sprintf "node/%d" _to)));
            ("type", `String typ);
          ] @ (
              match properties with
              | None    -> []
              | Some ps ->
                  [("data", `Assoc (List.map (fun (k, v) -> (k, `String v)) ps))]
            ))
      in
      _post (sprintf "node/%d/relationships" from) ~data from_rsp

    let delete id =
      _delete (sprintf "relationship/%d" id) (fun _ _ -> OK ())

    let properties id =
      _get (sprintf "relationship/%d/properties" id)
        Json.(fun _ json -> some_of json >>= assoc >>= vmap string)

    let set_properties id properties =
      _put (sprintf "relationship/%d/properties" id)
        ~data:(`Assoc (List.map (fun (k, v) -> (k, `String v)) properties))
        (fun _ _ -> OK ())

    let property id p =
      _get (sprintf "relationship/%d/properties/%s" id p)
        Json.(fun _ json -> some_of json >>= string)

    let set_property id k v =
      _put (sprintf "relationship/%d/properties/%s" id k)
        ~data:(`String v)
        Json.(fun _ _ -> OK ())

    let of_node ?(types = []) id t =
      let t =
        match t with
        | `In_out -> "all"
        | `In     -> "in"
        | `Out    -> "out"
      in
      let types =
        match types with
        | [] -> ""
        | _  -> "/" ^ String.concat "&" types
      in
      _get (sprintf "node/%d/relationships/%s%s" id t types)
        Json.(fun _ json -> some_of json >>= list >>= lmap from_json)
  end

  module Cypher =
  struct
    type stmt = string
    type param = (string * Json.json) list
    type query = stmt * param * [`Stats | `REST | `Row | `Graph] list

    type error = {
      code : string;
      message : string
    }

    let error_from_json json =
      let open Json in
      assoc json
      >>= (fun obj -> field "code" obj >>= string
      >>= (fun code -> field "message" obj >>= string
      >>= (fun message -> OK { code; message })))

    type stats = {
      contains_updates : bool;
      nodes_created : int;
      nodes_deleted : int;
      properties_set : int;
      relationships_created : int;
      relationship_deleted : int;
      labels_added : int;
      labels_removed : int;
      indexes_added : int;
      indexes_removed : int;
      constraints_added : int;
      constraints_removed : int;
    }

    type data = {
      row : Json.json list option;
      graph : Json.json option;
      rest : Json.json option;
    }

    type result = {
      columns : string list;
      data : data list;
      stats : stats option;
    }

    type rsp = result list * error list

    let data_from_json json =
      let open Json in
      assoc json
      >>= (fun obj -> opt_field "row" obj >>= omap list
      >>= (fun row -> opt_field "graph" obj
      >>= (fun graph -> opt_field "rest" obj
      >>= (fun rest -> OK { row; graph; rest }))))

    let stats_from_json json =
      let open Json in
      assoc json
      >>= (fun obj -> field "contains_updates" obj >>= bool
      >>= (fun contains_updates -> field "nodes_created" obj >>= int
      >>= (fun nodes_created -> field "nodes_deleted" obj >>= int
      >>= (fun nodes_deleted -> field "properties_set" obj >>= int
      >>= (fun properties_set -> field "relationships_created" obj >>= int
      >>= (fun relationships_created -> field "relationship_deleted" obj >>= int
      >>= (fun relationship_deleted -> field "labels_added" obj >>= int
      >>= (fun labels_added -> field "labels_removed" obj >>= int
      >>= (fun labels_removed -> field "indexes_added" obj >>= int
      >>= (fun indexes_added -> field "indexes_removed" obj >>= int
      >>= (fun indexes_removed -> field "constraints_added" obj >>= int
      >>= (fun constraints_added -> field "constraints_removed" obj >>= int
      >>= (fun constraints_removed -> OK {
              contains_updates;
              nodes_created;
              nodes_deleted;
              properties_set;
              relationships_created;
              relationship_deleted;
              labels_added;
              labels_removed;
              indexes_added;
              indexes_removed;
              constraints_added;
              constraints_removed;
            }
          )))))))))))))

    let result_from_json json =
      let open Json in
      assoc json
      >>= (fun obj -> field "columns" obj >>= list >>= lmap string
      >>= (fun columns -> field "data" obj >>= list >>= lmap data_from_json
      >>= (fun data -> opt_field "stats" obj >>= omap stats_from_json
      >>= (fun stats -> OK { columns; data; stats }))))

    type transaction = {
      tid : string;
      expires : string;
    }

    let rsp_from_json _ json =
      let open Json in
      some_of json >>= assoc
      >>= (fun rsp -> field "results" rsp >>= list >>= lmap result_from_json
      >>= (fun results -> field "errors" rsp >>= list >>= lmap error_from_json
      >>= (fun errors -> OK (results, errors))))

    let rsp_transaction_from_json t _ json =
      let open Json in
      some_of json >>= assoc
      >>= (fun rsp -> field "results" rsp >>= list >>= lmap result_from_json
      >>= (fun results -> field "errors" rsp >>= list >>= lmap error_from_json
      >>= (fun errors -> field "transaction" rsp
      >>= assoc >>= field "expires" >>= string
      >>= (fun expires -> OK ((results, errors), { t with expires })))))

    let query_to_json (stmt, params, contents) =
      let (includeStats, resultDataContents) =
        List.fold_left (fun (i, c) -> function
            | `Stats -> (true, c)
            | `Row   -> (i, `String "row" :: c)
            | `Graph -> (i, `String "graph" :: c)
            | `REST  -> (i, `String "REST" :: c)
          )
          (false, [])
          contents
      in
      let q =
        [("statement", `String stmt); ("includeStats", `Bool includeStats)]
      in
      let q =
        if resultDataContents = [] then q
        else q @ [("resultDataContents", `List resultDataContents)]
      in
      let q =
        if params = [] then q else q @ [("parameters", `Assoc params)]
      in
      `Assoc q

    let queries_to_json qs =
      `Assoc [
        ("statements", `List (List.map query_to_json qs))
      ]

    let execute qs =
      let data = queries_to_json qs in
      _post (sprintf "transaction/commit") ~data rsp_from_json

    let begin_transaction qs =
      let data = queries_to_json qs in
      _post (sprintf "transaction") ~data Json.(fun loc json ->
          match loc with
          | None -> Error (Malformed_rsp "no location header?")
          | Some loc ->
              (* Location: .../transaction/<id> *)
              let i = String.rindex loc '/' + 1 in
              let t = {
                tid = String.sub loc i (String.length loc - i);
                expires = ""
              } in
              rsp_transaction_from_json t loc json
        )

    let execute_in_transaction qs t =
      let data = queries_to_json qs in
      _post (sprintf "transaction/%s" t.tid) ~data (rsp_transaction_from_json t)

    let reset_timeout t =
      and_then (fun (_, t) -> OK t) (execute_in_transaction [] t)

    let commit qs t =
      let data = queries_to_json qs in
      _post (sprintf "transaction/%s/commit" t.tid) ~data rsp_from_json

    let rollback t =
      _delete (sprintf "transaction/%s" t.tid) rsp_from_json

    let delete_all_relationships =
      ("START r=rel(*)  DELETE r;", [], [])

    let delete_all_nodes =
      ("START n=node(*) DELETE n;", [], [])
  end
end

(*
  Local variables:
  compile-command: "make -C .."
  End:
 *)
