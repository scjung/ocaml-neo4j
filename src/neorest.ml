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
    | Invalid_json of string * Yojson.Safe.json
    | Malformed_rsp of string
    | Unexpected of string

  val error_to_string : error -> string

  type 'a result = ('a, error) Result.t

  val map_rsp : ('a -> 'b result) -> 'a call -> 'b call

  val ignore_rsp : 'a call -> unit call

  val batch : (int * 'a call) list -> (int * 'a) list call

  val execute : 'a call -> ('a, error) Result.t

  val root : (string * Yojson.Safe.json) list call

  val version : string call

  module Property :
  sig
    type k = string

    type v = [
      | `Bool of bool
      | `Float of float
      | `Int of int
      | `Intlit of string
      | `String of string
      | `List_bool of bool list
      | `List_float of float list
      | `List_int of int list
      | `List_intlit of string list
      | `List_string of string list
    ]

    type kv = k * v

    type t = kv list

    val v_from_json : Yojson.Safe.json -> v result

    val from_json : Yojson.Safe.json -> t result

    val v_to_json : v -> Yojson.Safe.json

    val to_json : t -> Yojson.Safe.json

    val keys : unit -> k list call
  end

  module Node :
  sig
    type id = int

    type t = {
      id : id;
      labels : string list;
      property : Property.t
    }

    val create : ?property:Property.t -> unit -> t call

    val get : id -> t call

    val delete : id -> t call

    val set_property : id -> Property.k -> Property.v -> unit call

    val update_properties : id -> Property.t -> unit call

    val get_properties : id -> Property.t call

    val get_property : id -> Property.k -> Property.v call

    val delete_properties : id -> unit call

    val delete_property : id -> Property.k -> unit call

    val degree : ?types:string list -> id -> [`In_out | `In | `Out] -> int call
  end

  module Label :
  sig
    type t = string

    val add : Node.id -> t list -> unit call

    val replace : Node.id -> t list -> unit call

    val remove : Node.id -> t -> unit call

    val get : Node.id -> t list call

    val get_nodes : ?property:Property.kv -> t -> Node.t list call

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

    val create : ?property:Property.t -> from:Node.id -> Node.id -> typ
      -> t call

    val delete : id -> unit call

    val properties : id -> Property.t call

    val set_properties : id -> Property.t -> unit call

    val delete_properties : id -> unit call

    val delete_property : id -> Property.k -> unit call

    val property : id -> Property.k -> Property.v call

    val set_property : id -> Property.k -> Property.v -> unit call

    val of_node : ?types:string list -> Node.id -> [`In_out | `In | `Out] -> t list call

    val types : unit -> string list call
  end

  module Cypher :
  sig
    type stmt = [`String of string | `Ast of Neo4j_cypher.statement]
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

  val debug : bool ref

  val print_debug : (string -> unit) ref
end

module Make(Cfg: CONFIG) : API =
struct
  let debug = ref false

  let print_debug = ref prerr_endline

  (* Paths should be relative to data-API URI. For instance, the path string of
     "http://localhost:7474/db/data/node" should be "node" (no leading '/'). *)
  type path = string

  type error =
    | Error_rsp of Nethttp.http_status * Yojson.Safe.json option
    | Invalid_json of string * Yojson.Safe.json
    | Malformed_rsp of string
    | Unexpected of string

  let error_to_string = function
    | Error_rsp (s, None) -> "Error_rsp: " ^ Nethttp.string_of_http_status s

    | Error_rsp (s, Some j) ->
        "Error_rsp: " ^ Nethttp.string_of_http_status s ^ ": "
        ^ Yojson.Safe.to_string j

    | Invalid_json (s, j) ->
        "Invalid_json: " ^ s ^ ": " ^ Yojson.Safe.to_string j

    | Malformed_rsp s ->
        "Malformed_rsp: " ^ s

    | Unexpected s ->
        "Unexpected: " ^ s

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
      | None -> Error (Malformed_rsp "no json returned")

    let int = function
      | `Int i -> OK i
      | j -> Error (Invalid_json ("expects int", j))

    let bool = function
      | `Bool b -> OK b
      | j -> Error (Invalid_json ("expects bool", j))

    let string : json -> string result = function
      | `String s -> OK s
      | j -> Error (Invalid_json ("expects string", j))

    let list : json -> json list result = function
      | `List l -> OK l
      | j -> Error (Invalid_json ("expects list", j))

    let assoc = function
      | `Assoc assoc -> OK assoc
      | j -> Error (Invalid_json ("expects object", j))

    let field ?def f obj : json result =
      try
        OK (List.assoc f obj)
      with
      | Not_found ->
          match def with
          | None   -> Error (Invalid_json ("field not found: " ^ f, `Assoc obj))
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

  let map_rsp m = function
    | Get (p, f)     -> Get (p, (fun l j -> f l j >>= m))
    | Delete (p, f)  -> Delete (p, (fun l j -> f l j >>= m))
    | Post (p, d, f) -> Post (p, d, (fun l j -> f l j >>= m))
    | Put (p, d, f)  -> Put (p, d, (fun l j -> f l j >>= m))

  let ignore_rsp c = map_rsp (fun _ -> OK ()) c

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
          let uri = get_uri path in
          if !debug then !print_debug ("GET: " ^ uri);
          let req = new Http_client.get uri in
          (req, f)

      | Delete (path, f) ->
          let uri = get_uri path in
          if !debug then !print_debug ("DELETE: " ^ uri);
          let req = new Http_client.delete uri in
          (req, f)

      | Post (path, json, f) ->
          let data =
            match json with
            | None      -> ""
            | Some json -> Json.to_string json
          in
          let uri = get_uri path in
          if !debug then !print_debug ("POST: " ^ uri ^ ": " ^ data);
          let req = new Http_client.post_raw uri data in
          (req, f)

      | Put (path, json, f) ->
          let data =
            match json with
            | None      -> ""
            | Some json -> Json.to_string json
          in
          let uri = get_uri path in
          if !debug then !print_debug ("PUT: " ^ uri ^ ": " ^ data);
          let req = new Http_client.put uri data in
          (req, f)
    in
    set_req_header req;
    let pipeline = new Http_client.pipeline in
    pipeline#add req;
    pipeline#run ();
    let v = req#response_body#value in
    let l = try Some (req#response_header#field "Location") with Not_found -> None in
    if !debug then (
      !print_debug ("RESPONSE STATUS: "
                    ^ Nethttp.string_of_http_status req#response_status);
      !print_debug ("RESPONSE BODY: " ^ v);
    );
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

  let root =
    _get "" (fun _ json -> Json.(some_of json >>= assoc))

  let version =
    _get "" (fun _ json ->
        Json.(some_of json >>= assoc >>= field "neo4j_version" >>= string)
      )

  module Property =
  struct
    type k = string

    type v = [
      | `Bool of bool
      | `Float of float
      | `Int of int
      | `Intlit of string
      | `String of string
      | `List_bool of bool list
      | `List_float of float list
      | `List_int of int list
      | `List_intlit of string list
      | `List_string of string list
    ]

    type kv = k * v

    type t = kv list

    let rec int_list revl : Json.json list -> int list result = function
      | []          -> OK (List.rev revl)
      | `Int i :: t -> int_list (i :: revl) t
      | j :: _      -> Error (Invalid_json ("mixed-type list is not allowed", j))

    let rec intlit_list revl = function
      | []             -> OK (List.rev revl)
      | `Intlit i :: t -> intlit_list (i :: revl) t
      | j :: _         -> Error (Invalid_json ("mixed-type list is not allowed", j))

    let rec bool_list revl = function
      | []           -> OK (List.rev revl)
      | `Bool b :: t -> bool_list (b :: revl) t
      | j :: _       -> Error (Invalid_json ("mixed-type list is not allowed", j))

    let rec float_list revl = function
      | []            -> OK (List.rev revl)
      | `Float f :: t -> float_list (f :: revl) t
      | j :: _        -> Error (Invalid_json ("mixed-type list is not allowed", j))

    let rec string_list revl = function
      | []             -> OK (List.rev revl)
      | `String s :: t -> string_list (s :: revl) t
      | j :: _         -> Error (Invalid_json ("mixed-type list is not allowed", j))

    let rec v_from_json (j : Json.json) : v result =
      match j with
      | `Bool b    -> OK (`Bool b)
      | `Float f   -> OK (`Float f)
      | `Int i     -> OK (`Int i)
      | `Intlit i  -> OK (`Intlit i)
      | `String s  -> OK (`String s)
      | `List l    -> (
          match l with
          | []             -> OK (`List_string [])
          | `Int _ :: t    -> int_list [] l >>= (fun l -> OK (`List_int l))
          | `Intlit _ :: t -> intlit_list [] l >>= (fun l -> OK (`List_intlit l))
          | `Bool _ :: t   -> bool_list [] l >>= (fun l -> OK (`List_bool l))
          | `Float _ :: t  -> float_list [] l >>= (fun l -> OK (`List_float l))
          | `String _ :: t -> string_list [] l >>= (fun l -> OK (`List_string l))
          | j :: _         ->
              Error (Invalid_json ("only basic type is allowed for list elements", j))
        )
      | `Assoc _   ->
          Error (Invalid_json ("object is not allowed", j))
      | `Null      ->
          Error (Invalid_json ("null is not allowed", j))
      | `Tuple _   ->
          Error (Invalid_json ("tuple is not allowed", j))
      | `Variant _ ->
          Error (Invalid_json ("variant is not allowed", j))

    let v_to_json (v : v) : Json.json =
      match v with
      | `Bool _ | `Float _ | `Int _ | `Intlit _ | `String _ as v ->
          v

      | `List_int is    -> `List (List.map (fun i -> `Int i) is)
      | `List_intlit is -> `List (List.map (fun i -> `Intlit i) is)
      | `List_float fs  -> `List (List.map (fun f -> `Float f) fs)
      | `List_bool bs   -> `List (List.map (fun b -> `Bool b) bs)
      | `List_string ss -> `List (List.map (fun s -> `String s) ss)

    let from_json json = Json.(assoc json >>= vmap v_from_json)

    let to_json (t : t) : Json.json =
      `Assoc (List.map (fun (k, (v : v)) -> (k, (v_to_json v))) t)

    let keys () =
      _get "propertykeys"
        (fun _ json -> Json.(some_of json >>= list >>= lmap string))
  end

  module Node =
  struct
    type id = int

    type t = {
      id : id;
      labels : string list;
      property : Property.t
    }

    let from_json json =
      let open Json in
      assoc json
      >>= (fun top -> field "metadata" top >>= assoc
      >>= (fun metadata -> field "id" metadata >>= int
      >>= (fun id -> field ~def:nil "labels" metadata >>= list >>= lmap string
      >>= (fun labels -> field ~def:empty "data" top >>= Property.from_json
      >>= (fun property ->
            OK { id; labels; property }
      )))))

    let from_rsp _ json =
      Json.some_of json >>= from_json

    let create ?(property = []) () =
      let data =
        match property with
        | [] -> None
        | ps -> Some (Property.to_json ps)
      in
      _post "node" ?data from_rsp

    let get id =
      _get (sprintf "node/%d/" id) from_rsp

    let delete id =
      _delete (sprintf "node/%d/" id) from_rsp

    let set_property id k v =
      _put (sprintf "node/%d/properties/%s" id k)
        ~data:(Property.v_to_json v)
        (fun _ _ -> OK ())

    let update_properties id t =
      _put (sprintf "node/%d/properties" id)
        ~data:(Property.to_json t)
        (fun _ _ -> OK ())

    let get_properties id =
      _get (sprintf "node/%d/properties" id)
        (fun _ json -> Json.some_of json >>= Property.from_json)

    let get_property id k =
      _get (sprintf "node/%d/properties/%s" id k)
        (fun _ json -> Json.some_of json >>= Property.v_from_json)

    let delete_properties id =
      _delete (sprintf "node/%d/properties" id)
        (fun _ _ -> OK ())

    let delete_property id k =
      _delete (sprintf "node/%d/properties/%s" id k)
        (fun _ _ -> OK ())

    let degree ?(types = []) id t =
      let path =
        sprintf "node/%d/degree/%s%s" id
          (match t with
           | `In_out -> "all"
           | `In     -> "in"
           | `Out    -> "out"
          )
          (match types with
           | [] -> ""
           | l  -> "/" ^ String.concat "&" types
          )
      in
      _get path (fun _ json -> Json.(some_of json >>= int))
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

    let get_nodes ?property label =
      let param =
        match property with
        | None        -> ""
        | Some (k, v) ->
            "?" ^ Netencoding.Url.(
                encode k ^ "=" ^ encode (Json.to_string (Property.v_to_json v))
              )
      in
      _get (sprintf "label/%s/nodes%s" label param)
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

    let create ?property ~from _to typ =
      let data =
        `Assoc ([
            ("to", `String (get_uri (sprintf "node/%d" _to)));
            ("type", `String typ);
          ] @ (
              match property with
              | None    -> []
              | Some ps -> [("data", Property.to_json ps)]
            ))
      in
      _post (sprintf "node/%d/relationships" from) ~data from_rsp

    let delete id =
      _delete (sprintf "relationship/%d" id) (fun _ _ -> OK ())

    let properties id =
      _get (sprintf "relationship/%d/properties" id)
        Json.(fun _ json -> some_of json >>= Property.from_json)

    let set_properties id properties =
      _put (sprintf "relationship/%d/properties" id)
        ~data:(Property.to_json properties)
        (fun _ _ -> OK ())

    let delete_properties id =
      _delete (sprintf "relationship/%d/properties" id)
        (fun _ _ -> OK ())

    let delete_property id k =
      _delete (sprintf "relationship/%d/properties/%s" id k)
        (fun _ _ -> OK ())

    let property id p =
      _get (sprintf "relationship/%d/properties/%s" id p)
        Json.(fun _ json -> some_of json >>= Property.v_from_json)

    let set_property id k v =
      _put (sprintf "relationship/%d/properties/%s" id k)
        ~data:(Property.v_to_json v)
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

    let types () =
      _get "relationship/types"
        Json.(fun _ json -> some_of json >>= list >>= lmap string)
  end

  module Cypher =
  struct
    type stmt = [`String of string | `Ast of Neo4j_cypher.statement]
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
      let stmt =
        match stmt with
        | `String s -> s
        | `Ast s    ->
            Neo4j_cypher.pp_print_statement Format.str_formatter s;
            Format.flush_str_formatter ()
      in
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
      map_rsp (fun (_, t) -> OK t) (execute_in_transaction [] t)

    let commit qs t =
      let data = queries_to_json qs in
      _post (sprintf "transaction/%s/commit" t.tid) ~data rsp_from_json

    let rollback t =
      _delete (sprintf "transaction/%s" t.tid) rsp_from_json

    let delete_all_relationships =
      (`String "START r=rel(*)  DELETE r;", [], [])

    let delete_all_nodes =
      (`String "START n=node(*) DELETE n;", [], [])
  end
end

(*
  Local variables:
  compile-command: "make -C .."
  End:
 *)
