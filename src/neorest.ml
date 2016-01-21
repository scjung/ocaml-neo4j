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

  type 'a req

  type error =
    | Error_rsp of Nethttp.http_status * Yojson.Safe.json option
    | Invalid_json of string
    | Malformed_json of string
    | Unexpected of string

  type 'a result = ('a, error) Result.t

  val and_then : ('a -> 'b result) -> 'a req -> 'b req

  val batch : (int * 'a req) list -> (int * 'a) list req

  val execute : 'a req -> ('a, error) Result.t

  val version : string req

  module Node :
  sig
    type id = int

    type t = {
      id : id;
      labels : string list;
      properties : (string * string) list;
    }

    val create : ?properties:(string * string) list -> unit -> t req

    val get : int -> t req

    val delete : int -> t req
  end

  module Label :
  sig
    type t = string

    val add : int -> t list -> unit req

    val replace : int -> t list -> unit req

    val remove : int -> t -> unit req

    val get : int -> t list req

    val get_nodes : ?properties:(string * string) list -> t -> Node.t list req

    val list : t list req
  end

  module Relationship :
  sig
    type id = int
    type typ = string

    type t = {
      id : id;
      typ : typ
    }

    val get : id -> t req

    val create : ?properties:(string * string) list -> from:Node.id -> Node.id -> typ
      -> t req

    val delete : id -> unit req

    val properties : id -> (string * string) list req

    val set_properties : id -> (string * string) list -> unit req

    val property : id -> string -> string req

    val set_property : id -> string -> string -> unit req

    val of_node : ?types:string list -> Node.id -> [`In_out | `In | `Out] -> t list req
  end

  (* val cypher : *)
  (*   ?params:(string * Yojson.Safe.json) list -> string *)
  (*   -> (Yojson.Safe.json, string) Result.t *)

  (* val wrap_cypher : *)
  (*   ?verbose:bool -> string *)
  (*   -> params:(string * Yojson.Safe.json) list -> f:(Yojson.Safe.json -> 'a) *)
  (*   -> ('a, string) Result.t *)

  (* val remove_all : unit -> (unit, 'a) Result.t *)

  val _get : path -> (Yojson.Safe.json option -> 'a result) -> 'a req

  val _delete : path -> (Yojson.Safe.json option -> 'a result) -> 'a req

  val _post :
    path -> ?data:Yojson.Safe.json -> (Yojson.Safe.json option -> 'a result) -> 'a req

  val _put :
    path -> ?data:Yojson.Safe.json -> (Yojson.Safe.json option -> 'a result) -> 'a req
end

module Make(Cfg: CONFIG) : API =
struct
  (* Paths should be relative to data-API URI. For instance, a path string of
     "http://localhost:7474/db/data/node" is "node". *)
  type path = string

  type error =
    | Error_rsp of Nethttp.http_status * Yojson.Safe.json option
    | Invalid_json of string
    | Malformed_json of string
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
      | Yojson.Json_error m -> Error (Malformed_json m)

    let some_of = function
      | Some x -> OK x
      | None -> Error (Invalid_json "expects something")

    let int = function
      | `Int i -> OK i
      | _ -> Error (Invalid_json "expects int")

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

    let nil = `List []

    let empty = `Assoc []

    let lmap : ('a -> 'b result) -> 'a list -> 'b list result =
      let rec aux revl f = function
        | []     -> OK (List.rev revl)
        | h :: t -> f h >>= (fun x -> aux (x :: revl) f t)
      in
      fun f l -> aux [] f l

    let vmap f l = lmap (fun (k, v) -> f v >>= (fun v -> OK (k, v))) l
  end

  type 'a req =
    | Get of path * (Json.json option -> ('a, error) Result.t)
    | Delete of path * (Json.json option -> ('a, error) Result.t)
    | Post of path * Json.json option * (Json.json option -> ('a, error) Result.t)
    | Put of path * Json.json option * (Json.json option -> ('a, error) Result.t)

  let and_then m = function
    | Get (p, f)     -> Get (p, (fun j -> f j >>= m))
    | Delete (p, f)  -> Delete (p, (fun j -> f j >>= m))
    | Post (p, d, f) -> Post (p, d, (fun j -> f j >>= m))
    | Put (p, d, f)  -> Put (p, d, (fun j -> f j >>= m))

  let _get path f = Get (path, f)
  let _delete path f = Delete (path, f)
  let _post path ?data f = Post (path, data, f)
  let _put path ?data f = Put (path, data, f)

  let batch l =
    let (rev_jsons, callbacks) =
      List.fold_left (fun (rev_jsons, callbacks) (i, req) ->
          let (mthd, path, body_opt, callback) =
            match req with
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
    let callback json =
      Json.(some_of json >>= list >>= lmap (fun rsp ->
          assoc rsp
          >>= (fun rsp -> field "id" rsp >>= int
          >>= (fun id -> field "body" rsp
          >>= (fun body ->
                (List.assoc id callbacks) (Some body) >>= (fun x -> OK (id, x))
        )))))
    in
    Post ("batch", Some (`List (List.rev rev_jsons)), callback)

  let auth =
    match Cfg.auth with
    | None              -> None
    | Some (user, pass) -> Some (B64.encode (user ^ ":" ^ pass))

  let get_uri path = sprintf "http://%s:%d/db/data/%s" Cfg.server Cfg.port path

  let set_req_header req =
    req#set_req_header "Accept"       "application/json; charset=UTF8";
    req#set_req_header "Content-type" "application/json";
    match auth with
    | None         -> ()
    | Some payload -> req#set_req_header "Authorization" ("Basic " ^ payload)

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
    match req#response_status with
    | `Ok | `Created | `No_content ->
        if String.length v = 0 then
          f None
        else
          Json.from_string v >>= (fun json -> f (Some json))

    | s ->
        if String.length v = 0 then
          Error (Error_rsp (s, None))
        else
          Json.from_string v >>= (fun json -> Error (Error_rsp (s, Some json)))

  let version =
    _get "" (fun json ->
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

    let from_json_opt json =
      Json.some_of json >>= from_json

    let create ?(properties = []) () =
      let data =
        match properties with
        | [] -> None
        | ps -> Some (`Assoc (List.map (fun (k, v) -> (k, `String v)) ps))
      in
      _post "node" ?data from_json_opt

    let get id =
      _get (sprintf "node/%d/" id) from_json_opt

    let delete id =
      _delete (sprintf "node/%d/" id) from_json_opt
  end

  module Label =
  struct
    type t = string

    let labels_from_json =
      Json.(fun json -> some_of json >>= list >>= lmap string)

    let labels_to_json labels =
        `List (
          List.map (fun l ->
              if String.length l = 0 then invalid_arg "empty label" else `String l
            ) labels
        )

    let add id labels =
      _post (sprintf "node/%d/labels" id) ~data:(labels_to_json labels)
        (fun _ -> OK ())

    let replace id labels =
      _put (sprintf "node/%d/labels" id) ~data:(labels_to_json labels)
        (fun _ -> OK ())

    let remove id label =
      _delete (sprintf "node/%d/labels/%s" id label)
        (fun _ -> OK ())

    let get id =
      _get (sprintf "node/%d/labels" id) labels_from_json

    let get_nodes ?properties label = (* TODO: with properties *)
      _get (sprintf "label/%s/nodes" label)
        Json.(fun json -> some_of json >>= list >>= lmap Node.from_json)

    let list =
      _get (sprintf "labels") labels_from_json
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

    let from_json_opt json =
      Json.some_of json >>= from_json

    let get id =
      _get (sprintf "relationship/%d" id) from_json_opt

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
      _post (sprintf "node/%d/relationships" from) ~data from_json_opt

    let delete id =
      _delete (sprintf "relationship/%d" id) (fun _ -> OK ())

    let properties id =
      _get (sprintf "relationship/%d/properties" id)
        Json.(fun json -> some_of json >>= assoc >>= vmap string)

    let set_properties id properties =
      _put (sprintf "relationship/%d/properties" id)
        ~data:(`Assoc (List.map (fun (k, v) -> (k, `String v)) properties))
        (fun _ -> OK ())

    let property id p =
      _get (sprintf "relationship/%d/properties/%s" id p)
        Json.(fun json -> some_of json >>= string)

    let set_property id k v =
      _put (sprintf "relationship/%d/properties/%s" id k)
        ~data:(`String v)
        Json.(fun _ -> OK ())

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
        Json.(fun json -> some_of json >>= list >>= lmap from_json)
  end

(* let cypher ?(params=[]) query = *)
(*   let data = *)
(*     Json.to_string (`Assoc [ *)
(*         ("query",  `String query); *)
(*         ("params", `Assoc params) *)
(*       ]) *)
(*   in *)
(*   post "db/data/cypher" data (fun call -> *)
(*       match call#response_status with *)
(*       | `Ok -> () *)
(*       | `Bad_request -> *)
(*           let j = Json.from_string call#response_body#value in *)
(*           j |> YoUtil.drop_assoc |> List.assoc "message" *)
(*           |> YoUtil.drop_string |> print_endline; *)
(*       | _ -> *)
(*           print_endline call#response_status_text; *)
(*           print_endline call#response_body#value; *)
(*           print_endline "callback" *)
(*     ) *)

(* let wrap_cypher ?(verbose=true) cmd ~params ~f = *)
(*   cypher ~params cmd >>= *)
(*     (fun json -> json |> YoUtil.drop_assoc |> List.assoc "data" |> f |> ok) *)

(* let remove_all () : (_,_) Result.t = *)
(*   let _ = *)
(*     wrap_cypher ~verbose:false ~params:[] ~f:(fun _ -> ()) *)
(*       "START r=rel(\*\)  DELETE r;" *)
(*   in *)
(*   let _ = *)
(*     wrap_cypher ~verbose:false ~params:[] ~f:(fun _ -> ()) *)
(*       "START n=node(\*\) DELETE n;" *)
(*   in *)
(*   OK () *)
end

(*
  Local variables:
  compile-command: "make -C .."
  End:
 *)
