open Result

module List        = Helpers.List
module String      = Helpers.String
module Http_client = Nethttp_client
module Json        = Yojson.Safe

let sprintf = Printf.sprintf

module YoUtil = struct
  let drop_assoc = function `Assoc xs -> xs | _ -> failwith "Bad argument"
  let drop_string = function `String s -> s | _ -> failwith "Bad argument"
  let unwrap_res x = x |> drop_assoc |> List.assoc "data"
end

module type CONFIG = sig
  val server : string
  val port : int
  val auth : (string * string) option
end

module type API =
sig
  type path = string

  val get : path -> (Yojson.Safe.json, string) Result.t
  val post : path -> string -> (Http_client.http_call -> unit)
    -> (Yojson.Safe.json, string) Result.t

  val make_empty_node : unit -> (int, string) Result.t

  val get_node : int -> (Yojson.Safe.json, string) Result.t

  val node_properties : int -> (Yojson.Safe.json, string) Result.t

  val labels : unit -> (Yojson.Safe.json, string) Result.t

  val add_label : int -> string -> (unit, string) Result.t

  val cypher :
    ?params:(string * Yojson.Safe.json) list -> string
    -> (Yojson.Safe.json, string) Result.t

  val wrap_cypher :
    ?verbose:bool -> string
    -> params:(string * Yojson.Safe.json) list -> f:(Yojson.Safe.json -> 'a)
    -> ('a, string) Result.t

  val remove_all : unit -> (unit, 'a) Result.t
end

module Make(Cfg: CONFIG) : API = struct

type path = string

let auth =
  match Cfg.auth with
  | None              -> None
  | Some (user, pass) -> Some (B64.encode (user ^ ":" ^ pass))

let get_uri path = sprintf "http://%s:%d/%s" Cfg.server Cfg.port path

let set_req_header req =
  req#set_req_header "Accept"       "application/json; charset=UTF8";
  req#set_req_header "Content-type" "application/json";
  match auth with
  | None         -> ()
  | Some payload -> req#set_req_header "Authorization" ("Basic " ^ payload)

let get path =
  let pipeline = new Http_client.pipeline in
  let req = new Http_client.get (get_uri path) in
  set_req_header req;
  pipeline#add req;
  pipeline#run ();
  match req#response_status with
  | `Ok -> OK (Json.from_string req#response_body#value)
  | s   -> Error (Nethttp.string_of_http_status s)

let post path post callback =
  let pipeline = new Http_client.pipeline in
  let req = new Http_client.post_raw (get_uri path) post in
  set_req_header req;
  pipeline#add_with_callback req callback;
  pipeline#run ();
  match req#response_status with
  | `Ok -> OK (Json.from_string req#response_body#value)
  | s   -> Error (Nethttp.string_of_http_status s)

let make_empty_node () : (_,_) Result.t =
  get "db/data/node/"
  >>= (function
      | `Assoc xs -> OK (List.Assoc.find_exn xs "self")
      | _ -> Error "field not found: self"
  ) >>= (fun url ->
      OK (int_of_string @@ String.rsplit (Json.to_string url) ~by:'/')
  )

let get_node nodeid =
  get (sprintf "db/data/node/%d/" nodeid)

let node_properties nodeid =
  get (sprintf "db/data/node/%d/properties" nodeid)

let labels () =
  get "db/data/labels/"

let add_label id label =
  post (sprintf "db/data/node/%d/labels" id) (sprintf "\"%s\"" label)
    (fun _ -> ()) >>= (fun _ -> OK ())

let cypher ?(params=[]) query =
  let data =
    Json.to_string (`Assoc [
        ("query",  `String query);
        ("params", `Assoc params)
      ])
  in
  post "db/data/cypher" data (fun call ->
      match call#response_status with
      | `Ok -> ()
      | `Bad_request ->
          let j = Json.from_string call#response_body#value in
          j |> YoUtil.drop_assoc |> List.assoc "message"
          |> YoUtil.drop_string |> print_endline;
      | _ ->
          print_endline call#response_status_text;
          print_endline call#response_body#value;
          print_endline "callback"
    )

let wrap_cypher ?(verbose=true) cmd ~params ~f =
  cypher ~params cmd >>=
    (fun json -> json |> YoUtil.drop_assoc |> List.assoc "data" |> f |> ok)

let remove_all () : (_,_) Result.t =
  let _ =
    wrap_cypher ~verbose:false ~params:[] ~f:(fun _ -> ())
      "START r=rel(*)  DELETE r;"
  in
  let _ =
    wrap_cypher ~verbose:false ~params:[] ~f:(fun _ -> ())
      "START n=node(*) DELETE n;"
  in
  OK ()
end

(*
  Local variables:
  compile-command: "make -C .."
  End:
 *)
