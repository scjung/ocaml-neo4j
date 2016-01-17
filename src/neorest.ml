open Printf
open Helpers
module Http_client = Nethttp_client
open Result
module Yojson = Yojson.Safe

let http_get  = Http_client.Convenience.http_get
let http_post = Http_client.Convenience.http_post
let to_json = Yojson.from_string
let print_json = Yojson.pretty_to_channel stdout

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
  val make_empty_node : unit -> (int, unit) Helpers.Result.t
  val get_node : int -> string
  val node_properties : int -> string
  val labels : unit -> string
  val add_label : int -> string -> unit
  type cypher_msg = string
  val string_of_cypher_msg : cypher_msg -> cypher_msg
  val post_cypher :
    ?params:(string * Yojson.json) list -> string -> string
  val wrap_cypher :
    ?verbose:bool ->
    string ->
    params:(string * Yojson.json) list -> f:(Yojson.json -> 'a) -> 'a
  val remove_all : unit -> (unit, 'a) Helpers.Result.t
  val insert_node_between : int -> string
  class node_of_json :
    (string * Yojson.json) list ->
    object
      method data : Yojson.json
      method id : int
      method json : (string * Yojson.json) list
      method prop : string -> Yojson.json
    end
  class date_of_json :
    (string * Yojson.json) list ->
    object
      method data : Yojson.json
      method id : int
      method json : (string * Yojson.json) list
      method when_ : string
    end
  class question_of_json :
    (string * Yojson.json) list ->
    object
      method data : Yojson.json
      method id : int
      method json : (string * Yojson.json) list
      method prop : string -> string
      method text : string
    end
  val get_questions :
    int -> (question_of_json list, string) Helpers.Result.t
  val get_next_timeline_node :
    int -> (node_of_json, string) Helpers.Result.t
  val id_from_node_json : (string * [> `String of string ]) list -> int64
end

module Make(Cfg: CONFIG) : API = struct

let auth =
  match Cfg.auth with
  | None              -> None
  | Some (user, pass) -> Some (B64.encode (user ^ ":" ^ pass))

let req url post callback =
  let url = sprintf "http://%s:%d/%s" Cfg.server Cfg.port url in
  let pipeline = new Http_client.pipeline in
  let req = new Http_client.post_raw url post in
  let () =
    req#set_req_header "Accept"       "application/json; charset=UTF8";
    req#set_req_header "Content-type" "application/json";
    match auth with
    | None         -> ()
    | Some payload -> req#set_req_header "Authorization" ("Basic " ^ payload)
  in
  pipeline#add_with_callback req callback;
  pipeline#run ();
  req#get_resp_body ()

let make_empty_node () : (_,_) Result.t =
  let rsp = req "db/data/node/" "" (fun _ -> ()) in
  (match to_json rsp with
   | `Assoc xs -> OK (List.Assoc.find_exn xs "self")
   | _ -> Error ()
  ) >>= fun url ->
  OK (int_of_string @@ String.rsplit (Yojson.to_string url) ~by:'/')

let get_node nodeid =
  req (sprintf "db/data/node/%d/" nodeid) "" (fun _ -> ())

let node_properties nodeid =
  req (sprintf "db/data/node/%d/properties" nodeid) "" (fun _ -> ())

let labels () =
  req "db/data/labels/" "" (fun _ -> ())

let add_label id label =
  let _ =
    req (sprintf "db/data/node/%d/labels" id) (sprintf "\"%s\"" label)
      (fun _ -> ())
  in
  ()

type cypher_msg = string

let string_of_cypher_msg (x:cypher_msg) = x

let post_cypher ?(params=[]) cypher =
  let post =
    Yojson.to_string (`Assoc [
        ("query",  `String cypher);
        ("params", `Assoc params)
      ])
  in
  req "db/data/cypher" post (fun call ->
      match call#response_status with
      | `Ok -> ()
      | `Bad_request ->
          let j = to_json call#response_body#value in
          j |> YoUtil.drop_assoc |> List.assoc "message"
          |> YoUtil.drop_string |> print_endline;
      | _ ->
          print_endline call#response_status_text;
          print_endline call#response_body#value;
          print_endline "callback"
    )

let wrap_cypher ?(verbose=true) cmd ~params ~f =
  let (ans: string) = post_cypher ~params cmd in
  if verbose then print_endline ans;
  ans |> to_json |> YoUtil.drop_assoc |> List.assoc "data" |> f

let remove_all () : (_,_) Result.t =
  wrap_cypher ~verbose:false ~params:[] ~f:(fun _ -> () )
     "START r=rel(*)  DELETE r;";
  wrap_cypher ~verbose:false ~params:[] ~f:(fun _ -> () )
     "START n=node(*) DELETE n;";
  OK ()

let insert_node_between id1  =
  let cmd = sprintf
	"START n=node(%d)
         MATCH n-[r:FOLLOWED_BY]->m
         DELETE r
	 CREATE UNIQUE n-[r1:FOLLOWED_BY]->(k{title:'qwe'})-[r2:FOLLOWED_BY]->m
	 set k: TIMELINE_ITEM
	" id1
  in
  print_endline (Str.global_replace (Str.regexp "\n") cmd " ");
  post_cypher cmd


class node_of_json (j: (string * Yojson.json) list) = object
  method id : int =
    match List.assoc "self" j with
    | `String s -> int_of_string @@ String.rsplit s ~by:'/'
    | _ -> failwith "Wrong json"
  method json = j
  method data = List.assoc "data" j
  method prop name = List.assoc name j
end

class date_of_json (j: (string * Yojson.json) list) = object
  method id : int =
    match List.assoc "self" j with
    | `String s -> int_of_string @@ String.rsplit s ~by:'/'
    | _ -> failwith "Wrong json"
  method json = j
  method data = List.assoc "data" j
  method when_ = match List.assoc "data" j with
    | `Assoc xs -> List.assoc "when" xs |> (function `String s -> s | _ -> assert false)
    | _ -> assert false
end

class question_of_json (j: (string * Yojson.json) list) = object(self)
  method id : int =
    match List.assoc "self" j with
    | `String s -> int_of_string @@ String.rsplit s ~by:'/'
    | _ -> failwith "Wrong json"
  method json = j
  method data = List.assoc "data" j
  method prop name = match List.assoc "data" j with
    | `Assoc xs -> List.assoc name xs |> (function `String s -> s | _ -> assert false)
    | _ -> assert false
  method text = self#prop "text"
end
(*
let get_start_day () =
  ukr_start_node () >>= fun start_node_id ->
  let cmd = sprintf
              "START x=node(%d) MATCH d<-[:WHEN]-x RETURN d;"
              start_node_id
  in
  let ans = post_cypher (*~params:["first_id", `Int start_node_id]*) cmd in
  (*print_endline ans;*)
  match to_json ans with
  | `Assoc [_; ("data",`List [`List [`Assoc xs]])] -> OK (new date_of_json xs)
  |  _ -> Error "JSON match failure"
 *)
let get_questions nodeid =
  let cmd = sprintf "START x=node(%d)
                     MATCH x-[:HAS_QUESTION]->y RETURN y" nodeid in
  let j = to_json @@ post_cypher cmd |> YoUtil.drop_assoc |> List.assoc "data" in
  print_endline @@ Yojson.to_string j;
  match j with
  | `List[`List ys ] -> OK (List.map (fun y -> new question_of_json (YoUtil.drop_assoc y)) ys)
  | _ -> Error "JSON match failure"

let get_next_timeline_node nodeid =
  let cmd = sprintf "START x=node(%d)
                     MATCH x-[:FOLLOWED_BY]->y RETURN y" nodeid in
  match to_json @@ post_cypher cmd |> YoUtil.drop_assoc |> List.assoc "data" with
  | `List[`List[`Assoc xs]] -> OK (new node_of_json xs)
  | _ -> Error "JSON match failure"

let id_from_node_json ej =
  match List.Assoc.find_exn ej "self" with
  | `String s -> Int64.of_string @@ String.rsplit s ~by:'/'
  | _ -> failwith "Wrong json for function id_from_node_json"

end

(*
  Local variables:
  compile-command: "make -C .."
  End:
 *)
