module YoUtil :
sig
  val drop_assoc : Yojson.Safe.json -> (string * Yojson.Safe.json) list
  val drop_string : Yojson.Safe.json -> string
  val unwrap_res : Yojson.Safe.json -> Yojson.Safe.json
end

module type CONFIG =
sig
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
    ?params:(string * Yojson.Safe.json) list -> string -> string
  val wrap_cypher :
    ?verbose:bool ->
    string ->
    params:(string * Yojson.Safe.json) list -> f:(Yojson.Safe.json -> 'a) -> 'a
  val remove_all : unit -> (unit, 'a) Helpers.Result.t
  val insert_node_between : int -> string
  class node_of_json :
    (string * Yojson.Safe.json) list ->
    object
      method data : Yojson.Safe.json
      method id : int
      method json : (string * Yojson.Safe.json) list
      method prop : string -> Yojson.Safe.json
    end
  class date_of_json :
    (string * Yojson.Safe.json) list ->
    object
      method data : Yojson.Safe.json
      method id : int
      method json : (string * Yojson.Safe.json) list
      method when_ : string
    end
  class question_of_json :
    (string * Yojson.Safe.json) list ->
    object
      method data : Yojson.Safe.json
      method id : int
      method json : (string * Yojson.Safe.json) list
      method prop : string -> string
      method text : string
    end
  val get_questions :
    int -> (question_of_json list, string) Helpers.Result.t
  val get_next_timeline_node :
    int -> (node_of_json, string) Helpers.Result.t
  val id_from_node_json : (string * [> `String of string ]) list -> int64
end

module Make : functor (Cfg : CONFIG) -> API
