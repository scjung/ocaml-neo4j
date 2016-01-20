module type CONFIG =
sig
  val server : string
  val port : int
  val auth : (string * string) option
end

module type API =
sig
  type path = string

  val get : path -> (Yojson.Safe.json, string) Result.t
  val post : path -> string -> (Nethttp_client.http_call -> unit)
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

module Make : functor (Cfg : CONFIG) -> API
