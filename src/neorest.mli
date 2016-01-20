module type CONFIG =
sig
  val server : string
  val port : int
  val auth : (string * string) option
end

module type API =
sig
  type path = string

  val get : path -> (string, string) Result.t
  val post : path -> string -> (Nethttp_client.http_call -> unit) -> (string, string) Result.t

  val make_empty_node : unit -> (int, string) Result.t
  val get_node : int -> (string, string) Result.t
  val node_properties : int -> (string, string) Result.t
  val labels : unit -> (string, string) Result.t
  val add_label : int -> string -> (unit, string) Result.t

  val cypher :
    ?params:(string * Yojson.Safe.json) list -> string -> (string, string) Result.t

  val wrap_cypher :
    ?verbose:bool -> string
    -> params:(string * Yojson.Safe.json) list -> f:(Yojson.Safe.json -> 'a)
    -> ('a, string) Result.t

  val remove_all : unit -> (unit, 'a) Result.t
end

module Make : functor (Cfg : CONFIG) -> API
