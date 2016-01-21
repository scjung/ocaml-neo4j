module type CONFIG =
sig
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

  (** {3 Front-end APIs} *)

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

  (** {3 APIs under the hood} *)

  val _get : path -> (Yojson.Safe.json option -> 'a result) -> 'a req

  val _delete : path -> (Yojson.Safe.json option -> 'a result) -> 'a req

  val _post :
    path -> ?data:Yojson.Safe.json -> (Yojson.Safe.json option -> 'a result) -> 'a req

  val _put :
    path -> ?data:Yojson.Safe.json -> (Yojson.Safe.json option -> 'a result) -> 'a req
end

module Make : functor (Cfg : CONFIG) -> API
