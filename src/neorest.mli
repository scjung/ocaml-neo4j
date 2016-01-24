module type CONFIG =
sig
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

  (** {3 Front-end APIs} *)

  module Property :
  sig
    type k = string

    type basic_v = [
      | `Bool of bool
      | `Float of float
      | `Int of int
      | `Intlit of string
      | `String of string
    ]

    type v = [ basic_v | `List of basic_v list ]

    type kv = k * v

    type t = kv list

    val v_from_json : Yojson.Safe.json -> v result

    val from_json : Yojson.Safe.json -> t result

    val keys : unit -> k list call
  end

  module Node :
  sig
    type id = int

    type t = {
      id : id;
      labels : string list;
      property : Property.t;
    }

    val create : ?property:Property.t -> unit -> t call

    val get : int -> t call

    val delete : int -> t call

    val set_property : int -> Property.k -> Property.v -> unit call

    val update_properties : int -> Property.t -> unit call

    val get_properties : int -> Property.t call

    val get_property : int -> Property.k -> Property.v call

    val delete_properties : int -> unit call

    val delete_property : int -> Property.k -> unit call

    val degree : ?types:string list -> id -> [`In_out | `In | `Out] -> int call
  end

  module Label :
  sig
    type t = string

    val add : int -> t list -> unit call

    val replace : int -> t list -> unit call

    val remove : int -> t -> unit call

    val get : int -> t list call

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

    val create : ?property:Property.t -> from:Node.id -> Node.id -> typ -> t call

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
    type stmt = string
    type param = (string * Yojson.Safe.json) list

    (** Statement, its parameters, and includeStats. *)
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

    (** {3 Predefined queries} *)

    val delete_all_relationships : query

    val delete_all_nodes : query
  end

  (** {3 APIs under the hood} *)

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

  (** Debug flag. It is setted to [false] by default. If [true], {!print_debug}
      will print a lot of debug messages. *)
  val debug : bool ref

  (** Function to print debug messages. It is setted to [Pervasives.prerr_endline]
      by default. *)
  val print_debug : (string -> unit) ref
end

module Make : functor (Cfg : CONFIG) -> API


(*
  Local variables:
  compile-command: "make -C .."
  End:
 *)
