(** Lists containing at least one element. *)
type 'a ne_list = 'a * 'a list

type symbolic_name = string

type variable = symbolic_name

type property_key_name = symbolic_name

type legacy_operator = string

type function_name = symbolic_name

type procedure_name = symbolic_name

type rel_type_name = symbolic_name

type rel_type = rel_type_name

type label_name = symbolic_name

type node_label = label_name

type literal =
  | Int of int
  | Float of float
  | String of string

type range = int option * int option

type parameter = string

type node_labels = node_label ne_list

(** Unary operators *)
type uop =
  | Not     (** [NOT] *)
  | UPlus   (** [+] *)
  | UMinus  (** [-] *)

(** Binary operators *)
type bop =
  | Or   (** [OR] *)
  | Xor  (** [XOR] *)
  | And  (** [AND] *)
  | Eq   (** [=] *)
  | Ne   (** [<>] *)
  | BNe  (** [!=]. It is invalid not-equal. *)
  | Lt   (** [<] *)
  | Gt   (** [>] *)
  | Lte  (** [<=] *)
  | Gte  (** [>=] *)
  | Add  (** [+] *)
  | Sub  (** [-] *)
  | Mul  (** [*] *)
  | Div  (** [/] *)
  | Mod  (** [%] *)
  | Pow  (** [^] *)

type expression =
  | Literal of literal

  | Parameter of parameter

  | True | False | Null

  | Case of case

  | Count_star
      (** [COUNT( * )] *)

  | Map of map

  | List_comprehension of list_comprehension

  | Collection of expression list
      (** [\[e, ...\]] *)

  | Filter of filter
      (** [FILTER(e)] *)

  | Extract of filter * expression option
      (** [EXTRACT(e | e)] *)

  | Reduce of variable * expression * id_in_coll * expression
      (** [REDUCE(v = e, i | e)] *)

  | All_pred of filter
      (** [ALL(e)] *)

  | Any_pred of filter
      (** [ANY(e)] *)

  | None_pred of filter
      (** [NONE(e)] *)

  | Single_pred of filter
      (** [SINGLE(e)] *)

  | Shortest_path of shortest_path

  | Relationships_pattern of relationships_pattern

  | Fun_invocation of fun_invocation

  | Variable of variable

  | Uexpr of uop * expression

  | Bexpr of expression * bop * expression

  | Container_index of expression * expression
      (** [e\[e]] *)

  | Collection_slice of expression * expression option * expression option
      (** [e\[e .. e\]] *)

  | Regex_match of expression * expression
      (** [e =~ e] *)

  | In of expression * expression
      (** [e IN e] *)

  | Starts_with of expression * expression
      (** [e STARTS WITH e] *)

  | Ends_with of expression * expression
      (** [e ENDS WITH e] *)

  | Contains of expression * expression
      (** [e CONTAINS e] *)

  | Is_null of expression
      (** [e IS NULL] *)

  | Is_not_null of expression
      (** [e IS NOT NULL] *)

  | Property_lookup of property_lookup

  | Has_labels of expression * [`Property_lookup of property_lookup |
                                `Node_labels of node_labels] list

and property_lookup =
  | Legacy_property of expression * property_key_name * legacy_operator
  | Property of expression * property_key_name

(** [x IN e WHERE e] *)
and filter = id_in_coll * expression option

(** [x IN e] *)
and id_in_coll = variable * expression

(** [{ k : e, ... }] *)
and map = (property_key_name * expression) list

(** [\[ f | e \]] *)
and list_comprehension = filter * expression option

(** [CASE e ... ELSE e END] *)
and case = expression option * case_alternatives ne_list * expression option

(** [WHEN e THEN e] *)
and case_alternatives = expression * expression

(** The second boolean value denotes whether [DISTINCT] is given or not. *)
and fun_invocation = function_name * bool * expression list

(** [(x ls p)] *)
and node_pattern = variable option * node_labels option * properties option

and properties = [`Map of map | `Parameter of parameter]

and pattern_element = node_pattern * pattern_element_chain list

and pattern_element_chain = relationship_pattern * node_pattern

and relationships_pattern = node_pattern * pattern_element_chain ne_list

and relationship_pattern = [`Both | `Incoming | `Outgoing] * relationship_detail

(** [\[ x ? rt l p \]]. The second boolean value denotes whether [?] is given
    or not. *)
and relationship_detail =
  variable option * bool * relationship_types * variable_length option
  * properties option

(** [: n | ...] *)
and relationship_types = rel_type_name list

(** [* n .. n] *)
and variable_length = range option

(** If the first boolean value is [true], then it is [allShortestPath(p)].
    Otherwise it is [shortestPaths(p)]. *)
and shortest_path = bool * pattern_element

type anonymous_pattern_part =
  | Shortest_path_pattern of shortest_path
  | Pattern_element of pattern_element

type pattern_part = variable option * anonymous_pattern_part

type pattern = pattern_part ne_list

type literal_ids = int ne_list

(** [: n (n = ...)] *)
type identified_index_lookup =
  symbolic_name * symbolic_name * [`String of string | `Parameter of parameter]

(** [: n (...)] *)
type index_query =
  symbolic_name * [`String of string | `Parameter of parameter]

type node_lookup =
  | Node_by_ids of literal_ids
  | Node_by_parameter of parameter
  | Node_by_identified_index of identified_index_lookup
  | Node_by_index_query of index_query
  | All_nodes

(** [RELATIONSHIP ...] *)
type relationship_lookup =
  | Relationship_by_ids of literal_ids
  | Relationship_by_parameter of parameter
  | Relationship_by_identified_index of identified_index_lookup
  | Relationship_by_index_query of index_query
  | All_relationships

type lookup =
  | Node_lookup of node_lookup
  | Relationship_lookup of relationship_lookup

type start_point = variable * lookup

(** [WHERE e] *)
type where = expression

type hint =
  | Using_index of variable * node_label * property_key_name
      (** [USING INDEX] *)

  | Using_join of variable ne_list
      (** [USING JOIN ON] *)

  | Using_scan of variable * node_label
      (** [USING SCAN] *)

type property_expression = expression * property_lookup ne_list

type set_item =
  | Set_property_item of property_expression * expression
  | Set_exact_properties of variable * expression
  | Set_including_properties of variable * expression
  | Set_label_item of variable * node_labels

(** [SET] *)
type set_clause = set_item ne_list

type remove_item =
  | Remove_label_item of variable * node_labels
  | Remove_property_item of property_expression

type return_item = expression * variable option

type return_items =
  | Return_include_existing of return_item ne_list
      (** [* , ...] *)

  | Return of return_item list

type sort_item = expression * [`ASC | `DESC]

(** [ORDER BY] *)
type order = sort_item ne_list

(** [SKIP] *)
type skip = expression

(** [LIMIT] *)
type limit = expression

type return_body = return_items * order option * skip option * limit option

type merge_action =
  | On_match of set_clause
      (** [ON MATCH] *)

  | On_create of set_clause
      (** [ON CREATE] *)

(** [LOAD CSV]. The first boolean value denotes whether [WITH HEADER] is
    given or not. The last string literal is field terminator. *)
type load_csv = bool * expression * variable * string option

type clause =
  | LoadCSV of load_csv

  | Start of start_point ne_list * where option
      (** [START] *)

  | Match of bool * pattern * hint list * where option
      (** [MATCH]. The first boolean value denotes whether [OPTIONAL] is given
          or not. *)

  | Unwind of expression * variable
      (** [UNWIND] *)

  | Merge of pattern_part * merge_action list
      (** [MERGE] *)

  | Create of bool * pattern
      (** [CREATE]. The first boolean value denotes whether [UNIQUE] is given or not. *)

  | Set of set_item ne_list
      (** [SET] *)

  | Delete of bool * expression ne_list
      (** [DELETE]. The first boolean value denotes whether [DETACH] is given
          or not. *)

  | Remove of remove_item ne_list
      (** [REMOVE] *)

  | Foreach of variable * expression * clause ne_list
      (** [FOREACH] *)

  | With of bool * return_body * where option
      (** [WITH]. The first boolean value denotes whether [DISTINCT] is given or not. *)

  | Return of bool * return_body
      (** [RETURN] *)

type single_query = clause ne_list

type union =
  | UnionAll of single_query
  | Union of single_query

type regular_query = single_query * union list

type load_csv_query = load_csv * clause list

(** [USING PERIODIC COMMIT] *)
type periodic_commit_hint = int option

type bulk_import_query = periodic_commit_hint * load_csv_query

type query =
  | Regular of regular_query
  | Bulk_import of bulk_import_query

(** [CONSTRAINT ON] *)
type unique_constraint = variable * node_label * property_expression

type command =
  | Create_index of node_label * property_key_name
      (** [CREATE INDEX ON] *)

  | Drop_index of node_label * property_key_name
      (** [DROP INDEX ON] *)

  | Create_node_constraint of
      variable * node_label * property_expression * [`UNIQUE | `EXISTS]
      (** [CREATE CONSTRAINT ON ... ASSERT ...] *)

  | Create_relationship_constraint of
      [`None | `Left | `Right] * variable * rel_type * property_expression
      (** [CREATE CONSTRAINT ON ... ASSERT ...] *)

  | Drop_node_constraint of
      variable * node_label * property_expression * [`UNIQUE | `EXISTS]
      (** [DROP CONSTRAINT ON ... ASSERT ...] *)

  | Drop_relationship_constraint of
      [`None | `Left | `Right] * variable * rel_type * property_expression
      (** [DROP CONSTRAINT ON ... ASSERT ...] *)

  | Call of symbolic_name list * procedure_name * expression list

type statement =
  | Command of command
  | Query of query

(** {3 Pretty printer} *)

(** The functions below prints given AST in the format of Cypher query syntax. *)

val pp_print_symbolic_name : Format.formatter -> symbolic_name -> unit
val pp_print_variable : Format.formatter -> variable -> unit
val pp_print_property_key_name : Format.formatter -> property_key_name -> unit
val pp_print_legacy_operator : Format.formatter -> legacy_operator -> unit
val pp_print_function_name : Format.formatter -> function_name -> unit
val pp_print_procedure_name : Format.formatter -> procedure_name -> unit
val pp_print_rel_type_name : Format.formatter -> rel_type_name -> unit
val pp_print_label_name : Format.formatter -> label_name -> unit
val pp_print_node_label : Format.formatter -> node_label -> unit
val pp_print_literal : Format.formatter -> literal -> unit
val pp_print_range : Format.formatter -> range -> unit
val pp_print_parameter : Format.formatter -> parameter -> unit
val pp_print_node_labels : Format.formatter -> node_labels -> unit
val pp_print_uop : Format.formatter -> uop -> unit
val pp_print_bop : Format.formatter -> bop -> unit
val pp_print_expression : Format.formatter -> expression -> unit
val pp_print_property_lookup : Format.formatter -> property_lookup -> unit
val pp_print_filter : Format.formatter -> filter -> unit
val pp_print_id_in_coll : Format.formatter -> id_in_coll -> unit
val pp_print_map : Format.formatter -> map -> unit
val pp_print_list_comprehension : Format.formatter -> list_comprehension -> unit
val pp_print_case : Format.formatter -> case -> unit
val pp_print_case_alternatives : Format.formatter -> case_alternatives -> unit
val pp_print_fun_invocation : Format.formatter -> fun_invocation -> unit
val pp_print_node_pattern : Format.formatter -> node_pattern -> unit
val pp_print_properties : Format.formatter -> properties -> unit
val pp_print_pattern_element : Format.formatter -> pattern_element -> unit
val pp_print_pattern_element_chain : Format.formatter -> pattern_element_chain -> unit
val pp_print_relationships_pattern : Format.formatter -> relationships_pattern -> unit
val pp_print_relationship_pattern : Format.formatter -> relationship_pattern -> unit
val pp_print_relationship_detail : Format.formatter -> relationship_detail -> unit
val pp_print_relationship_types : Format.formatter -> relationship_types -> unit
val pp_print_variable_length : Format.formatter -> variable_length -> unit
val pp_print_shortest_path : Format.formatter -> shortest_path -> unit
val pp_print_anonymous_pattern_part : Format.formatter -> anonymous_pattern_part -> unit
val pp_print_pattern_part : Format.formatter -> pattern_part -> unit
val pp_print_pattern : Format.formatter -> pattern -> unit
val pp_print_literal_ids : Format.formatter -> literal_ids -> unit
val pp_print_identified_index_lookup : Format.formatter -> identified_index_lookup -> unit
val pp_print_index_query : Format.formatter -> index_query -> unit
val pp_print_node_lookup : Format.formatter -> node_lookup -> unit
val pp_print_relationship_lookup : Format.formatter -> relationship_lookup -> unit
val pp_print_lookup : Format.formatter -> lookup -> unit
val pp_print_start_point : Format.formatter -> start_point -> unit
val pp_print_where : Format.formatter -> where -> unit
val pp_print_hint : Format.formatter -> hint -> unit
val pp_print_property_expression : Format.formatter -> property_expression -> unit
val pp_print_set_item : Format.formatter -> set_item -> unit
val pp_print_set_clause : Format.formatter -> set_clause -> unit
val pp_print_remove_item : Format.formatter -> remove_item -> unit
val pp_print_return_item : Format.formatter -> return_item -> unit
val pp_print_return_items : Format.formatter -> return_items -> unit
val pp_print_sort_item : Format.formatter -> sort_item -> unit
val pp_print_order : Format.formatter -> order -> unit
val pp_print_skip : Format.formatter -> skip -> unit
val pp_print_limit : Format.formatter -> limit -> unit
val pp_print_return_body : Format.formatter -> return_body -> unit
val pp_print_merge_action : Format.formatter -> merge_action -> unit
val pp_print_load_csv : Format.formatter -> load_csv -> unit
val pp_print_clause : Format.formatter -> clause -> unit
val pp_print_single_query : Format.formatter -> single_query -> unit
val pp_print_union : Format.formatter -> union -> unit
val pp_print_regular_query : Format.formatter -> regular_query -> unit
val pp_print_load_csv_query : Format.formatter -> load_csv_query -> unit
val pp_print_periodic_commit_hint : Format.formatter -> periodic_commit_hint -> unit
val pp_print_bulk_import_query : Format.formatter -> bulk_import_query -> unit
val pp_print_query : Format.formatter -> query -> unit
val pp_print_unique_constraint : Format.formatter -> unique_constraint -> unit
val pp_print_command : Format.formatter -> command -> unit
val pp_print_statement : Format.formatter -> statement -> unit

(*
  Local variables:
  compile-command: "make -C .."
  End:
 *)
