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

type uop =
  | Not
  | UPlus
  | UMinus

type bop =
  | Or
  | Xor
  | And
  | Eq
  | Ne
  | BNe
  | Lt
  | Gt
  | Lte
  | Gte
  | Add
  | Sub
  | Mul
  | Div
  | Mod
  | Pow

type expression =
  | Literal of literal
  | Parameter of parameter
  | True | False | Null
  | Case of case
  | Count_star
  | Map of map
  | List_comprehension of list_comprehension
  | Collection of expression list
  | Filter of filter
  | Extract of filter * expression option
  | Reduce of variable * expression * id_in_coll * expression
  | All_pred of filter
  | Any_pred of filter
  | None_pred of filter
  | Single_pred of filter
  | Shortest_path of shortest_path
  | Relationships_pattern of relationships_pattern
  | Fun_invocation of fun_invocation
  | Variable of variable
  | Uexpr of uop * expression
  | Bexpr of expression * bop * expression
  | Container_index of expression * expression
  | Collection_slice of expression * expression option * expression option
  | Regex_match of expression * expression
  | In of expression * expression
  | Starts_with of expression * expression
  | Ends_with of expression * expression
  | Contains of expression * expression
  | Is_null of expression
  | Is_not_null of expression
  | Property_lookup of property_lookup
  | Has_labels of expression * [`Property_lookup of property_lookup |
                                `Node_labels of node_labels] list

and property_lookup =
  | Legacy_property of expression * property_key_name * legacy_operator
  | Property of expression * property_key_name

and filter = id_in_coll * expression option

and id_in_coll = variable * expression

and map = (property_key_name * expression) list

and list_comprehension = filter * expression option

and case = expression option * case_alternatives ne_list * expression option

and case_alternatives = expression * expression

and fun_invocation = function_name * bool * expression list

and node_pattern = variable option * node_labels option * properties option

and properties = [`Map of map | `Parameter of parameter]

and pattern_element = node_pattern * pattern_element_chain list

and pattern_element_chain = relationship_pattern * node_pattern

and relationships_pattern = node_pattern * pattern_element_chain ne_list

and relationship_pattern = [`Both | `Incoming | `Outgoing] * relationship_detail

and relationship_detail =
  variable option * bool * relationship_types * variable_length option
  * properties option

and relationship_types = rel_type_name list

and variable_length = range option

and shortest_path = bool * pattern_element

type anonymous_pattern_part =
  | Shortest_path_pattern of shortest_path
  | Pattern_element of pattern_element

type pattern_part = variable option * anonymous_pattern_part

type pattern = pattern_part ne_list

type literal_ids = int ne_list

type identified_index_lookup =
  symbolic_name * symbolic_name * [`String of string | `Parameter of parameter]

type index_query =
  symbolic_name * [`String of string | `Parameter of parameter]

type node_lookup =
  | Node_by_ids of literal_ids
  | Node_by_parameter of parameter
  | Node_by_identified_index of identified_index_lookup
  | Node_by_index_query of index_query
  | All_nodes

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

type where = expression

type hint =
  | Using_index of variable * node_label * property_key_name
  | Using_join of variable ne_list
  | Using_scan of variable * node_label

type property_expression = expression * property_lookup ne_list

type set_item =
  | Set_property_item of property_expression * expression
  | Set_exact_properties of variable * expression
  | Set_including_properties of variable * expression
  | Set_label_item of variable * node_labels

type set_clause = set_item ne_list

type remove_item =
  | Remove_label_item of variable * node_labels
  | Remove_property_item of property_expression

type return_item = expression * variable option

type return_items =
  | Include_existing of return_item list
  | Items of return_item ne_list

type sort_item = expression * [`ASC | `DESC]

type order = sort_item ne_list

type skip = expression

type limit = expression

type return_body = return_items * order option * skip option * limit option

type merge_action =
  | On_match of set_clause
  | On_create of set_clause

type load_csv = bool * expression * variable * string option

type clause =
  | LoadCSV of load_csv
  | Start of start_point ne_list * where option
  | Match of bool * pattern * hint list * where option
  | Unwind of expression * variable
  | Merge of pattern_part * merge_action list
  | Create of bool * pattern
  | Set of set_item ne_list
  | Delete of bool * expression ne_list
  | Remove of remove_item ne_list
  | Foreach of variable * expression * clause ne_list
  | With of bool * return_body * where option
  | Return of bool * return_body

type single_query = clause ne_list

type union =
  | UnionAll of single_query
  | Union of single_query

type regular_query = single_query * union list

type load_csv_query = load_csv * clause list

type periodic_commit_hint = int option

type bulk_import_query = periodic_commit_hint * load_csv_query

type query =
  | Regular of regular_query
  | Bulk_import of bulk_import_query

type unique_constraint = variable * node_label * property_expression

type command =
  | Create_index of node_label * property_key_name
  | Drop_index of node_label * property_key_name
  | Create_node_constraint of
      variable * node_label * property_expression * [`UNIQUE | `EXISTS]
  | Create_relationship_constraint of
      [`None | `Left | `Right] * variable * rel_type_name * property_expression
  | Drop_node_constraint of
      variable * node_label * property_expression * [`UNIQUE | `EXISTS]
  | Drop_relationship_constraint of
      [`None | `Left | `Right] * variable * rel_type * property_expression
  | Call of symbolic_name list * procedure_name * expression list

type statement =
  | Command of command
  | Query of query

module F = Format

let rec pp_print_list f sep fmt = function
  | []     -> ()
  | [x]    -> f fmt x
  | h :: t -> F.fprintf fmt "%a%a%a" f h sep () (pp_print_list f sep) t

let rec pp_print_option f fmt = function
  | None   -> ()
  | Some x -> f fmt x

let pp_print_comma fmt () = F.fprintf fmt ",@ "

let pp_print_symbolic_name = F.pp_print_string
let pp_print_variable = F.pp_print_string
let pp_print_property_key_name = F.pp_print_string
let pp_print_legacy_operator = F.pp_print_string
let pp_print_function_name = F.pp_print_string
let pp_print_procedure_name = F.pp_print_string

let pp_print_rel_type_name = F.pp_print_string
let pp_print_rel_type fmt s = F.pp_print_string fmt (":" ^ s)

let pp_print_label_name = F.pp_print_string
let pp_print_node_label = F.pp_print_string

let pp_print_literal fmt = function
  | Int i    -> F.pp_print_int fmt i
  | Float f  -> F.pp_print_float fmt f
  | String s -> F.pp_print_string fmt (String.escaped s) (* TODO *)

let pp_print_range fmt = function
  | (None, None) -> F.pp_print_string fmt ".."
  | (Some n1, Some n2) when n1 = n2 -> F.pp_print_int fmt n1
  | (Some n1, Some n2) -> F.fprintf fmt "%d..%d" n1 n2
  | (Some n, None) -> F.fprintf fmt "%d.." n
  | (None, Some n) -> F.fprintf fmt "..%d" n

let pp_print_parameter fmt p = F.fprintf fmt "{%s}" p

let pp_print_node_labels fmt (n, nl) =
  pp_print_list pp_print_node_label F.pp_print_space fmt (n :: nl)

let pp_print_uop fmt = function
  | Not    -> F.pp_print_string fmt "NOT"
  | UPlus  -> F.pp_print_string fmt "+"
  | UMinus -> F.pp_print_string fmt "-"

let pp_print_bop fmt = function
  | Or  -> F.pp_print_string fmt "OR"
  | Xor -> F.pp_print_string fmt "XOR"
  | And -> F.pp_print_string fmt "AND"
  | Eq  -> F.pp_print_string fmt "="
  | Ne  -> F.pp_print_string fmt "<>"
  | BNe -> F.pp_print_string fmt "!="
  | Lt  -> F.pp_print_string fmt "<"
  | Gt  -> F.pp_print_string fmt ">"
  | Lte -> F.pp_print_string fmt "<="
  | Gte -> F.pp_print_string fmt ">="
  | Add -> F.pp_print_string fmt "+"
  | Sub -> F.pp_print_string fmt "-"
  | Mul -> F.pp_print_string fmt "*"
  | Div -> F.pp_print_string fmt "/"
  | Mod -> F.pp_print_string fmt "%"
  | Pow -> F.pp_print_string fmt "^"

let rec pp_print_expression fmt = function
  | Literal l ->
      pp_print_literal fmt l

  | Parameter p ->
      pp_print_parameter fmt p

  | True  -> F.pp_print_string fmt "TRUE"
  | False -> F.pp_print_string fmt "FALSE"
  | Null  -> F.pp_print_string fmt "NULL"

  | Case c ->
      pp_print_case fmt c

  | Count_star ->
      F.pp_print_string fmt "COUNT(*)"

  | Map m ->
      pp_print_map fmt m

  | List_comprehension lc ->
      pp_print_list_comprehension fmt lc

  | Collection es ->
      F.fprintf fmt "[%a]" (pp_print_list pp_print_expression pp_print_comma) es

  | Filter f ->
      F.fprintf fmt "FILTER(@[%a@])" pp_print_filter f

  | Extract (f, None) ->
      F.fprintf fmt "EXTRACT(@[%a@])" pp_print_filter f

  | Extract (f, Some e) ->
      F.fprintf fmt "EXTRACT(@[%a | @[%a@]@])"
        pp_print_filter f
        pp_print_expression e

  | Reduce (v, e1, i, e2) ->
      F.fprintf fmt "REDUCE(@[%a = @[%a, @[%a | @[%a@]@]@]@])"
        pp_print_variable v
        pp_print_expression e1
        pp_print_id_in_coll i
        pp_print_expression e2

  | All_pred f    -> F.fprintf fmt "ALL(@[%a@])" pp_print_filter f
  | Any_pred f    -> F.fprintf fmt "ANY(@[%a@])" pp_print_filter f
  | None_pred f   -> F.fprintf fmt "NONE(@[%a@])" pp_print_filter f
  | Single_pred f -> F.fprintf fmt "SINGLE(@[%a@])" pp_print_filter f

  | Shortest_path s ->
      pp_print_shortest_path fmt s

  | Relationships_pattern p ->
      pp_print_relationships_pattern fmt p

  | Fun_invocation f ->
      pp_print_fun_invocation fmt f

  | Variable v -> pp_print_variable fmt v

  | Uexpr (o, e) ->
      F.fprintf fmt "%a%a"
        pp_print_uop o
        pp_print_pexpression e

  | Bexpr (e1, o, e2) ->
      F.fprintf fmt "@[%a @[%a @[%a@]@]@]"
        pp_print_pexpression e1
        pp_print_bop o
        pp_print_pexpression e2

  | Container_index (e1, e2) ->
      F.fprintf fmt "%a[@[%a@]]"
        pp_print_expression e1
        pp_print_expression e2

  | Collection_slice (e, None, None) ->
      F.fprintf fmt "%a[..]" pp_print_expression e

  | Collection_slice (e, Some e1, None) ->
      F.fprintf fmt "%a[@[%a..@]]"
        pp_print_expression e
        pp_print_expression e1

  | Collection_slice (e, None, Some e1) ->
      F.fprintf fmt "%a[@[..@[%a@]@]]"
        pp_print_expression e
        pp_print_expression e1

  | Collection_slice (e, Some e1, Some e2) ->
      F.fprintf fmt "%a[@[%a..@[%a@]@]]"
        pp_print_expression e
        pp_print_expression e1
        pp_print_expression e2

  | Regex_match (e1, e2) ->
      F.fprintf fmt "%a =~ @[%a@]"
        pp_print_expression e1
        pp_print_expression e2

  | In (e1, e2) ->
      F.fprintf fmt "%a IN @[%a@]"
        pp_print_expression e1
        pp_print_expression e2

  | Starts_with (e1, e2) ->
      F.fprintf fmt "%a STARTS WITH @[%a@]"
        pp_print_expression e1
        pp_print_expression e2

  | Ends_with (e1, e2) ->
      F.fprintf fmt "%a ENDS WITH @[%a@]"
        pp_print_expression e1
        pp_print_expression e2

  | Contains (e1, e2) ->
      F.fprintf fmt "%a CONTAINS @[%a@]"
        pp_print_expression e1
        pp_print_expression e2

  | Is_null e ->
      F.fprintf fmt "%a IS NULL" pp_print_expression e

  | Is_not_null e ->
      F.fprintf fmt "%a IS NOT NULL" pp_print_expression e

  | Property_lookup l ->
      pp_print_property_lookup fmt l

  | Has_labels (e, l) ->
      F.fprintf fmt "%a @[%a@]"
        pp_print_expression e
        (pp_print_list (fun fmt -> function
             | `Property_lookup l -> pp_print_property_lookup fmt l
             | `Node_labels l     -> pp_print_node_labels fmt l
           ) F.pp_print_space)
        l

and pp_print_pexpression fmt = function
  | Literal _ | Parameter _ | True | False | Null | Count_star
  | Map _ | List_comprehension _ | Collection _  | Filter _
  | Extract _ | Reduce _ | All_pred _ | Any_pred _ | None_pred _ | Single_pred _
  | Shortest_path _ | Relationships_pattern _ | Fun_invocation _
  | Variable _ as e ->
      pp_print_expression fmt e

  | Case _ | Uexpr _ | Bexpr _ | Container_index _ | Collection_slice _
  | Regex_match _ | In _ | Starts_with _ | Ends_with _ | Contains _
  | Is_null _ | Is_not_null _ | Property_lookup _ | Has_labels _ as e ->
      F.fprintf fmt "(@[%a@])" pp_print_expression e

and pp_print_property_lookup fmt = function
  | Legacy_property (e, k, o) ->
      F.fprintf fmt "%a.%a%a"
        pp_print_expression e
        pp_print_property_key_name k
        pp_print_legacy_operator o

  | Property (e, k) ->
      F.fprintf fmt "%a.%a"
        pp_print_expression e
        pp_print_property_key_name k

and pp_print_filter fmt = function
  | (i, None)   -> pp_print_id_in_coll fmt i
  | (i, Some e) ->
      F.fprintf fmt "%a WHERE @[%a@]"
        pp_print_id_in_coll i pp_print_expression e

and pp_print_id_in_coll fmt (v, e) =
  F.fprintf fmt "%a IN @[%a@]" pp_print_variable v pp_print_expression e

and pp_print_map fmt l =
  F.fprintf fmt "{ @[%a@] }"
    (pp_print_list (fun fmt (k, e) ->
         F.fprintf fmt "%a : @[%a@]"
           pp_print_property_key_name k
           pp_print_expression e
       ) pp_print_comma)
    l

and pp_print_list_comprehension fmt = function
  | (f, None)   -> F.fprintf fmt "[@[%a@]]" pp_print_filter f
  | (f, Some e) ->
      F.fprintf fmt "[%a | @[%a@]]"
        pp_print_filter f
        pp_print_expression e

and pp_print_case fmt (ce, (a, al), ee) =
  let () =
    match ce with
    | None   -> F.fprintf fmt "CASE "
    | Some e -> F.fprintf fmt "CASE @[%a@] " pp_print_expression e
  in
  let () =
    pp_print_list pp_print_case_alternatives F.pp_print_space fmt (a :: al)
  in
  match ee with
  | None   -> ()
  | Some e -> F.fprintf fmt " ELSE @[%a@]" pp_print_expression e

and pp_print_case_alternatives fmt (e1, e2) =
  F.fprintf fmt "WHEN @[%a THEN @[%a@]@]"
    pp_print_expression e1
    pp_print_expression e2

and pp_print_fun_invocation fmt (n, d, es) =
  F.fprintf fmt "%a(@[%s@[%a@]@])"
    pp_print_function_name n
    (if d then "DISTINCT " else "")
    (pp_print_list pp_print_expression pp_print_comma) es

and pp_print_node_pattern fmt (v, nl, p) =
  F.fprintf fmt "(@[";
  let () =
    match v with
    | None   -> ()
    | Some v -> F.fprintf fmt "%a@ " pp_print_variable v
  in
  let () =
    match nl with
    | None    -> ()
    | Some nl -> F.fprintf fmt "%a@ " pp_print_node_labels nl
  in
  let () =
    match p with
    | None    -> ()
    | Some p -> F.fprintf fmt "%a" pp_print_properties p
  in
  F.fprintf fmt "@])"

and pp_print_properties fmt = function
  | `Map m       -> pp_print_map fmt m
  | `Parameter p -> pp_print_parameter fmt p

and pp_print_pattern_element fmt (n, l) =
  F.fprintf fmt "(@[%a" pp_print_node_pattern n;
  match l with
  | []-> F.fprintf fmt "@])"
  | _ ->
      F.fprintf fmt "@ @[%a@]@])"
        (pp_print_list pp_print_pattern_element_chain F.pp_print_space) l

and pp_print_pattern_element_chain fmt (r, n) =
  F.fprintf fmt "%a@[%a@]" pp_print_relationship_pattern r pp_print_node_pattern n

and pp_print_relationships_pattern fmt (n, (c, cl)) =
  F.fprintf fmt "%a @[%a@]"
    pp_print_node_pattern n
    (pp_print_list pp_print_pattern_element_chain F.pp_print_space) (c :: cl)

and pp_print_relationship_pattern fmt = function
  | (`Both, d)     -> F.fprintf fmt "<-%a->" pp_print_relationship_detail d
  | (`Incoming, d) -> F.fprintf fmt "<-%a-" pp_print_relationship_detail d
  | (`Outgoing, d) -> F.fprintf fmt "-%a->" pp_print_relationship_detail d

and pp_print_relationship_detail fmt (v, b, t, l, p) =
  F.fprintf fmt "[@[%a%s%a%a%a@]]"
    (pp_print_option pp_print_variable) v
    (if b then "?" else "")
    pp_print_relationship_types t
    (pp_print_option pp_print_variable_length) l
    (pp_print_option pp_print_properties) p

and pp_print_relationship_types fmt = function
  | [] -> ()
  | l  ->
      F.fprintf fmt ":%a"
        (pp_print_list pp_print_rel_type_name
           (fun fmt () -> F.pp_print_string fmt " | ")) l

and pp_print_variable_length fmt = function
  | None   -> F.pp_print_string fmt "*"
  | Some r -> F.fprintf fmt "*%a" pp_print_range r

and pp_print_shortest_path fmt = function
  | (false, e) ->
      F.fprintf fmt "shortestPath(@[%a@])" pp_print_pattern_element e

  | (true, e) ->
      F.fprintf fmt "allShortestPath(@[%a@])" pp_print_pattern_element e

and pp_print_anonymous_pattern_part fmt = function
  | Shortest_path_pattern p -> pp_print_shortest_path fmt p
  | Pattern_element e       -> pp_print_pattern_element fmt e

and pp_print_pattern_part fmt = function
  | (None, p)   -> pp_print_anonymous_pattern_part fmt p
  | (Some v, p) ->
      F.fprintf fmt "%a=%a"
        pp_print_variable v
        pp_print_anonymous_pattern_part p

and pp_print_pattern fmt (p, pl) =
  pp_print_list pp_print_pattern_part pp_print_comma fmt (p :: pl)

let pp_print_literal_ids fmt (i, is) =
  pp_print_list F.pp_print_int pp_print_comma fmt (i :: is)

let pp_print_identified_index_lookup fmt = function
  | (n1, n2, `String s)    -> F.fprintf fmt ":%s(%s = %s)" n1 n2 s
  | (n1, n2, `Parameter p) ->
      F.fprintf fmt ":%s(%s = %a)" n1 n2 pp_print_parameter p

let pp_print_index_query fmt = function
  | (n, `String s)    -> F.fprintf fmt ":%s(%s)" n s
  | (n, `Parameter p) -> F.fprintf fmt ":%s(%a)" n pp_print_parameter p

let pp_print_node_lookup fmt = function
  | Node_by_ids is ->
      F.fprintf fmt "NODE(@[%a@])" pp_print_literal_ids is

  | Node_by_parameter p ->
      F.fprintf fmt "NODE(@[%a@])" pp_print_parameter p

  | Node_by_identified_index iil ->
      F.fprintf fmt "NODE(@[%a@])" pp_print_identified_index_lookup iil

  | Node_by_index_query iq ->
      F.fprintf fmt "NODE(@[%a@])" pp_print_index_query iq

  | All_nodes ->
      F.pp_print_string fmt "NODE(*)"

let pp_print_relationship_lookup fmt = function
  | Relationship_by_ids is ->
      F.fprintf fmt "REL(@[%a@])" pp_print_literal_ids is

  | Relationship_by_parameter p ->
      F.fprintf fmt "REL(@[%a@])" pp_print_parameter p

  | Relationship_by_identified_index iil ->
      F.fprintf fmt "REL(@[%a@])" pp_print_identified_index_lookup iil

  | Relationship_by_index_query iq ->
      F.fprintf fmt "REL(@[%a@])" pp_print_index_query iq

  | All_relationships ->
      F.pp_print_string fmt "REL(*)"

let pp_print_lookup fmt = function
  | Node_lookup nl         -> pp_print_node_lookup fmt nl
  | Relationship_lookup rl -> pp_print_relationship_lookup fmt rl

let pp_print_start_point fmt (v, l) =
  F.fprintf fmt "%a = @[%a@]"
    pp_print_variable v
    pp_print_lookup l

let pp_print_where fmt e =
  F.fprintf fmt "WHERE @[%a@]" pp_print_expression e

let pp_print_hint fmt = function
  | Using_index (v, nl, k) ->
      F.fprintf fmt "USING INDEX %a%a(%a)"
        pp_print_variable v
        pp_print_node_label nl
        pp_print_property_key_name k

  | Using_join (v, vs) ->
      F.fprintf fmt "USING JOIN ON %a"
        (pp_print_list pp_print_variable pp_print_comma)
        (v :: vs)

  | Using_scan (v, nl) ->
      F.fprintf fmt "USING SCAN %a%a"
        pp_print_variable v
        pp_print_node_label nl

let pp_print_property_expression fmt (e, (pl, pls)) =
  F.fprintf fmt "%a.@[%a@]"
    pp_print_expression e
    (pp_print_list pp_print_property_lookup (fun fmt () -> F.pp_print_char fmt '.'))
    (pl :: pls)

let pp_print_set_item fmt = function
  | Set_property_item (pe, e) ->
      F.fprintf fmt "%a = @[%a@]"
        pp_print_property_expression pe
        pp_print_expression e

  | Set_exact_properties (v, e) ->
      F.fprintf fmt "%a = @[%a@]"
        pp_print_variable v
        pp_print_expression e

  | Set_including_properties (v, e) ->
      F.fprintf fmt "%a += @[%a@]"
        pp_print_variable v
        pp_print_expression e

  | Set_label_item (v, nls) ->
      F.fprintf fmt "%a%a"
        pp_print_variable v
        pp_print_node_labels nls

let pp_print_set_clause fmt (si, sis) =
  F.fprintf fmt "SET @[%a@]"
    (pp_print_list pp_print_set_item pp_print_comma) (si :: sis)

let pp_print_remove_item fmt = function
  | Remove_label_item (v, nls) ->
      F.fprintf fmt "%a%a" pp_print_variable v pp_print_node_labels nls

  | Remove_property_item pe ->
      pp_print_property_expression fmt pe

let pp_print_return_item fmt (e, vopt) =
  pp_print_expression fmt e;
  match vopt with
  | None   -> ()
  | Some v -> F.fprintf fmt " AS %a" pp_print_variable v

let pp_print_return_items fmt = function
  | Include_existing ris ->
      F.fprintf fmt "*, %a"
        (pp_print_list pp_print_return_item pp_print_comma) ris

  | Items (ri, ris) ->
      pp_print_list pp_print_return_item pp_print_comma fmt (ri :: ris)

let pp_print_sort_item fmt (e, o) =
  F.fprintf fmt "@[%a %s@]"
    pp_print_expression e
    (match o with `DESC -> "DESC" | `ASC -> "ASC")

let pp_print_order fmt (si, sis) =
  F.fprintf fmt "ORDER BY @[%a@]"
    (pp_print_list pp_print_sort_item pp_print_comma) (si :: sis)

let pp_print_skip fmt e =
  F.fprintf fmt "SKIP @[%a@]" pp_print_expression e

let pp_print_limit fmt e =
  F.fprintf fmt "LIMIT @[%a@]" pp_print_expression e

let pp_print_return_body fmt (ri, oo, so, lo) =
  F.fprintf fmt "%a" pp_print_return_items ri;
  let () =
    match oo with
    | None   -> ()
    | Some o -> F.fprintf fmt " @[%a@]" pp_print_order o
  in
  let () =
    match so with
    | None   -> ()
    | Some s -> F.fprintf fmt " @[%a@]" pp_print_skip s
  in
  let () =
    match lo with
    | None   -> ()
    | Some l -> F.fprintf fmt " @[%a@]" pp_print_limit l
  in
  ()

let pp_print_merge_action fmt = function
  | On_match sc  -> F.fprintf fmt "ON MATCH @[%a@]" pp_print_set_clause sc
  | On_create sc -> F.fprintf fmt "ON CREATE @[%a@]" pp_print_set_clause sc

let pp_print_load_csv fmt (wh, e, v, ft) =
  F.fprintf fmt "LOAD CSV%s FROM @[%a AS %a"
    (if wh then " WITH HEADERS" else "")
    pp_print_expression e
    pp_print_variable v;
  match ft with
  | None    -> F.fprintf fmt "@]"
  | Some ft -> F.fprintf fmt " FIELDTERMINATOR %s@]" ft

let rec pp_print_clause fmt = function
  | LoadCSV lc ->
      pp_print_load_csv fmt lc

  | Start ((sp, sps), wopt) ->
      F.fprintf fmt "START @[%a"
        (pp_print_list pp_print_start_point pp_print_comma) (sp :: sps);
      let () =
        match wopt with
        | None   -> ()
        | Some w -> F.fprintf fmt " @[%a@]" pp_print_where w
      in
      F.fprintf fmt "@]"

  | Match (b, p, hs, wopt) ->
      F.fprintf fmt "%sMATCH @[%a @[%a"
        (if b then "OPTIONAL " else "")
        pp_print_pattern p
        (pp_print_list pp_print_hint F.pp_print_space) hs;
      let () =
        match wopt with
        | None   -> ()
        | Some w -> F.fprintf fmt " @[%a@]" pp_print_where w
      in
      F.fprintf fmt "@]@]"

  | Unwind (e, v) ->
      F.fprintf fmt "UNWIND @[%a AS @[%a@]@]"
        pp_print_expression e
        pp_print_variable v

  | Merge (p, ms) ->
      F.fprintf fmt "MERGE @[%a @[%a@]@]"
        pp_print_pattern_part p
        (pp_print_list pp_print_merge_action F.pp_print_space) ms

  | Create (b, p) ->
      F.fprintf fmt "CREATE%s @[%a@]"
        (if b then " UNIQUE" else "")
        pp_print_pattern p

  | Set (si, sis) ->
      F.fprintf fmt "SET @[%a@]"
        (pp_print_list pp_print_set_item pp_print_comma) (si :: sis)

  | Delete (b, (e, es)) ->
      F.fprintf fmt "%sDELETE @[%a@]"
        (if b then "DETACH " else "")
        (pp_print_list pp_print_expression pp_print_comma) (e :: es)

  | Remove (r, rs) ->
      F.fprintf fmt "REMOVE @[%a@]"
        (pp_print_list pp_print_remove_item pp_print_comma) (r :: rs)

  | Foreach (v, e, (c, cs)) ->
      F.fprintf fmt "FOREACH (@[%a IN @[%a | @[%a@]@]@])"
        pp_print_variable v
        pp_print_expression e
        (pp_print_list pp_print_clause F.pp_print_space) (c :: cs)

  | With (b, r, w) ->
      F.fprintf fmt "WITH%s @[%a"
        (if b then " DISTINCT" else "")
        pp_print_return_body r;
      let () =
        match w with
        | None   -> ()
        | Some w -> F.fprintf fmt " @[%a@]" pp_print_where w
      in
      F.fprintf fmt "@]"

  | Return (b, r) ->
      F.fprintf fmt "RETURN%s @[%a@]"
        (if b then " DISTINCT" else "")
        pp_print_return_body r

let pp_print_single_query fmt (c, cs) =
  pp_print_list pp_print_clause F.pp_print_space fmt (c :: cs)

let pp_print_union fmt = function
  | UnionAll sq -> F.fprintf fmt "UNION ALL @[%a@]" pp_print_single_query sq
  | Union sq    -> F.fprintf fmt "UNION @[%a@]" pp_print_single_query sq

let pp_print_regular_query fmt = function
  | (sq, []) -> pp_print_single_query fmt sq
  | (sq, us) ->
      F.fprintf fmt "%a @[%a@]"
        pp_print_single_query sq
        (pp_print_list pp_print_union F.pp_print_space) us

let pp_print_load_csv_query fmt (l, cs) =
  F.fprintf fmt "%a @[%a@]"
    pp_print_load_csv l
    (pp_print_list pp_print_clause F.pp_print_space) cs

let pp_print_periodic_commit_hint fmt = function
  | None   -> F.pp_print_string fmt "USING PERIODIC COMMIT"
  | Some i -> F.fprintf fmt "USING PERIODIC COMMIT %d" i

let pp_print_bulk_import_query fmt (h, q) =
  F.fprintf fmt "%a @[%a@]"
    pp_print_periodic_commit_hint h
    pp_print_load_csv_query q

let pp_print_query fmt = function
  | Regular q     -> pp_print_regular_query fmt q
  | Bulk_import q -> pp_print_bulk_import_query fmt q

let pp_print_unique_constraint fmt (v, nl, pe) =
  F.fprintf fmt "CONSTRAINT ON (%a%a) ASSERT @[%a IS UNIQUE@]"
    pp_print_variable v
    pp_print_node_label nl
    pp_print_property_expression pe

let pp_print_command fmt = function
  | Create_index (l, k) ->
      F.fprintf fmt "CREATE INDEX ON %a(%a)"
        pp_print_node_label l
        pp_print_property_key_name k

  | Drop_index (l, k) ->
      F.fprintf fmt "DROP INDEX ON %a(%a)"
        pp_print_node_label l
        pp_print_property_key_name k

  | Create_node_constraint (v, nl, pe, `UNIQUE) ->
      F.fprintf fmt "CREATE CONSTRAINT ON (%a%a) ASSERT @[%a@] IS_UNIQUE"
        pp_print_variable v
        pp_print_node_label nl
        pp_print_property_expression pe

  | Create_node_constraint (v, nl, pe, `EXISTS) ->
      F.fprintf fmt "CREATE CONSTRAINT ON (%a%a) ASSERT EXISTS @[%a@]"
        pp_print_variable v
        pp_print_node_label nl
        pp_print_property_expression pe

  | Create_relationship_constraint (d, v, t, pe) ->
      F.fprintf fmt "CREATE CONSTRAINT ON %s%a%a%s ASSERT EXISTS (@[%a@])"
        (match d with `None | `Right -> "()-[" | `Left -> "()<-[")
        pp_print_variable v
        pp_print_rel_type t
        (match d with `None | `Left -> "]-()" | `Right -> "]->()")
        pp_print_property_expression pe

  | Drop_node_constraint (v, nl, pe, `UNIQUE) ->
      F.fprintf fmt "DROP CONSTRAINT ON (%a%a) ASSERT @[%a@] IS_UNIQUE"
        pp_print_variable v
        pp_print_node_label nl
        pp_print_property_expression pe

  | Drop_node_constraint (v, nl, pe, `EXISTS) ->
      F.fprintf fmt "DROP CONSTRAINT ON (%a%a) ASSERT EXISTS (@[%a@])"
        pp_print_variable v
        pp_print_node_label nl
        pp_print_property_expression pe

  | Drop_relationship_constraint (d, v, t, pe) ->
      F.fprintf fmt "DROP CONSTRAINT ON %s%a%a%s ASSERT EXISTS (@[%a@])"
        (match d with `None | `Right -> "()-[" | `Left -> "()<-[")
        pp_print_variable v
        pp_print_rel_type t
        (match d with `None | `Left -> "]-()" | `Right -> "]->()")
        pp_print_property_expression pe

  | Call (ns, pn, es) ->
      F.fprintf fmt "CALL %s(%a)"
        (String.concat "." (ns @ [pn]))
        (pp_print_list pp_print_expression pp_print_comma) es

let pp_print_statement fmt = function
  | Command c -> pp_print_command fmt c
  | Query q   -> pp_print_query fmt q

(*
  Local variables:
  compile-command: "make -C .."
  End:
 *)
