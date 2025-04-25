type expected_t = PStr | PBool
type result_t =
  | String of string
  | Bool of bool
  | Undefined

val argument_crawl:
  (* argv *)
  string list ->
  (* short, long, type *)
  (string * string * expected_t) list ->
  (* (long, result), positional *)
  ((string * result_t) list * string list)

val parse_arguments:
  string list ->
  (string * string * expected_t) list ->
  (string, result_t) Hashtbl.t * string list
