include Result

let (>>=) x f = match x with
  | Ok a -> f a
  | Error b -> Error b

let (>>>) x f = match x with Ok a -> Ok (f a) | Error b -> Error b

let return x = Ok x


(*
  Local variables:
  compile-command: "make -C .."
  End:
 *)
