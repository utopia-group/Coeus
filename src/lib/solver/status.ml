type t = Verified | Unknown | Timeout | Rejected of int list

let pp fmt = function
  | Verified -> Fmt.string fmt "verified"
  | Unknown -> Fmt.string fmt "unknown"
  | Timeout -> Fmt.string fmt "timeout"
  | Rejected preds ->
      Fmt.pf fmt "rejected (blame = @[<h>%a@])"
        (Fmt.list ~sep:Fmt.comma Fmt.int)
        preds
