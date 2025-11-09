type t

val empty : 'a list
val length : t -> int
val rev : t -> t
val nth : t -> int -> int * string list
val split : t -> int list * string list list
val combine : int list -> string list list -> t

(* Gives the number of times a word appears in a string.
   Used in `create_corpus`. *)
val word_freq : string -> int -> int

(* Creates corpus *)
val create : string -> t

(* Pretty printing corpus *)
val pretty_print : t -> unit
