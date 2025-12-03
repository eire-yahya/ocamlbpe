type t

val empty : 'a list
val length : t -> int
val rev : t -> t
val nth : t -> int -> int * (string * string) list
val split : t -> int list * (string * string) list list
val tuplify : Corpus.t -> t
val pretty_print : t -> unit
