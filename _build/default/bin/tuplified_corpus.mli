type t

val length : t -> int
val rev : t -> t
val nth : t -> int -> int * (string * string) list
val tuplify : Corpus.t -> t
val pretty_print : t -> unit
