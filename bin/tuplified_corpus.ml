type t = (int * (string * string) list) list

let empty = []
let length (tuplified_corpus : t) = List.length tuplified_corpus
let rev (tuplified_corpus : t) = List.rev tuplified_corpus
let nth (tuplified_corpus : t) = List.nth tuplified_corpus
let split (tuplified_corpus : t) = List.split tuplified_corpus

let tuplify (corpus : Corpus.t) =
  let rec tuplify_impl (corpus : Corpus.t) acc n =
    if n = Corpus.length corpus
    then List.rev acc
    else (
      let current_corpus_val = Corpus.nth corpus n in
      let current_cv_freq = fst current_corpus_val in
      let current_cv_val = snd current_corpus_val in
      let rec tuplify_cv_val acc2 n2 =
        if n2 = List.length current_cv_val - 1
        then List.rev acc2
        else (
          let tuplify_current_cv_val =
            List.nth current_cv_val n2, List.nth current_cv_val (n2 + 1)
          in
          tuplify_cv_val (tuplify_current_cv_val :: acc2) (n2 + 1))
      in
      let current_cv_tuplified = current_cv_freq, tuplify_cv_val [] 0 in
      tuplify_impl corpus (current_cv_tuplified :: acc) (n + 1))
  in
  tuplify_impl corpus [] 0
;;

let pretty_print (tuplified_corpus : t) =
  let pp_str_tuple ppf (x, y) = Format.fprintf ppf "(%s, %s)" x y in
  let pp_str_tuple_lst tuplified_corpus =
    Format.pp_print_list pp_str_tuple tuplified_corpus
  in
  let pp_corpus_tuplified_val ppf (x, y) =
    Format.fprintf ppf "%d:%a" x pp_str_tuple_lst y
  in
  let pp_corpus_tuplifed =
    Format.pp_print_list
      ~pp_sep:(fun out () -> Format.fprintf out "\n")
      pp_corpus_tuplified_val
  in
  Format.printf
    "tuplified_corpus:\n@[<h>@;<0 2>%a@;<0 0>@]\n\n"
    pp_corpus_tuplifed
    tuplified_corpus
;;
