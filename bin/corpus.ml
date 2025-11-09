type t = (int * string list) list

let empty = []
let length (corpus : t) = List.length corpus
let rev (corpus : t) = List.rev corpus
let nth (corpus : t) = List.nth corpus
let split (corpus : t) = List.split corpus
let combine int_list corpus_vals : t = List.combine int_list corpus_vals

let word_freq str n =
  let words = String.split_on_char ' ' str in
  let word_index = List.nth words n in
  let filter = List.filter (fun str -> str = word_index) words in
  List.length filter
;;

let create str =
  let rec create_impl str n corpus =
    let words = String.split_on_char ' ' str in
    let word = List.nth words n in
    let word_chars_as_str =
      String.fold_left (fun acc c -> Char.escaped c :: acc) [] word
    in
    let word_freq = word_freq str n in
    let corpus_tuple = word_freq, List.rev word_chars_as_str in
    if n = List.length words - 1
    then List.rev (List.sort_uniq Stdlib.compare corpus)
    else create_impl str (n + 1) (corpus_tuple :: corpus)
  in
  create_impl str 0 []
;;

let pretty_print corpus =
  let pp_int ppf d = Format.fprintf ppf "%d" d in
  let pp_string ppf s = Format.fprintf ppf "%s" s in
  let pp_char_list lst =
    (Format.pp_print_list ~pp_sep:(fun ppf () -> Format.fprintf ppf ", ") pp_string) lst
  in
  let pp_tuple ppf (x, y) = Format.fprintf ppf "(%a, %a)" pp_int x pp_char_list y in
  let pp_corpus crps =
    Format.pp_print_list ~pp_sep:(fun ppf () -> Format.fprintf ppf "\n") pp_tuple crps
  in
  Format.printf "corpus:\n@[<h>@;<0 2>%a@;<0 0>@]\n\n" pp_corpus corpus
;;
