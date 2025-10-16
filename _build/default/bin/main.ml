let example_string =
  "low low low low low lowest lowest newer newer newer newer newer newer wider wider \
   wider new new"
;;

let add_end_of_word_tokens str =
  let words = String.split_on_char ' ' str in
  List.fold_left (fun acc w -> (w ^ "_ ") ^ acc) "" words
;;

let () =
  Printf.printf
    "string w. eow tokens -> string -> %s\n~\n"
    (add_end_of_word_tokens example_string)
;;

let create_vocab str =
  let rec create_char_list str i acc =
    if i = String.length str
    then List.sort_uniq Stdlib.compare acc
    else if str.[i] = ' '
    then create_char_list str (i + 1) acc
    else create_char_list str (i + 1) (str.[i] :: acc)
  in
  create_char_list str 0 []
;;

let print_vocab vocab =
  let pp_char ppf char = Format.fprintf ppf "%c" char in
  Format.printf
    "%a\n~\n"
    (Format.pp_print_list ~pp_sep:(fun out () -> Format.fprintf out ", ") pp_char)
    vocab
;;

let () = print_vocab (create_vocab (add_end_of_word_tokens example_string))

let word_freq str n =
  let words = String.split_on_char ' ' str in
  let word_index = List.nth words n in
  let filter = List.filter (fun str -> str = word_index) words in
  List.length filter
;;

let create_corpus str =
  let rec create_corpus_impl str n corpus =
    let words = String.split_on_char ' ' str in
    let word = List.nth words n in
    let word_chars = String.fold_left (fun acc c -> c :: acc) [] word in
    let word_freq = word_freq str n in
    let corpus_tuple = word_freq, List.rev word_chars in
    if n = List.length words - 1
    then List.rev (List.sort_uniq Stdlib.compare corpus)
    else create_corpus_impl str (n + 1) (corpus_tuple :: corpus)
  in
  create_corpus_impl str 0 []
;;

(* let convert_seq_list seq = Seq.fold_left (fun acc a -> a :: acc) [] seq *)

let pretty_print_corpus corpus =
  let pp_int ppf d = Format.fprintf ppf "%d" d in
  let pp_char ppf c = Format.fprintf ppf "%c" c in
  let pp_char_list lst =
    (Format.pp_print_list ~pp_sep:(fun ppf () -> Format.fprintf ppf ", ") pp_char) lst
  in
  let pp_tuple ppf (x, y) = Format.fprintf ppf "(%a, %a)" pp_int x pp_char_list y in
  let pp_corpus crps =
    Format.pp_print_list ~pp_sep:(fun ppf () -> Format.fprintf ppf "\n") pp_tuple crps
  in
  Format.printf "@[<h>@;<0 2>%a@;<0 0>@]\n~\n" pp_corpus corpus
;;

let pretty_print_pair_list lst =
  let pp_tuple ppf (x, y) = Format.fprintf ppf "(%c, %c)" x y in
  Format.printf
    "%a\n~\n"
    (Format.pp_print_list ~pp_sep:(fun out () -> Format.fprintf out ",\n") pp_tuple)
    lst
;;

(* print the corpus *)
let () = pretty_print_corpus (create_corpus (add_end_of_word_tokens example_string))

let tuplify_string str =
  let str_no_space = String.split_on_char ' ' str in
  let str = String.concat "" str_no_space in
  let rec tuplify_string_impl str acc n =
    if n = String.length str - 1
    then List.rev acc
    else (
      let token_tuple = String.get str n, String.get str (n + 1) in
      tuplify_string_impl str (token_tuple :: acc) (n + 1))
  in
  tuplify_string_impl str [] 0
;;

let () = pretty_print_pair_list (tuplify_string (add_end_of_word_tokens example_string))

let get_token_max tokens =
  let rec get_token_max_impl tokens n token_max token_max_count =
    if n = List.length tokens - 1
    then token_max
    else (
      let current_token = List.nth tokens n in
      let token_count = List.length (List.filter (fun t -> t = current_token) tokens) in
      if token_count > token_max_count
      then get_token_max_impl tokens (n + 1) current_token token_count
      else get_token_max_impl tokens (n + 1) token_max token_max_count)
  in
  get_token_max_impl tokens 0 (' ', ' ') 0
;;

(* printing most freq tokens *)
let () =
  let str = get_token_max (tuplify_string (add_end_of_word_tokens example_string)) in
  let print_tuple_func fmt (x, y) =
    Format.fprintf fmt "(most common token -> %c, %c)" x y
  in
  print_tuple_func Format.std_formatter str
;;

(* let token_learner (token_max : char * char) (corpus : (int * char list) list) vocab = *)
(* let corpus_vals = snd (List.split corpus) in *)
(* let token_max_fst = (fst token_max) in *)
(* let token_max_snd = (snd token_max) in *)
(* let token_max_both = Char.escaped token_max_fst ^ Char.escaped token_max_snd in *)
(* let rec token_learner_impl n = *)
(* let corpus_vals_n = List.nth corpus_vals n in *)
(* let fst_token_max_index_opt = List.find_index (fun t -> t = token_max_fst) corpus_vals_n in *)
(* Option.get fst_token_max_index_opt *)

let rec replace_fst_token_max n corpus_vals fst_token =
  let replaced_w_fst_token = 
  List.fold_left
    (fun acc t ->
       if t = fst_token then fst_token :: acc else t :: acc)
    []
    corpus_vals
  in

    
;;
