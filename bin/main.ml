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
    let word_chars_as_str =
      String.fold_left (fun acc c -> Char.escaped c :: acc) [] word
    in
    let word_freq = word_freq str n in
    let corpus_tuple = word_freq, List.rev word_chars_as_str in
    if n = List.length words - 1
    then List.rev (List.sort_uniq Stdlib.compare corpus)
    else create_corpus_impl str (n + 1) (corpus_tuple :: corpus)
  in
  create_corpus_impl str 0 []
;;

(* let convert_seq_list seq = Seq.fold_left (fun acc a -> a :: acc) [] seq *)

let pretty_print_corpus corpus =
  let pp_int ppf d = Format.fprintf ppf "%d" d in
  let pp_string ppf s = Format.fprintf ppf "%s" s in
  let pp_char_list lst =
    (Format.pp_print_list ~pp_sep:(fun ppf () -> Format.fprintf ppf ", ") pp_string) lst
  in
  let pp_tuple ppf (x, y) = Format.fprintf ppf "(%a, %a)" pp_int x pp_char_list y in
  let pp_corpus crps =
    Format.pp_print_list ~pp_sep:(fun ppf () -> Format.fprintf ppf "\n") pp_tuple crps
  in
  Format.printf "@[<h>@;<0 2>%a@;<0 0>@]\n~\n" pp_corpus corpus
;;

let pretty_print_pair_list lst =
  let pp_str_tuple ppf (x, y) = Format.fprintf ppf "(%S, %S)" x y in
  let pp_str_tuple_lst lst = Format.pp_print_list pp_str_tuple lst in
  let pp_corpus_tuplified ppf (x, y) =
    Format.fprintf ppf "(%d, %a)" x pp_str_tuple_lst y
  in
  Format.printf
    "%a\n~\n"
    (Format.pp_print_list
       ~pp_sep:(fun out () -> Format.fprintf out ",\n")
       pp_corpus_tuplified)
    lst
;;

(* print the corpus *)
let () = pretty_print_corpus (create_corpus (add_end_of_word_tokens example_string))

(* let tuplify_corpus_vals (corpus_vals : string list list) = *)
(*   let cv_flattened = List.flatten corpus_vals in *)
(*   let rec tuplify_corpus_vals_impl corpus_vals acc n = *)
(*     if n = List.length cv_flattened - 1 *)
(*     then List.rev acc *)
(*     else ( *)
(*       let token_tuple = List.nth cv_flattened n, List.nth cv_flattened (n + 1) in *)
(*       tuplify_corpus_vals_impl corpus_vals (token_tuple :: acc) (n + 1)) *)
(*   in *)
(*   tuplify_corpus_vals_impl corpus_vals [] 0 *)
(* ;; *)

let tuplify_corpus_vals (corpus : (int * string list) list) =
  let rec tuplify_corpus_vals_impl (corpus : (int * string list) list) acc n =
    if n = List.length corpus
    then List.rev acc
    else (
      let current_corpus_val = List.nth corpus n in
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
      tuplify_corpus_vals_impl corpus (current_cv_tuplified :: acc) (n + 1))
  in
  tuplify_corpus_vals_impl corpus [] 0
;;

(* printing tuplified corpus vals *)
let () =
  let corpus_vals = add_end_of_word_tokens example_string |> create_corpus in
  pretty_print_pair_list (tuplify_corpus_vals corpus_vals)
;;

(* changing tuples from (char * char) to (string * string) here btw *)
let get_token_max (tuplified_corpus : (int * (string * string) list) list) =
  let rec get_token_max_impl
            (tuplified_corpus : (int * (string * string) list) list)
            n
            token_max
            token_max_count
    =
    if n = List.length tuplified_corpus - 2
    then token_max
    else (
      let current_token = List.nth tuplified_corpus n in
      let current_token_vals = snd current_token in
      let current_token_tuple = List.nth current_token_vals n in
      let current_token_freq = fst current_token in
      let token_count =
        List.length (List.filter (fun t -> t = current_token_tuple) current_token_vals)
        * current_token_freq
      in
      Printf.printf "%d\n" token_count;
      if token_count > token_max_count
      then get_token_max_impl tuplified_corpus (n + 1) current_token_tuple token_count
      else get_token_max_impl tuplified_corpus (n + 1) token_max token_max_count)
  in
  get_token_max_impl tuplified_corpus 0 (Char.escaped ' ', Char.escaped ' ') 0
;;

(* printing most freq token tuple *)
let () =
  let token_max =
    add_end_of_word_tokens example_string
    |> create_corpus
    |> tuplify_corpus_vals
    |> get_token_max
  in
  let print_tuple_func fmt (x, y) =
    Format.fprintf fmt "(most common token -> %S, %S)\n" x y
  in
  print_tuple_func Format.std_formatter token_max
;;

let corpus_learner (corpus : (int * string list) list) (token_max : string * string) =
  let corpus_vals = snd (List.split corpus) in
  let corpus_freqs = fst (List.split corpus) in
  let fst_token_max_val = fst token_max in
  let snd_token_max_val = snd token_max in
  let fst_token_max_index v = List.find_index (fun t -> t = fst_token_max_val) v in
  let snd_token_max_index v = List.find_index (fun t -> t = snd_token_max_val) v in
  let check_both_token_max v =
    Option.get (snd_token_max_index v) = Option.get (fst_token_max_index v) + 1
  in
  let corpus_vals_w_token_max =
    List.filter
      (fun v -> fst_token_max_index v <> None && check_both_token_max v)
      corpus_vals
  in
  let token_max_replacement = fst_token_max_val ^ snd_token_max_val in
  let corpus_vals_replacement =
    List.fold_left
      (fun acc l ->
         List.map
           (fun s ->
              if s = List.nth l (Option.get (fst_token_max_index l))
              then token_max_replacement
              else if s = List.nth l (Option.get (snd_token_max_index l))
              then
                if Option.get (snd_token_max_index l) = List.length l - 1
                then "x?x"
                else "???"
              else s)
           l
         :: acc)
      []
      corpus_vals_w_token_max
  in
  let corpus_replacement =
    List.fold_left
      (fun acc cv ->
         if List.mem cv corpus_vals_w_token_max
         then List.nth corpus_vals_replacement 0 :: acc
         else cv :: acc)
      []
      corpus_vals
  in
  List.combine corpus_freqs corpus_replacement
;;

let () =
  let token_max =
    add_end_of_word_tokens example_string
    |> create_corpus
    |> tuplify_corpus_vals
    |> get_token_max
  in
  let corpus = add_end_of_word_tokens example_string |> create_corpus in
  let cp_learner = corpus_learner corpus token_max in
  pretty_print_corpus cp_learner
;;
