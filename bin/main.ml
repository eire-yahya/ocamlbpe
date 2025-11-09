let example_string =
  "low low low low low lowest lowest newer newer newer newer newer newer wider wider \
   wider new new"
;;

let add_end_of_word_tokens str =
  let words = String.split_on_char ' ' str in
  List.fold_left (fun acc w -> (w ^ "_ ") ^ acc) "" words
;;

let () =
  Printf.printf "string w. eow tokens: %s\n\n" (add_end_of_word_tokens example_string)
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
    "vocab: %a\n\n"
    (Format.pp_print_list ~pp_sep:(fun out () -> Format.fprintf out ", ") pp_char)
    vocab
;;

let () = print_vocab (create_vocab (add_end_of_word_tokens example_string))
let () = Corpus.pretty_print (Corpus.create (add_end_of_word_tokens example_string))

let () =
  let corpus_vals = add_end_of_word_tokens example_string |> Corpus.create in
  Tuplified_corpus.pretty_print (Tuplified_corpus.tuplify corpus_vals)
;;

(* changing tuples from (char * char) to (string * string) here btw *)
let get_token_max tuplified_corpus =
  let rec get_token_max_impl tuplified_corpus n token_max token_max_count =
    if n = Tuplified_corpus.length tuplified_corpus - 2
    then token_max
    else (
      let current_token = Tuplified_corpus.nth tuplified_corpus n in
      let current_token_vals = snd current_token in
      let current_token_tuple = List.nth current_token_vals n in
      let current_token_freq = fst current_token in
      let token_count =
        List.length (List.filter (fun t -> t = current_token_tuple) current_token_vals)
        * current_token_freq
      in
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
    |> Corpus.create
    |> Tuplified_corpus.tuplify
    |> get_token_max
  in
  let print_tuple_func fmt (x, y) =
    Format.fprintf fmt "most common token -> (%s, %s)\n" x y
  in
  print_tuple_func Format.std_formatter token_max
;;

let () = Corpus.pretty_print (Corpus.create (add_end_of_word_tokens example_string))
(* let pp_string ppf s = Format.fprintf ppf "%s" s *)

(* let pp_list list = *)
(*   Format.pp_print_list ~pp_sep:(fun ppf () -> Format.fprintf ppf ", ") pp_string list *)
(* ;; *)

let corpus_learner (corpus : Corpus.t) (token_max : string * string) =
  let corpus_vals = snd (Corpus.split corpus) in
  let corpus_freqs = fst (Corpus.split corpus) in
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
  let replace_token_max_in_cv corpus_val =
    let insert_token_max =
      List.rev
        (List.fold_left
           (fun acc t ->
              if t = fst_token_max_val then token_max_replacement :: acc else t :: acc)
           []
           corpus_val)
    in
    let old_snd_token_max_val_index =
      match List.find_index (fun t -> t = snd_token_max_val) insert_token_max with
      | None -> 909
      | Some i -> i
    in
    let remove_old_snd_token_max_val =
      List.filteri (fun i _ -> i != old_snd_token_max_val_index) insert_token_max
    in
    remove_old_snd_token_max_val
  in
  let replace_tokens =
    List.fold_left
      (fun acc cv ->
         if List.mem cv corpus_vals_w_token_max
         then replace_token_max_in_cv cv :: acc
         else cv :: acc)
      []
      corpus_vals
  in
  Printf.printf "Replaced_corpus:\n";
  Corpus.combine corpus_freqs (List.rev replace_tokens)
;;

let () =
  let token_max =
    add_end_of_word_tokens example_string
    |> Corpus.create
    |> Tuplified_corpus.tuplify
    |> get_token_max
  in
  let corpus = add_end_of_word_tokens example_string |> Corpus.create in
  let cp_learner = corpus_learner corpus token_max in
  Corpus.pretty_print cp_learner
;;
