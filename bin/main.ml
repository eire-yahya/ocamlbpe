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
let corpus_vals = add_end_of_word_tokens example_string |> Corpus.create
let tuplified_corpus_vals = Tuplified_corpus.tuplify corpus_vals
let () = Tuplified_corpus.pretty_print tuplified_corpus_vals
(* let () = *)
(* Printf.printf *)
(* "tuplified_corpus_vals length: %d\n" *)
(* (Tuplified_corpus.length tuplified_corpus_vals) *)
(* ;; *)

let pp_token_max fmt (x, y) = Format.fprintf fmt "most common token -> (%s, %s)\n" x y

(* changing tuples from (char * char) to (string * string) here btw *)
let get_token_max tuplified_corpus =
  let rec get_token_max_impl tuplified_corpus n ~token_max ~token_max_count =
    (* Printf.printf "n is %d\n" n; *)
    if n = Tuplified_corpus.length tuplified_corpus - 1
    then token_max
    else (
      let tuplified_corpus_freqs = fst (Tuplified_corpus.split tuplified_corpus) in
      let tuplified_corpus_vals = snd (Tuplified_corpus.split tuplified_corpus) in
      let tuplified_corpus_unravelled =
        List.combine tuplified_corpus_freqs tuplified_corpus_vals
      in
      let all_tuplified_tokens = List.flatten tuplified_corpus_vals in
      let current_tuplified_token = List.nth all_tuplified_tokens n in
      let current_token_count =
        List.fold_left
          (fun acc corpus ->
             if List.mem current_tuplified_token (snd corpus)
             then
               (fst corpus
                * List.length
                    (List.filter (fun t -> t = current_tuplified_token) (snd corpus)))
               + acc
             else acc)
          0
          tuplified_corpus_unravelled
      in
      (* Printf.printf "current_token_count: %d\n" current_token_count; *)
      if current_token_count > token_max_count
      then
        get_token_max_impl
          tuplified_corpus
          (n + 1)
          ~token_max:current_tuplified_token
          ~token_max_count:current_token_count
      else get_token_max_impl tuplified_corpus (n + 1) ~token_max ~token_max_count)
  in
  get_token_max_impl
    tuplified_corpus
    0
    ~token_max:(Char.escaped ' ', Char.escaped ' ')
    ~token_max_count:0
;;

(* printing most freq token tuple *)
let () =
  let token_max =
    add_end_of_word_tokens example_string
    |> Corpus.create
    |> Tuplified_corpus.tuplify
    |> get_token_max
  in
  pp_token_max Format.std_formatter token_max
;;

let () = Corpus.pretty_print (Corpus.create (add_end_of_word_tokens example_string))
(* let pp_string ppf s = Format.fprintf ppf "%s" s *)

(* let pp_list list = *)
(*   Format.pp_print_list ~pp_sep:(fun ppf () -> Format.fprintf ppf ", ") pp_string list *)
(* ;; *)

(* let pp_option = function *)
(*   | None -> Printf.printf "option: None\n" *)
(*   | Some x -> Printf.printf "option: %d\n" x *)
(* ;; *)

let corpus_learner (corpus : Corpus.t) (token_max : string * string) =
  let corpus_vals = snd (Corpus.split corpus) in
  let corpus_freqs = fst (Corpus.split corpus) in
  let fst_token_max_val = fst token_max in
  let snd_token_max_val = snd token_max in
  let search_fst_token_max_index cv =
    let rec search_fst_token_max_index_impl cv (original_cv : string list) acc =
      let first_fst_token_max_index =
        List.find_index (fun t -> t = fst_token_max_val) cv
      in
      if cv = [] || first_fst_token_max_index = None
      then None
      else (
        let first_fst_token_max_index_plus_acc =
          match first_fst_token_max_index with
          | None -> None
          | Some x -> Some (x + acc)
        in
        let first_fst_token_max_index_plus_acc_val =
          Option.get first_fst_token_max_index_plus_acc
        in
        let new_cv = List.drop (first_fst_token_max_index_plus_acc_val + 1) cv in
        let tokens_dropped = List.length cv - List.length new_cv in
        if
          List.nth original_cv (first_fst_token_max_index_plus_acc_val + 1)
          = snd_token_max_val
        then first_fst_token_max_index_plus_acc
        else search_fst_token_max_index_impl new_cv cv tokens_dropped)
    in
    search_fst_token_max_index_impl cv cv 0
  in
  let search_fst_token_max_index_val cv = Option.get (search_fst_token_max_index cv) in
  let search_snd_token_max_index cv =
    List.find_index (fun t -> t = List.nth cv (search_fst_token_max_index_val cv + 1)) cv
  in
  let corpus_vals_w_token_max =
    List.filter
      (fun cv ->
         search_fst_token_max_index cv <> None && search_snd_token_max_index cv <> None)
      corpus_vals
  in
  let token_max_replacement = fst_token_max_val ^ snd_token_max_val in
  let replace_token_max_in_cv cv =
    let insert_token_max =
      List.mapi
        (fun i t ->
           if i = search_fst_token_max_index_val cv then token_max_replacement else t)
        cv
    in
    let old_snd_token_max_val_index =
      match List.find_index (fun t -> t = snd_token_max_val) insert_token_max with
      | None -> 80
      | Some i -> i
    in
    List.filteri (fun i _ -> i != old_snd_token_max_val_index) insert_token_max
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
  Corpus.combine corpus_freqs (List.rev replace_tokens)
;;

let () = Printf.printf "Replaced_corpus:\n"

let generate_corpus_stage i =
  let init_corpus = add_end_of_word_tokens example_string |> Corpus.create in
  let init_token_max =
    add_end_of_word_tokens example_string
    |> Corpus.create
    |> Tuplified_corpus.tuplify
    |> get_token_max
  in
  let init_corpus_learned = corpus_learner init_corpus init_token_max in
  let rec generate_corpus_stage_impl corpus counter =
    if i = 0 || i = 1
    then init_corpus_learned
    else if i = counter
    then corpus
    else (
      let current_token_max = corpus |> Tuplified_corpus.tuplify |> get_token_max in
      Format.printf "%a" pp_token_max current_token_max;
      let corpus_learned = corpus_learner corpus current_token_max in
      generate_corpus_stage_impl corpus_learned (counter + 1))
  in
  generate_corpus_stage_impl init_corpus 0
;;

let () =
  let generated_corpus = generate_corpus_stage 13 in
  Corpus.pretty_print generated_corpus
;;
