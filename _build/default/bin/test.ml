let printf = Format.printf
let fprintf = Format.fprintf

let pp_string ppf string =
  fprintf ppf "%S" string

let pp_print_list ~sep pp_item =
  Format.pp_print_list
    ~pp_sep:(fun ppf () -> fprintf ppf sep) pp_item

let example = [
    [];
    ["one"; "two"; "three"];
    [
      "one"; "two"; "three"; "four"; "five";
      "six"; "seven"; "eight"; "nine"; "ten";
    ];
  ]

let pp_list pp_item ppf list =
  fprintf ppf "[%a]"
    (pp_print_list ~sep:", " pp_item) list

let () = printf "%a" (pp_list (pp_list pp_string)) example
