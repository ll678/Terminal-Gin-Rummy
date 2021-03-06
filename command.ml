type object_phrase = string list

type command = 
  | Draw of object_phrase
  | Discard of object_phrase
  | Knock
  | Match
  | Pass
  | Sort
  | Hint
  | Score
  | Help
  | Quit

exception Empty

exception Malformed

let rec remove_emptys_lower str_list = 
  match str_list with
  | [] -> []
  | x :: xs ->
    if x = "" then remove_emptys_lower xs
    else (String.lowercase_ascii x) :: remove_emptys_lower xs

(** [gen_obj_phrase str_list] is the [command] interpreted from [str_list].
    The first element determines the command type, and the rest become the
    object phrase.
    Examples:
    - [gen_obj_phrase ["draw"; "stock"]] is [Draw ["stock"]]
    - [gen_obj_phrase ["quit"]] is [Quit].

    Raises: [Empty] if [str_list] is an empty list

    Raises: [Malformed] if the command is malformed. A command
    is {i malformed} if the verb is not defined,
    or if the verb does not expect inputs and there is a non-empty object
    phrase,
    or if the verb is expects inputs and there is an empty object phrase. *)
let gen_obj_phrase str_list = 
  match str_list with
  | [] -> raise Empty
  | "quit" :: [] -> Quit
  | "draw" :: x :: xs -> Draw (x :: xs)
  | "discard" :: x :: xs -> Discard (x :: xs)
  | "knock" :: [] | "gin" :: [] -> Knock
  | "match" :: [] -> Match
  | "pass" :: [] -> Pass
  | "sort" :: [] -> Sort
  | "score" :: [] -> Score
  | "help" :: [] -> Help
  | "hint" :: [] -> Hint
  | _ -> raise Malformed

let parse str =
  str |> String.split_on_char ' ' |> remove_emptys_lower |> gen_obj_phrase

