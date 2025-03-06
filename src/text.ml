
(* Raw images of characters. *)
let ascii_imgs = Items.ascii
let extended_characters_imgs = Items.extended_characters

(* The unknown character replacement “�”. *)
let unknown = List.hd extended_characters_imgs

(* A raw table of ASCII characters as images. *)
let ascii_table =
  let a = Array.make 255 unknown in
  let start = Char.code ' ' in
  let finish = Char.code '~' in
  assert (List.length ascii_imgs = finish - start + 1) ;
  for i = start to finish do
    a.(i) <- List.nth ascii_imgs (i - start)
  done ;
  a

(* Remove the first character of a string. *)
let tail str = String.sub str 1 (String.length str - 1)

(* A type to help find substrings that will be stored in an external map.
  In each case, we store as a boolean whether the current substring can be accepted. *)
type substring_search =
  | NoSubstring of bool (* No data is associated with the current substring. *)
  | SubstringData of substring_search array * bool (* For each next character, we provide the data. *)

(* Set the current accepted-boolean to true. *)
let add_accepted = function
  | NoSubstring _ -> NoSubstring true
  | SubstringData (data, _) -> SubstringData (data, true)

(* Read the current accepted-boolean. *)
let is_accepted = function
  | NoSubstring accepted -> accepted
  | SubstringData (_data, accepted) -> accepted

(* Add as a data a string. *)
let rec add_substring data str =
  if String.length str = 0 then add_accepted data
  else (
    let (data, accepted) =
      match data with
      | NoSubstring accepted -> (Array.make 255 (NoSubstring false), accepted)
      | SubstringData (data, accepted) -> (data, accepted) in
    let c = Char.code str.[0] in
    data.(c) <- add_substring data.(c) (tail str) ;
    SubstringData (data, accepted)
  )

(* Given a string, returns the length of the maximum substring for which a data is associated,
  if any. *)
let rec search_string data str =
  if String.length str = 0 then
    if is_accepted data then Some 0 else None
  else
    match data with
    | NoSubstring accepted -> if accepted then Some 0 else None
    | SubstringData (data, accepted) ->
      match search_string data.(Char.code str.[0]) (tail str) with
      | None -> (* No further string is accepted. *)
        if accepted then Some 0 else None
      | Some i -> Some (1 + i)

module StringMap = Map.Make (String)

(* The kind of character. *)
type kind =
  | Vowel
  | Consonant
  | Punctuation
  | Number
  | OtherKind

(* Table of kinds for ASCII characters. *)
let kind_table =
  let a = Array.make 255 OtherKind in
  for c = Char.code 'a' to Char.code 'z' do
    a.(c) <- Consonant ;
    a.(c - Char.code 'a' + Char.code 'A') <- Consonant
  done ;
  for c = Char.code '0' to Char.code '9' do
    a.(c) <- Number
  done ;
  List.iter (fun c -> a.(Char.code c) <- Vowel) [
    'a'; 'e'; 'i'; 'o'; 'u'; 'y';
    'A'; 'E'; 'I'; 'O'; 'U'; 'Y'
  ] ;
  List.iter (fun c -> a.(Char.code c) <- Punctuation) [
    '!'; '"'; '\''; '('; ')'; ','; '.'; ':'; ';'; '?'; '`'
  ] ;
  a

(* The data of all images, including ligatures, and so on. *)
let characters =
  let get i =
    assert (i < List.length extended_characters_imgs) ;
    List.nth extended_characters_imgs i in
  let get_ascii c = ascii_table.(Char.code c) in
  List.fold_left (fun (substrings, m) (str, img, kind) ->
      (add_substring substrings str, StringMap.add str (img, kind) m))
    (NoSubstring false, StringMap.empty) [
    (" " (* Non-breaking space *), get_ascii ' ', Punctuation) ;
    ("¡", get 1, Punctuation) ;
    (* TODO *)
  ]

(* Split a string into a list of lexemes, each being either a raw character or a valid substring. *)
let split_characters : string -> (char, string) Either.t list =
  let rec aux acc str =
    if String.length str = 0 then List.rev acc
    else
      match search_string (fst characters) str with
      | None -> (* We then default to reading ascii characters. *)
        aux (Either.Left str.[0] :: acc) (tail str)
      | Some n ->
        aux (Either.Right (String.sub str 0 n) :: acc) (String.sub str n (String.length str - n)) in
  aux []

module StringPairMap =
  Map.Make (struct
    type t = string * string
    let compare = compare
  end)

let kernings = StringPairMap.empty (* TODO *)

(* Possible break-line behaviours. *)
type breakline =
  | NoBreak (* No line-break can be inserted here. *)
  | BreakRemove (* This character can be replaced by a line-break (typically a space). *)
  | BreakHyphen (* A line-break can be inserted after this character by adding an hyphenation. *)
  | BreakSimple (* A line-break can be added after this character without any addition. *)

(* Divide a text into a list of images, kerning with the next character, and how we can break lines
  here. *)
let rec parse str : (Subimage.t * int * breakline) list =
  let get_image = function
    | Either.Left c -> ascii_table.(Char.code c)
    | Either.Right str ->
      match StringMap.find_opt str (snd characters) with
      | None -> assert false
      | Some (img, _kind) -> img in
  let get_kind = function
    | Either.Left c -> kind_table.(Char.code c)
    | Either.Right str ->
      match StringMap.find_opt str (snd characters) with
      | None -> assert false
      | Some (_img, kind) -> kind in
  let get_kerning d1 d2 =
    let to_str = function
      | Either.Left c -> String.make 1 c
      | Either.Right str -> str in
    if d1 = Either.Left ' ' || d2 = Either.Left ' ' then 0
    else
      match StringPairMap.find_opt (to_str d1, to_str d2) kernings with
      | None -> 1 (* Default kerning *)
      | Some n -> n in
  let rec aux acc = function
    | [] -> List.rev acc
    | (Either.Left ' ' as d) :: l -> aux ((get_image d, 0, BreakRemove) :: acc) l
    | (Either.Left '-' as d) :: l -> aux ((get_image d, 1, BreakSimple) :: acc) l
    | d1 :: d2 :: l when get_kind d1 = Consonant && d1 = d2 ->
      aux ((get_image d1, get_kerning d1 d2, BreakHyphen) :: acc) l
    | d1 :: d2 :: l when get_kind d1 = Vowel && get_kind d2 = Consonant ->
      (* This rule is not correct, but should be good enough in this context. *)
      aux ((get_image d1, get_kerning d1 d2, BreakHyphen) :: acc) l
    | d1 :: d2 :: l -> aux ((get_image d1, get_kerning d1 d2, NoBreak) :: acc) l
    | [d] -> aux ((get_image d, 0, NoBreak) :: acc) [] in
  aux [] (split_characters str)

let render str max_width =
  let l = parse str in
  (* TODO: Deal with line-breaks. *)
  let rec aux pr acc = function
    | [] -> List.rev acc
    | (img, kerning, _line_break) :: l -> aux kerning ((pr, img) :: acc) l in
  Subimage.combine_horizontally (aux 0 [] l)

