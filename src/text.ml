
(* Raw images of characters. *)
let ascii_imgs = Items.ascii
let extended_characters_imgs = Items.extended_characters

(* The height of characters, in pixels. *)
let font_height = 8

(* The width of an hyphen, in pixels. *)
let hyphen_width = 3

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

(* Raised when a string has been added twice, which is probably a mistake (the same string would then
  have several renderings). *)
exception StringAddedTwice

(* Set the current accepted-boolean to true. *)
let add_accepted = function
  | NoSubstring false -> NoSubstring true
  | SubstringData (data, false) -> SubstringData (data, true)
  | NoSubstring true | SubstringData (_, true) -> raise StringAddedTwice

(* Read the current accepted-boolean. *)
let is_accepted = function
  | NoSubstring accepted -> accepted
  | SubstringData (_data, accepted) -> accepted

(* Add as a data a string. *)
let add_substring data str =
  let rec aux data str =
    if String.length str = 0 then add_accepted data
    else (
      let (data, accepted) =
        match data with
        | NoSubstring accepted -> (Array.make 255 (NoSubstring false), accepted)
        | SubstringData (data, accepted) -> (data, accepted) in
      let c = Char.code str.[0] in
      data.(c) <- aux data.(c) (tail str) ;
      SubstringData (data, accepted)
    ) in
  try aux data str with
  | StringAddedTwice -> failwith (Printf.sprintf "String added twice: \"%s\"." str)

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
  | OtherLetter
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

(* LATER: Encode and compress this list. *)
let character_data = [
    ([" "] (* Non-breaking space *), Punctuation) ;
    (["Ã"; "Ã"; "Â"; "Â"; "Ā"; "Ā"; "À"; "À"; "Á"; "Á"; "Ă"; "Ă"; "Ǎ"; "Ǎ"], Vowel) ;
    (["Ĉ"; "Ĉ"; "Ć"; "Ć"; "Č"; "Č"], Consonant) ;
    (["Ẽ"; "Ẽ"; "Ê"; "Ê"; "Ē"; "Ē"; "È"; "È"; "É"; "É"; "Ě"; "Ě"], Vowel) ;
    (["Ĝ"; "Ĝ"; "Ǵ"; "Ǵ"; "Ğ"; "Ğ"; "Ǧ"; "Ǧ"], Consonant) ;
    (["Ĥ"; "Ĥ"], Consonant) ;
    (["Ĩ"; "Ĩ"; "Î"; "Î"; "Ī"; "Ī"; "Ì"; "Ì"; "Í"; "Í"; "Ĭ"; "Ĭ"], Vowel) ;
    (["Ĵ"; "Ĵ"; "J́"], Consonant) ;
    (["Ñ"; "Ñ"; "Ǹ"; "Ǹ"; "Ń"; "Ń"], Consonant) ;
    (["Õ"; "Õ"; "Ô"; "Ô"; "Ō"; "Ō"; "Ò"; "Ò"; "Ó"; "Ó"; "Ŏ"; "Ŏ"; "Ǒ"; "Ǒ"], Vowel) ;
    (["Ŝ"; "Ŝ"; "Ś"; "Ś"; "Š"; "Š"], Consonant) ;
    (["Ũ"; "Ũ"; "Û"; "Û"; "Ū"; "Ū"; "Ù"; "Ù"; "Ú"; "Ú"; "Ŭ"; "Ŭ"], Vowel) ;
    (["Ỹ"; "Ỹ"; "Ŷ"; "Ŷ"; "Ỳ"; "Ỳ"; "Ý"; "Ý"], Vowel) ;
    (["Ẑ"; "Ẑ"; "Ź"; "Ź"; "Z̆"; "Ž"; "Ž"], Consonant) ;
    (["ã"; "ã"; "â"; "â"; "ā"; "ā"; "à"; "à"; "á"; "á"; "ă"; "ă"; "ǎ"; "ǎ"], Vowel) ;
    (["ĉ"; "ĉ"; "ć"; "ć"; "č"; "č"], Consonant) ;
    (["ẽ"; "ẽ"; "ê"; "ê"; "ē"; "ē"; "è"; "è"; "é"; "é"; "ě"; "ě"], Vowel) ;
    (["ĝ"; "ĝ"; "ǵ"; "ǵ"; "ğ"; "ğ"; "ǧ"; "ǧ"], Consonant) ;
    (["ĥ"; "ĥ"], Consonant) ;
    (["ĩ"; "ĩ"; "i̇̃"; "î"; "î"; "ī"; "ī"; "ì"; "ì"; "i̇̀"; "í"; "í"; "i̇́"; "ĭ"; "ĭ"], Vowel) ;
    (["ĵ"; "ĵ"; "j́"], Consonant) ;
    (["ñ"; "ñ"; "ǹ"; "ǹ"; "ń"; "ń"], Consonant) ;
    (["õ"; "õ"; "ô"; "ô"; "ō"; "ō"; "ò"; "ò"; "ó"; "ó"; "ŏ"; "ŏ"; "ǒ"; "ǒ"], Vowel) ;
    (["ŝ"; "ŝ"; "ś"; "ś"; "š"; "š"], Consonant) ;
    (["ũ"; "ũ"; "û"; "û"; "ū"; "ū"; "ù"; "ù"; "ú"; "ú"; "ŭ"; "ŭ"], Vowel) ;
    (["ỹ"; "ỹ"; "ŷ"; "ŷ"; "ỳ"; "ỳ"; "ý"; "ý"], Vowel) ;
    (["ẑ"; "ẑ"; "ź"; "ź"; "z̆"; "ž"; "ž"], Consonant) ;
    (["ẞ"; "ß"], Consonant) ;
    (["⁰"], Number) ;
    (["¹"], Number) ;
    (["²"], Number) ;
    (["³"], Number) ;
    (["⁴"], Number) ;
    (["⁵"], Number) ;
    (["⁶"], Number) ;
    (["⁷"], Number) ;
    (["⁸"], Number) ;
    (["⁹"], Number) ;
    (["¿"], Punctuation) ;
    (["¡"], Punctuation) ;
    (["ª"], Vowel) ;
    (["º"], Vowel) ;
    (["fi"], OtherLetter) ;
    (["fl"], OtherLetter) ;
    (["°"], OtherKind) ;
    (["–"; "֊"; "־"; "᠆"; "-"; "‑"; "‒"; "−"; "﹣"], Punctuation) ;
    (["—"; "﹘"], Punctuation) ;
    (["🄯"; "(ɔ)"], OtherKind) ;
    (["->"; "→"; "🡒"; "⟶"; "➙"; "➛"; "➜"; "➔"; "➝"; "➞"; "➺"; "➻"; "⭢"; "🠂"; "🠆"; "🠊"; "🠢"; "🠦"; "🠪"; "🠒"; "🠖"; "🡢"; "🡪"; "🡲"; "➤"; "⮞"; "➢"; "➣"; "⮚"; "🠺"], OtherKind) ;
    (["<-"; "←"; "🡐"; "⟵"; "⭠"; "🠀"; "🠄"; "🠈"; "🠠"; "🠤"; "🠨"; "🠐"; "🠔"; "🡠"; "🡨"; "🡰"; "⮜"; "⮘"; "🠸"], OtherKind) ;
    (["↑"; "🡑"; "⭡"; "🠁"; "🠅"; "🠉"; "🠡"; "🠥"; "🠩"; "🠑"; "🠕"; "🡡"; "🡩"; "🡱"; "⮝"; "⮙"; "🠹"], OtherKind) ;
    (["↓"; "🡓"; "⭣"; "🠃"; "🠇"; "🠋"; "🠣"; "🠧"; "🠫"; "🠓"; "🠗"; "🡣"; "🡫"; "🡳"; "⮟"; "⮛"; "🠻"], OtherKind) ;
    (["Ą"; "Ą"], Vowel) ;
    (["ą"; "ą"], Vowel) ;
    (["Ç"; "Ç"], Consonant) ;
    (["ç"; "ç"], Consonant) ;
    (["Ę"; "Ę"], Vowel) ;
    (["ę"; "ę"], Vowel) ;
    (["Į"; "Į"], Vowel) ;
    (["į"; "į"], Vowel) ;
    (["Ș"; "Ș"; "Ş"; "Ş"], Consonant) ;
    (["ș"; "ș"; "ş"; "ş"], Consonant) ;
    (["Ț"; "Ț"; "Ţ"; "Ţ"], Consonant) ;
    (["ț"; "ț"; "ţ"; "ţ"], Consonant) ;
    (["Ų"; "Ų"], Vowel) ;
    (["ų"; "ų"], Vowel) ;
    (["Z̦"; "Z̧"], Consonant) ;
    (["z̦"; "z̧"], Consonant) ;
    (["¬"], OtherKind) ;
    (["‘"; "‛"], Punctuation) ;
    (["’"], Punctuation) ;
    (["‚"], Punctuation) ;
    (["«"], Punctuation) ;
    (["»"], Punctuation) ;
    (["“"; "‟"], Punctuation) ;
    (["”"], Punctuation) ;
    (["„"], Punctuation) ;
    (["·"; "·"; "⸱"; "ꞏ"; "・"; "᛫"; "⋅"], Punctuation) ;
    (["⚠︎"; "⚠️"; "⚠"], OtherKind) ;
    (["✴"; "✷"; "✵"], OtherKind) ;
    (["Æ"], Vowel) ;
    (["æ"], Vowel) ;
    (["Œ"], Vowel) ;
    (["œ"], Vowel) ;
    (["Ø"], Vowel) ;
    (["ø"], Vowel) ;
    (["…"], Vowel) ;
    (["Ł"], Consonant) ;
    (["ł"], Consonant) ;
    (["Ė"; "Ė"], Vowel) ;
    (["ė"; "ė"], Vowel) ;
    (["Ċ"; "Ċ"], Consonant) ;
    (["ċ"; "ċ"], Consonant) ;
    (["Ż"; "Ż"], Consonant) ;
    (["ż"; "ż"], Consonant) ;
    (["Ɣ"], Consonant) ;
    (["ɣ"], Consonant) ;
    (["Ɛ"], Vowel) ;
    (["ɛ"], Vowel) ;
    (["Đ"], Consonant) ;
    (["đ"], Consonant) ;
    (["Ḍ"; "Ḍ"], Consonant) ;
    (["ḍ"; "ḍ"], Consonant) ;
    (["Ḥ"; "Ḥ"], Consonant) ;
    (["ḥ"; "ḥ"], Consonant) ;
    (["Ṛ"; "Ṛ"], Consonant) ;
    (["ṛ"; "ṛ"], Consonant) ;
    (["Ṣ"; "Ṣ"], Consonant) ;
    (["ṣ"; "ṣ"], Consonant) ;
    (["Ṭ"; "Ṭ"], Consonant) ;
    (["ṭ"; "ṭ"], Consonant) ;
    (["Ẓ"; "Ẓ"], Consonant) ;
    (["ẓ"; "ẓ"], Consonant) ;
    (["♡"; "♥"; "❤"; "💓"; "💖"; "💗"; "💝"], OtherKind)
  ]

(* The data of all images, including ligatures, and so on. *)
let characters =
  let get_ascii c = ascii_table.(Char.code c) in
  let l =
    List.map2 (fun img (strl, kind) -> (strl, img, kind))
      (get_ascii ' ' :: List.tl extended_characters_imgs) character_data in
  List.fold_left (fun (substrings, m) (strl, img, kind) ->
      List.fold_left (fun (substrings, m) str ->
        (add_substring substrings str, StringMap.add str (img, kind) m)) (substrings, m) strl)
    (NoSubstring false, StringMap.empty) l

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

(* Check whether the first argument is a prefix of the second. *)
let is_prefix pre str =
  if String.length pre > String.length str then false
  else pre = String.sub str 0 (String.length pre)

(* Check whether the first argument is a suffix of the second. *)
let is_suffix suf str =
  let len = String.length suf in
  if len > String.length str then false
  else suf = String.sub str (String.length str - len) len

module StringPairMap =
  Map.Make (struct
    type t = string * string
    let compare = compare
  end)

let kernings =
  List.fold_left (fun m (str1, str2, k) ->
    let firsts =
      List.fold_left (fun l (strl, _kind) ->
        List.filter (is_prefix str2) strl @ l) [str2] character_data in
    let seconds =
      List.fold_left (fun l (strl, _kind) ->
        List.filter (is_suffix str1) strl @ l) [str1] character_data in
    List.fold_left (fun m first ->
      List.fold_left (fun m second ->
        StringPairMap.add (first, second) k m) m firsts) m seconds) StringPairMap.empty [
      ("/", "/", -1) ;
      ("C", "s", 0) ;
      ("F", "a", 0) ; ("F", "c", 0) ; ("F", "d", 0) ; ("F", "e", 0) ; ("F", "g", 0) ;
      ("F", "j", 0) ; ("F", "m", 0) ; ("F", "o", 0) ; ("F", "p", 0) ; ("F", "q", 0) ;
      ("F", "r", 0) ; ("F", "s", 0) ; ("F", "u", 0) ; ("F", "v", 0) ; ("F", "w", 0) ;
      ("F", "x", 0) ; ("F", "y", 0) ; ("F", "z", 0) ;
      ("I", "q", 0) ; ("I", "s", 0) ;
      ("J", "a", 0) ; ("J", "c", 0) ; ("J", "d", 0) ; ("J", "g", 0) ; ("J", "j", 0) ;
      ("J", "m", 0) ; ("J", "n", 0) ; ("J", "o", 0) ; ("J", "p", 0) ; ("J", "q", 0) ;
      ("J", "r", 0) ; ("J", "s", 0) ; ("J", "u", 0) ; ("J", "v", 0) ; ("J", "w", 0) ;
      ("J", "x", 0) ; ("J", "y", 0) ;
      ("L", "'", -1) ; ("L", "g", 0) ; ("L", "q", 0) ; ("L", "s", 0) ; ("L", "v", 0) ;
      ("L", "w", 0) ; ("L", "y", 0) ;
      ("P", "a", 0) ; ("P", "c", 0) ; ("P", "d", 0) ; ("P", "g", 0) ; ("P", "j", 0) ;
      ("P", "m", 0) ; ("P", "n", 0) ; ("P", "o", 0) ; ("P", "p", 0) ; ("P", "q", 0) ;
      ("P", "r", 0) ;
      ("T", "a", 0) ; ("T", "c", 0) ; ("T", "d", 0) ; ("T", "e", 0) ; ("T", "g", 0) ;
      ("T", "j", 0) ; ("T", "m", 0) ; ("T", "n", 0) ; ("T", "o", 0) ; ("T", "p", 0) ;
      ("T", "q", 0) ; ("T", "r", 0) ; ("T", "s", 0) ; ("T", "u", 0) ; ("T", "v", 0) ;
      ("T", "w", 0) ; ("T", "x", 0) ; ("T", "y", 0) ; ("T", "z", 0) ;
      ("V", "a", 0) ; ("V", "c", 0) ; ("V", "d", 0) ; ("V", "g", 0) ; ("V", "j", 0) ;
      ("V", "m", 0) ; ("V", "n", 0) ; ("V", "o", 0) ; ("V", "r", 0) ;
      ("W", "a", 0) ; ("W", "c", 0) ; ("W", "d", 0) ; ("W", "g", 0) ; ("W", "j", 0) ;
      ("W", "m", 0) ; ("W", "n", 0) ; ("W", "o", 0) ; ("W", "r", 0) ;
      ("a", "T", 0) ;
      ("b", "T", -1) ; ("b", "V", 0) ; ("b", "W", 0) ;
      ("c", "T", 0) ;
      ("e", "T", 0) ;
      ("f", "f", 0) ; ("f", "l", 0) ;
      ("g", "T", 0) ; ("h", "T", 0) ; ("h", "V", 0) ; ("h", "W", 0) ;
      ("k", "T", 0) ; ("l", "'", 0) ; ("l", "T", 0) ; ("l", "l", 0) ;
      ("m", "T", 0) ; ("m", "V", 0) ; ("m", "W", 0) ;
      ("n", "T", 0) ; ("n", "V", 0) ; ("n", "W", 0) ;
      ("o", "T", 0) ; ("o", "V", 0) ; ("o", "W", 0) ;
      ("p", "T", 0) ;
      ("q", "T", 0) ;
      ("r", "J", 0) ; ("r", "T", 0) ;
      ("s", "T", 0) ; ("s", "V", 0) ; ("s", "W", 0) ;
      ("t", "J", 0) ; ("t", "t", 0) ;
      ("u", "T", 0) ;
      ("v", "T", 0) ;
      ("w", "T", 0) ;
      ("x", "T", 0) ;
      ("y", "T", 0) ;
      ("z", "T", 0) ;
    ]

(* Possible break-line behaviours. *)
type breakline =
  | NoBreak (* No line-break can be inserted here. *)
  | BreakRemove (* This character can be replaced by a line-break (typically a space). *)
  | BreakHyphen (* A line-break can be inserted after this character by adding an hyphenation. *)
  | BreakSimple (* A line-break can be added after this character without any addition. *)

(* Divide a text into a list of images, kerning with the next character, and how we can break lines
  here. *)
let parse str : (Subimage.t * int * breakline) list =
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
      aux ((get_image d1, get_kerning d1 d2, BreakHyphen) :: acc) (d2 :: l)
    | d1 :: d2 :: l when get_kind d1 = Vowel && get_kind d2 = Consonant ->
      (* This rule is not correct, but should be good enough in this context. *)
      aux ((get_image d1, get_kerning d1 d2, BreakHyphen) :: acc) (d2 :: l)
    | d1 :: d2 :: l -> aux ((get_image d1, get_kerning d1 d2, NoBreak) :: acc) (d2 :: l)
    | [d] -> aux ((get_image d, 0, NoBreak) :: acc) [] in
  aux [] (split_characters str)

(* We define a small monad for the rendering.
  It stores the list of strings that have already been validated (as one line each),
  and the list of characters that are pending.
  It also carries the current position within the line (in pixels), as well as the last-committed
  position.
  Finally, it enables to backtrack to the previous non-committed state. *)
type state = {
  previous_lines : Subimage.t list (* This list is stored in reverse. *) ;
  current_committed_line : Subimage.t ;
  offset_with_pending : int ;
  pending : (Subimage.t * int * breakline) list (* This list is stored in reverse. *) ;
  position : int ;
  next_characters : (Subimage.t * int * breakline) list ;
  backtrack : state option (* Invariant: the pointed state will always have a [backtrack] field set to [None]. *)
}

type 'a m = state -> (state * 'a)

let return (type a) (a : a) : a m = fun st -> (st, a)
let bind (type u v) (o : u m) (k : u -> v m) : v m =
  fun st ->
    let (st, r) = o st in
    k r st

let ( let* ) = bind

let ( %% ) (type t) : unit m -> t m -> t m =
  fun m1 m2 -> bind m1 (fun () -> m2)

(* Reading the next character, removing it from the next characters. *)
let read : (Subimage.t * int * breakline) option m =
  fun st ->
    match st.next_characters with
    | [] -> (st, None)
    | data :: l -> ({ st with next_characters = l }, Some data)

(* Get the current position if we were to stop the line here. *)
let get_current_position_if_new_line : int m =
  fun st ->
    let pos =
        match st.pending with
        | [] -> st.position
        | (img, _offset, break) :: l ->
            match break with
            | NoBreak | BreakSimple -> st.position
            | BreakHyphen -> st.position + hyphen_width
            | BreakRemove ->
              let p = st.position - (fst (Subimage.dimensions img)) in
              match l with
              | [] -> p
              | (_img, offset, _break) :: _ -> p - offset in
    (st, pos)

(* Whether one can break a line right now. *)
let can_break_line : bool m =
  fun st ->
    let can =
      match st.pending with
      | [] -> false
      | (_img, _offset, NoBreak) :: _ -> false
      | (_img, _offset, _) :: _ -> true in
    (st, can)

(* Commit the currently pending characters. *)
let commit : unit m =
  fun st ->
    let (committed_line, offset) =
      let (l, offset) =
        let rec aux pr acc = function
          | [] -> (List.rev acc, pr)
          | (img, offset, _break) :: l ->
            aux offset ((pr, img) :: acc) l in
        aux st.offset_with_pending [] (List.rev st.pending) in
      (Subimage.combine_horizontally
        ((0, st.current_committed_line) :: l), offset) in
    let st =
      { st with
            current_committed_line = committed_line ;
            offset_with_pending = offset ;
            pending = [] ;
            backtrack = None
      } in
    ({ st with backtrack = Some st }, ())

(* Add an element to the pending position. *)
let write (img, offset, break) : unit m =
  fun st ->
    ({ st with
            offset_with_pending = offset ;
            pending = (img, offset, break) :: st.pending ;
            position = st.position + st.offset_with_pending + fst (Subimage.dimensions img)
    }, ())

(* Insert (and commit) a new line. *)
let new_line : unit m =
  commit %%
  fun st ->
    let zero_width_image img =
      Subimage.sub img 0 (snd (Subimage.dimensions img)) (0, 0) in
    ({ st with
            previous_lines = st.current_committed_line :: st.previous_lines ;
            current_committed_line = zero_width_image st.current_committed_line ;
            offset_with_pending = 0 ;
            position = 0
    }, ())

(* Backtrack to a previous position.
  It returns a boolean stating whether it succeeded. *)
let backtrack : bool m =
  fun st ->
    match st.backtrack with
    | Some st -> (st, true)
    | None -> (st, false)

(* Save the current state. *)
let save : unit m =
  fun st -> ({ st with backtrack = Some { st with backtrack = None } }, ())

let render str max_width =
  let rec aux prevent_backtrack : unit m =
    let* r = read in
    match r with
    | None -> new_line
    | Some c ->
      write c %%
      let* p = get_current_position_if_new_line in
      if p > max_width && not prevent_backtrack then (
        let* _b = backtrack in
        let* can = can_break_line in
        (if can then new_line else return ()) %%
        aux true
      ) else (
        let* can = can_break_line in
        if can then (
          save %%
          aux false
        ) else aux prevent_backtrack
      ) in
  let (st, ()) =
    let st = {
      previous_lines = [] ;
      current_committed_line = Filter.rectangle Filter.transparent (0, font_height) ;
      offset_with_pending = 0 ;
      pending = [] ;
      position = 0 ;
      next_characters = parse str ;
      backtrack = None
    } in
    aux false { st with backtrack = Some st } in
  Subimage.vertical_sequence 1 (List.rev st.previous_lines)

