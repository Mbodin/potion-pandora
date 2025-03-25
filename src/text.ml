
(* Raw images of characters. *)
let ascii_imgs = Items.ascii
let extended_characters_imgs = Items.extended_characters

(* The unknown character replacement “�”. *)
let unknown_img = List.hd extended_characters_imgs

(* We assume that all character have the same height (possibly with transparent pixels). *)
let height = snd (Subimage.dimensions unknown_img)

(* An empty image, the size of a character. *)
let empty_img =
  Filter.rectangle Filter.transparent (0, height)

(* Remove the first character of a string. *)
let tail str = String.sub str 1 (String.length str - 1)

(* A type to help find substrings that will be stored in an external map.
  In each case, we store the image of the associated substring if it is associated with one. *)
type 'a substring_search =
  | NoSubstring of 'a option (* This string is an end-of-data: no larger substrings will be accepted. *)
  | SubstringData of 'a substring_search array * 'a option (* For each next character, we provide the data. *)

(* Raised when a string has been added twice, which is probably a mistake (the same string would then
  be associated to several renderings). *)
exception StringAddedTwice

(* Associate an image with the current substring. *)
let add_accepted img = function
  | NoSubstring None -> NoSubstring (Some img)
  | SubstringData (data, None) -> SubstringData (data, Some img)
  | NoSubstring (Some _) | SubstringData (_, Some _) -> raise StringAddedTwice

(* Read the current accepted-image. *)
let get_accepted = function
  | NoSubstring accepted -> accepted
  | SubstringData (_data, accepted) -> accepted

(* Add a string/image correspondance to the data. *)
let add_substring data str img =
  let rec aux data str =
    if String.length str = 0 then add_accepted img data
    else (
      let (data, accepted) =
        match data with
        | NoSubstring accepted -> (Array.make 255 (NoSubstring None), accepted)
        | SubstringData (data, accepted) -> (data, accepted) in
      let c = Char.code str.[0] in
      data.(c) <- aux data.(c) (tail str) ;
      SubstringData (data, accepted)
    ) in
  try aux data str with
  | StringAddedTwice -> failwith (Printf.sprintf "String added twice: \"%s\"." str)

(* Given a string, returns the length of the maximum substring for which a data is associated
  as well as the associated image, if any. *)
let rec search_string data str =
  if String.length str = 0 then
    Option.map (fun img -> (0, img)) (get_accepted data)
  else
    match data with
    | NoSubstring accepted -> Option.map (fun img -> (0, img)) accepted
    | SubstringData (data, accepted) ->
      match search_string data.(Char.code str.[0]) (tail str) with
      | None -> (* No further string is accepted. *)
        Option.map (fun img -> (0, img)) accepted
      | Some (i, img) -> Some (1 + i, img)

module StringMap = Map.Make (String)

(* The kind of character. *)
type kind =
  | Vowel
  | Consonant
  | OtherLetter
  | Punctuation
  | Number
  | OtherKind
  | Newline

(* LATER: Encode and compress this list. *)
let character_data = [
    ([" "; " " (* Non-breaking space *)], OtherKind) ;
    (["!"], Punctuation) ;
    (["\""], Punctuation) ;
    (["#"], OtherKind) ;
    (["$"], OtherKind) ;
    (["%"], OtherKind) ;
    (["&"], OtherKind) ;
    (["'"; "ʹ"; "ʹ"], Punctuation) ;
    (["("], Punctuation) ;
    ([")"], Punctuation) ;
    (["*"], OtherKind) ;
    (["+"], OtherKind) ;
    ([","], Punctuation) ;
    (["-"], OtherKind) ;
    (["."], Punctuation) ;
    (["/"], OtherKind) ;
    (["0"], Number) ;
    (["1"], Number) ;
    (["2"], Number) ;
    (["3"], Number) ;
    (["4"], Number) ;
    (["5"], Number) ;
    (["6"], Number) ;
    (["7"], Number) ;
    (["8"], Number) ;
    (["9"], Number) ;
    ([":"], Punctuation) ;
    ([";"; ";"], Punctuation) ;
    (["<"], OtherKind) ;
    (["="], OtherKind) ;
    ([">"], OtherKind) ;
    (["?"], Punctuation) ;
    (["@"], OtherKind) ;
    (["A"; "Α"], Vowel) ;
    (["B"; "Β"], Consonant) ;
    (["C"], Consonant) ;
    (["D"], Consonant) ;
    (["E"; "Ε"], Vowel) ;
    (["F"], Consonant) ;
    (["G"], Consonant) ;
    (["H"; "Η" (* Technically this is a greek vowel. *)], Consonant) ;
    (["I"; "Ι"], Vowel) ;
    (["J"], Consonant) ;
    (["K"; "Κ"], Consonant) ;
    (["L"], Consonant) ;
    (["M"; "Μ"], Consonant) ;
    (["N"; "Ν"], Consonant) ;
    (["O"; "Ο"], Vowel) ;
    (["P"; "Ρ"], Consonant) ;
    (["Q"], Consonant) ;
    (["R"], Consonant) ;
    (["S"], Consonant) ;
    (["T"; "Τ"], Consonant) ;
    (["U"], Vowel) ;
    (["V"], Consonant) ;
    (["W"], Consonant) ;
    (["X"; "Χ"], Consonant) ;
    (["Y"; "Υ"], Vowel) ;
    (["Z"; "Ζ"], Consonant) ;
    (["["], OtherKind) ;
    (["\\"], OtherKind) ;
    (["]"], OtherKind) ;
    (["^"], OtherKind) ;
    (["_"], OtherKind) ;
    (["`"], Punctuation) ;
    (["a"], Vowel) ;
    (["b"], Consonant) ;
    (["c"], Consonant) ;
    (["d"], Consonant) ;
    (["e"], Vowel) ;
    (["f"], Consonant) ;
    (["g"], Consonant) ;
    (["h"], Consonant) ;
    (["i"], Vowel) ;
    (["j"], Consonant) ;
    (["k"], Consonant) ;
    (["l"], Consonant) ;
    (["m"], Consonant) ;
    (["n"], Consonant) ;
    (["o"; "ο"], Vowel) ;
    (["p"], Consonant) ;
    (["q"], Consonant) ;
    (["r"], Consonant) ;
    (["s"], Consonant) ;
    (["t"], Consonant) ;
    (["u"], Vowel) ;
    (["v"], Consonant) ;
    (["w"], Consonant) ;
    (["x"], Consonant) ;
    (["y"], Vowel) ;
    (["z"], Consonant) ;
    (["{"], OtherKind) ;
    (["|"], OtherKind) ;
    (["}"], OtherKind) ;
    (["~"], OtherKind) ;
    (["Ã"; "Ã"; "Â"; "Â"; "Ā"; "Ā"; "À"; "À"; "Á"; "Á"; "Ă"; "Ă"; "Ǎ"; "Ǎ"; "Ä"; "Ä"], Vowel) ;
    (["Ĉ"; "Ĉ"; "Ć"; "Ć"; "Č"; "Č"], Consonant) ;
    (["Ẽ"; "Ẽ"; "Ê"; "Ê"; "Ē"; "Ē"; "È"; "È"; "É"; "É"; "Ě"; "Ě"; "Ë"; "Ë"], Vowel) ;
    (["Ĝ"; "Ĝ"; "Ǵ"; "Ǵ"; "Ğ"; "Ğ"; "Ǧ"; "Ǧ"], Consonant) ;
    (["Ĥ"; "Ĥ"], Consonant) ;
    (["Ĩ"; "Ĩ"; "Î"; "Î"; "Ī"; "Ī"; "Ì"; "Ì"; "Í"; "Í"; "Ĭ"; "Ĭ"; "Ï"; "Ï"], Vowel) ;
    (["Ĵ"; "Ĵ"; "J́"], Consonant) ;
    (["Ñ"; "Ñ"; "Ǹ"; "Ǹ"; "Ń"; "Ń"], Consonant) ;
    (["Õ"; "Õ"; "Ô"; "Ô"; "Ō"; "Ō"; "Ò"; "Ò"; "Ó"; "Ó"; "Ŏ"; "Ŏ"; "Ǒ"; "Ǒ"; "Ö"; "Ö"], Vowel) ;
    (["Ŝ"; "Ŝ"; "Ś"; "Ś"; "Š"; "Š"], Consonant) ;
    (["Ũ"; "Ũ"; "Û"; "Û"; "Ū"; "Ū"; "Ù"; "Ù"; "Ú"; "Ú"; "Ŭ"; "Ŭ"; "Ü"; "Ü"], Vowel) ;
    (["Ỹ"; "Ỹ"; "Ŷ"; "Ŷ"; "Ỳ"; "Ỳ"; "Ý"; "Ý"; "Ÿ"; "Ÿ"], Vowel) ;
    (["Ẑ"; "Ẑ"; "Ź"; "Ź"; "Z̆"; "Ž"; "Ž"], Consonant) ;
    (["ã"; "ã"; "â"; "â"; "ā"; "ā"; "à"; "à"; "á"; "á"; "ă"; "ă"; "ǎ"; "ǎ"; "ä"; "ä"], Vowel) ;
    (["ĉ"; "ĉ"; "ć"; "ć"; "č"; "č"], Consonant) ;
    (["ẽ"; "ẽ"; "ê"; "ê"; "ē"; "ē"; "è"; "è"; "é"; "é"; "ě"; "ě"; "ë"; "ë"], Vowel) ;
    (["ĝ"; "ĝ"; "ǵ"; "ǵ"; "ğ"; "ğ"; "ǧ"; "ǧ"], Consonant) ;
    (["ĥ"; "ĥ"], Consonant) ;
    (["ĩ"; "ĩ"; "i̇̃"; "î"; "î"; "ī"; "ī"; "ì"; "ì"; "i̇̀"; "í"; "í"; "i̇́"; "ĭ"; "ĭ"; "ï"; "ï"], Vowel) ;
    (["ĵ"; "ĵ"; "j́"], Consonant) ;
    (["ñ"; "ñ"; "ǹ"; "ǹ"; "ń"; "ń"], Consonant) ;
    (["õ"; "õ"; "ô"; "ô"; "ō"; "ō"; "ò"; "ò"; "ó"; "ó"; "ŏ"; "ŏ"; "ǒ"; "ǒ"; "ö"; "ö"], Vowel) ;
    (["ŝ"; "ŝ"; "ś"; "ś"; "š"; "š"], Consonant) ;
    (["ũ"; "ũ"; "û"; "û"; "ū"; "ū"; "ù"; "ù"; "ú"; "ú"; "ŭ"; "ŭ"; "ü"; "ü"], Vowel) ;
    (["ỹ"; "ỹ"; "ŷ"; "ŷ"; "ỳ"; "ỳ"; "ý"; "ý"; "ÿ"; "ÿ"], Vowel) ;
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
    (["–"; "֊"; "־"; "᠆"; "‑"; "‒"; "−"; "﹣"], Punctuation) ;
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
  (* We remove the special unknown character from the character images. *)
  let extended_characters_imgs = List.tl extended_characters_imgs in
  let l =
    List.map2 (fun img (strl, kind) -> (strl, img, kind))
      (ascii_imgs @ extended_characters_imgs) character_data in
  let characters =
    List.fold_left (fun substrings (strl, img, kind) ->
        List.fold_left (fun substrings str ->
          (add_substring substrings str (img, kind))) substrings strl)
      (NoSubstring None) (
        (["\n"; "\r"; "\r\n"; "\n\r"], empty_img, Newline)
        :: (["​"] (* Zero-width space *), empty_img, OtherKind)
        :: l) in
  match characters with
  | SubstringData (data, None) ->
    (* We add a default unknown character to the base cases. *)
    SubstringData (Array.map (function
      | (NoSubstring None | SubstringData (_, None)) as d ->
        add_accepted (unknown_img, OtherKind) d
      | a -> a) data, None)
  | _ -> assert false

(* Split a string into a list of lexemes, with their string (with one Unicode character),
  image, and kind. *)
let split_characters : string -> (string * Subimage.t * kind) list =
  let rec aux acc str =
    if String.length str = 0 then List.rev acc
    else
      match search_string characters str with
      | None ->
        assert false (* All ASCII characters are associated to an image, so we should at least get one. *)
      | Some (n, (img, kind)) ->
        aux ((String.sub str 0 n, img, kind) :: acc) (String.sub str n (String.length str - n)) in
  aux []


(* The image of an hyphen. *)
let hyphen_img =
  match split_characters "-" with
  | [("-", img, OtherKind)] -> img
  | _ -> assert false

(* The width of an hyphen, in pixels. *)
let hyphen_width =
  fst (Subimage.dimensions hyphen_img)

(* Check whether the first argument is a prefix of the second. *)
let _is_prefix pre str =
  if String.length pre > String.length str then false
  else pre = String.sub str 0 (String.length pre)

(* Check whether the first argument is a suffix of the second. *)
let _is_suffix suf str =
  let len = String.length suf in
  if len > String.length str then false
  else suf = String.sub str (String.length str - len) len

(* Compute some kind of “optimal” kerning between two images.
  Exceptions can be declared with the [set_kerning] function below. *)
let compute_kerning img1 img2 =
  assert (height = snd (Subimage.dimensions img1)) ;
  assert (height = snd (Subimage.dimensions img2)) ;
  let width1 = fst (Subimage.dimensions img1) in
  let width2 = fst (Subimage.dimensions img2) in
  let is_transparent img (x, y) =
    assert (x >= 0 && x < fst (Subimage.dimensions img)) ;
    assert (y >= 0 && y < height) ;
    let (_r, _g, _b, a) = Subimage.read img (x, y) in
    a = 0 in
  let nb_in_column img x =
    assert (x >= 0 && x < fst (Subimage.dimensions img)) ;
    let rec aux acc y =
      if y = height then acc
      else
        let acc = if is_transparent img (x, y) then acc else (acc + 1) in
        aux acc (y + 1) in
    aux 0 0 in
  let depth_right img y =
    assert (y >= 0 && y < height) ;
    let rec aux acc x =
      if x < 0 then acc
      else
        if is_transparent img (x, y) then aux (acc + 1) (x - 1)
        else acc in
    aux 0 (fst (Subimage.dimensions img) - 1) in
  let depth_left img y =
    assert (y >= 0 && y < height) ;
    let rec aux x =
      if x = fst (Subimage.dimensions img) then x
      else
        if is_transparent img (x, y) then aux (x + 1)
        else x in
    aux 0 in
  let empty_column img x = nb_in_column img x = 0 in
  (* First, checking whether the last column of img1 is empty: if so, the kerning is [0]. *)
  if width1 = 0 then 0
  else if width2 = 0 then 1
  else if empty_column img1 (width1 - 1) then 0
  else
    (* Second, checking whether the first column of img2 is empty: if so, we only accept one pixel from img1 to fuse in. *)
    if empty_column img2 0 then (
      if nb_in_column img1 (width1 - 1) <= 1 then (-1) else 0
    ) else (
      (* Otherwise, we compute a possible candidate by only looking at the difference horizontally. *)
      let min_distance =
        List.fold_left (fun acc y ->
          (* The number of pixels we fit in both direction at this y-coordinate. *)
          let dist = depth_right img1 y + depth_left img2 y in
          min acc dist) (width1 + width2) (Items_aux.range 0 (height - 1)) in
      let candidate = 1 - min (min width1 width2) min_distance in
      (* Finally, we check diagonals: we only accept one diagonal-touch between the characters. *)
      let rec aux distance =
        assert (distance <= 1) ;
        if distance = 1 then 1
        else (
          (* We are going to look slightly outside of the bounds of the image, so we extend the
            scope of [is_transparent] accordingly. *)
          let is_transparent img (x, y) =
            if x < 0 || y < 0 || y >= height || x >= fst (Subimage.dimensions img) then true
            else is_transparent img (x, y) in
          (* Check for all relevant pixels of img2.
            (This means that if comparing values with another coordinate, it has to be img1's
            coordinates to be changed, not img2's.) *)
          let for_all_relevant f acc =
            assert (1 - candidate >= 0) ;
            let rec check acc x y =
              assert (y <= height) ;
              let x1 = width1 + distance + x in
              if y = height then acc
              else if x1 >= width1 + 1 || x > width2 then check acc 0 (y + 1)
              else (
                assert (y >= 0 && y < height) ;
                assert (x1 >= 0 && x1 <= width1) (* We are checking around this pixel, so we need to search for more than just the normal pixels. *) ;
                assert (x >= 0 && x < width2) ;
                let (continue, acc) = f acc x1 x y in
                if continue then check acc (x + 1) y
                else acc
              ) in
            check acc 0 0 in
          (* Checking that there are no direct vertically touching pixels. *)
          let check_up_down dy =
            for_all_relevant (fun b x1 x2 y ->
              assert b ;
                if not (is_transparent img1 (x1, y + dy))
                && not (is_transparent img2 (x2, y)) then
                  (false, false)
                else (true, true)
              ) true in
          (* Check that there are at most one touching diagonal. *)
          let diagonals = [(1, 1); (1, -1); (-1, 1); (-1, -1)] in
          let rec check_diag already_seen_one = function
            | [] -> true
            | (dx, dy) :: l ->
              (* Meaning of the accumulator:
                - None: two touching diagonals or more.
                - Some false: no touching diagonals.
                - Some true: exactly one touching diagonal. *)
              match for_all_relevant (function
                | None -> assert false
                | Some already_seen_one -> fun x1 x2 y ->
                  if not (is_transparent img1 (x1 + dx, y + dy))
                  && not (is_transparent img2 (x2, y)) then (
                    if already_seen_one then (false, None)
                    else (true, Some true)
                  ) else (true, Some already_seen_one)) (Some already_seen_one) with
              | None -> false
              | Some already_seen_one -> check_diag already_seen_one l in
          if check_up_down (-1)
          && check_up_down 1
          && check_diag false diagonals then distance
          else aux (distance + 1)
        ) in
      aux candidate
    )
    (* TODO FIXME: Consider alternative character graphs. *)

module StringPairMap =
  Map.Make (struct
    type t = Subimage.t * Subimage.t
    let compare = compare
  end)


let kernings = ref StringPairMap.empty

let set_kerning img1 img2 v =
  kernings := StringPairMap.add (img1, img2) v !kernings

let get_kerning img1 img2 =
  match StringPairMap.find_opt (img1, img2) !kernings with
  | Some v -> v
  | None ->
    let v = compute_kerning img1 img2 in
    set_kerning img1 img2 v ;
    v

let set_kerning_str str1 str2 v =
  let get_img str =
    match search_string characters str with
    | None -> assert false
    | Some (_, (img, _)) -> img in
  set_kerning (get_img str1) (get_img str2) v

let () = (* TODO: Define this as a ligature. *)
  set_kerning_str "t" "t" 0


(* Possible break-line behaviours. *)
type breakline =
  | NoBreak (* No line-break can be inserted here. *)
  | BreakRemove (* This character can be replaced by a line-break (typically a space). *)
  | BreakHyphen (* A line-break can be inserted after this character by adding an hyphenation. *)
  | BreakSimple (* A line-break can be added after this character without any addition. *)

(* Divide a text into a list of images, kerning with the next character, and how we can break lines
  here.  When returning [None], this means that there is a forced line-break here. *)
let parse str : (Subimage.t * int * breakline) option list =
  let rec aux acc = function
    | [] -> List.rev acc
    | (_, _, Newline) :: l -> aux (None :: acc) l
    | (" ", img, _) :: l -> aux (Some (img, 0, BreakRemove) :: acc) l
    | [("-", img, _)] -> aux (Some (img, 1, BreakSimple) :: acc) []
    | ("-", img1, _) :: ((_, img2, _) :: _ as l) ->
      aux (Some (img1, get_kerning img1 img2, BreakSimple) :: acc) l
    | (_, img1, Consonant) :: ((_, img2, Consonant) :: (_, _, (Consonant | Vowel)) :: (_, _, (Consonant | Vowel)) :: _ as l) ->
      aux (Some (img1, get_kerning img1 img2, BreakHyphen) :: acc) l
    | (_, img1, Vowel) :: ((_, img2, Consonant) :: (_, _, (Consonant | Vowel)) :: (_, _, (Consonant | Vowel)) :: _ as l) ->
      (* This rule is not always correct, but should be good enough in this context. *)
      aux (Some (img1, get_kerning img1 img2, BreakHyphen) :: acc) l
    | (_, img1, _) :: ((_, img2, _) :: _ as l) ->
      aux (Some (img1, get_kerning img1 img2, NoBreak) :: acc) l
    | [(_, img, _)] -> aux (Some (img, 0, NoBreak) :: acc) [] in
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
  next_characters : (Subimage.t * int * breakline) option list ;
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

(* Reading the next character, removing it from the next characters.
  If it returns [Right], it also returns a boolean: if it is true,
  then we have just read a new line, otherwise, this is the end of
  the string to be displayed. *)
let read : (Subimage.t * int * breakline, bool) Either.t m =
  fun st ->
    match st.next_characters with
    | [] -> (st, Either.Right false)
    | None :: l -> ({ st with next_characters = l }, Either.Right true)
    | Some data :: l -> ({ st with next_characters = l }, Either.Left data)

(* Same as [read], but doesn't pop the character from the list of next characters. *)
let peek : (Subimage.t * int * breakline, bool) Either.t m =
  fun st ->
    match st.next_characters with
    | [] -> (st, Either.Right false)
    | None :: _ -> (st, Either.Right true)
    | Some data :: _ -> (st, Either.Left data)

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
let _can_break_line : bool m =
  fun st ->
    let can =
      match st.pending with
      | [] -> false
      | (_img, _offset, NoBreak) :: _ -> false
      | (_img, _offset, _) :: _ -> true in
    (st, can)

(* If one can break line given this data, return the list of data that should be inserted
  to the next characters if actually doing the line break here. *)
let break_line_modifications (img, offset, break) : (Subimage.t * int * breakline) list option m =
  match break with
  | NoBreak -> return None
  | BreakRemove -> return (Some [])
  | BreakHyphen ->
    let k = get_kerning img hyphen_img in
    return (Some [(img, k, break) ; (hyphen_img, 1, BreakSimple)])
  | BreakSimple -> return (Some [(img, offset, break)])

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
let _save : unit m =
  fun st -> ({ st with backtrack = Some { st with backtrack = None } }, ())

(* Save the current state in which these characters are being added into the next characters. *)
let save_with l : unit m =
  fun st ->
    ({ st with backtrack = Some { st with
      backtrack = None;
      next_characters = l @ st.next_characters } }, ())

let render str max_width =
  let rec aux prevent_backtrack : unit m =
    let* r = read in
    match r with
    | Either.Right b ->
      new_line %%
      if b then aux true else return ()
    | Either.Left c ->
      let* has_saved =
        let* modifications = break_line_modifications c in
        match modifications with
        | None -> return false
        | Some l ->
          let l = List.map (fun data -> Some data) l in
          save_with (l @ [None]) %%
          return true in
      write c %%
      let* p = get_current_position_if_new_line in
      let* n = peek in
      if p > max_width && not prevent_backtrack && n <> Either.Right true then (
        let* _b = backtrack in
        aux true
      ) else aux (if has_saved then false else prevent_backtrack) in
  let (st, ()) =
    let st = {
      previous_lines = [] ;
      current_committed_line = empty_img ;
      offset_with_pending = 0 ;
      pending = [] ;
      position = 0 ;
      next_characters = parse str ;
      backtrack = None
    } in
    aux false { st with backtrack = Some st } in
  Subimage.vertical_sequence 1 (List.rev st.previous_lines)

