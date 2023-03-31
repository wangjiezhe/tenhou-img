open Re
open Const

let clean (t : string) : string =
  t |> replace_string (Perl.compile_pat {|(\^|_|\|)|}) ~by:""

let expand (t : string) : string =
  let f sub =
    List.map (Group.get sub) [ 1; 9; 2; 9; 3; 9; 4; 9; 5; 9; 6; 9; 7; 9; 8; 9 ]
    |> String.concat ""
  in
  t
  |> replace
       (Perl.compile_pat
          {|(\d)(\d{0,8})(\d{0,8})(\d{0,8})(\d{0,8})(\d{0,8})(\d{0,8})(\d{8})(m|p|s|z)|})
       ~f
  |> replace
       (Perl.compile_pat {|(\d?)(\d?)(\d?)(\d?)(\d?)(\d?)(\d)(\d)(m|p|s|z)|})
       ~f
  |> replace (Perl.compile_pat {|(m|p|s|z)(m|p|s|z)+|}) ~f:(fun sub ->
         Group.get sub 1)
  |> replace_string (Perl.compile_pat {|^[^\d]|}) ~by:""

let extract34 (t : string) : string =
  t
  |> replace_string (str "0m" |> compile) ~by:"51"
  |> replace_string (str "0p" |> compile) ~by:"52"
  |> replace_string (str "0s" |> compile) ~by:"53"
  |> replace (Perl.compile_pat {|(\d)m|}) ~f:(fun sub -> "1" ^ Group.get sub 1)
  |> replace (Perl.compile_pat {|(\d)p|}) ~f:(fun sub -> "2" ^ Group.get sub 1)
  |> replace (Perl.compile_pat {|(\d)s|}) ~f:(fun sub -> "3" ^ Group.get sub 1)
  |> replace (Perl.compile_pat {|(\d)z|}) ~f:(fun sub -> "4" ^ Group.get sub 1)

let encode (t : string) : string = t |> clean |> expand |> extract34

let zfill (d : int) (s : string) =
  let l = String.length s in
  if l < d then String.make (d - l) '0' ^ s else s

let generate_url (row : string list) : string =
  assert (List.length row = 5);
  let[@warning "-partial-match"] [ kyoku; rot; step; dora; tehai ] = row in
  let tehai = encode tehai in
  if String.length tehai <> 14 * 2 then failwith "INVALID TEHAI LENGTH"
  else
    let q =
      tehai
      ^
      if not tehaionly then
        encode dora
        ^ (kyoku |> int_of_string |> pred |> string_of_int |> zfill 2)
        ^ (step |> zfill 2)
        ^ (rot |> int_of_string |> pred |> string_of_int)
      else ""
    in
    "https://mjv.jp/2/img/n" ^ q ^ ".png?twiimg=" ^ string_of_bool twiimg
    ^ "&bgtrans=" ^ string_of_bool bgtrans ^ "&nologo=" ^ string_of_bool nologo
