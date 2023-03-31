let csv_301 = "301.csv"
let tehaionly = false
let bgtrans = false
let nologo = true
let twiimg = false
let referer = "https://tenhou.net/"

let user_agent =
  "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like \
   Gecko) Chrome/111.0.0.0 Safari/537.36 Edg/111.0.1661.54"

let clean (t : string) : string =
  let open Re in
  t |> replace_string (Perl.compile_pat {|(\^|_|\|)|}) ~by:""

let expand (t : string) : string =
  let open Re in
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
  let open Re in
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

let compile (row : string list) : string =
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

open Cohttp
open Cohttp_lwt
open Cohttp_lwt_unix

let headers =
  Header.of_list [ ("referer", referer); ("user-agent", user_agent) ]

let download_png (url : string) (image_name : string) : unit Lwt.t =
  let uri = Uri.of_string url in
  let dir_name = Filename.chop_extension csv_301 in
  let _ =
    match Sys.is_directory dir_name with
    | true -> ()
    | false ->
        Sys.rename dir_name (dir_name ^ ".old");
        Sys.mkdir dir_name 0755
    | exception Sys_error _ -> Sys.mkdir dir_name 0755
  in
  let image_path = Filename.concat dir_name image_name in
  if Sys.file_exists image_path then Lwt.return ()
  else
    let open Lwt_io in
    let%lwt resp, body = Client.get ~headers uri in
    if Response.status resp = `OK then
      let stream = Body.to_stream body in
      Lwt.join
        [
          with_file ~mode:output ~perm:0644 image_path (fun channel ->
              Lwt_stream.iter_s (write channel) stream);
          printlf "Image successfully Downloaded: %s" image_name;
        ]
    else printl "Image Couldn't be retrieved."

let download_csv (csv_file : string) : unit Lwt.t =
  let%lwt rows = Csv_lwt.Rows.load ~has_header:true csv_file in
  let f i row =
    let image_name = (i |> succ |> string_of_int |> zfill 3) ^ ".png" in
    download_png (compile (Csv_lwt.Row.to_list row)) image_name
  in
  Lwt_list.iteri_s f rows

let () = Lwt_main.run (Lwt.join [ Lwt_io.printl ""; download_csv csv_301 ])
