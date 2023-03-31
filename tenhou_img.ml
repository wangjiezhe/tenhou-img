open Cohttp
open Cohttp_lwt
open Cohttp_lwt_unix
open Const
open Mpsz

let headers =
  Header.of_list [ ("referer", referer); ("user-agent", user_agent) ]

let download_png (url : string) (dir_name : string) (image_name : string) :
    unit Lwt.t =
  let uri = Uri.of_string url in
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
  let open Csv_lwt in
  let%lwt rows = Rows.load ~has_header:true csv_file in
  let dir_name = Filename.chop_extension csv_file in
  let _ =
    match Sys.is_directory dir_name with
    | true -> ()
    | false ->
        Sys.rename dir_name (dir_name ^ ".old");
        Sys.mkdir dir_name 0755
    | exception Sys_error _ -> Sys.mkdir dir_name 0755
  in
  let f i row =
    let image_name = (i |> succ |> string_of_int |> zfill 3) ^ ".png" in
    download_png (generate_url (Row.to_list row)) dir_name image_name
  in
  Lwt_list.iteri_s f rows

let () = Lwt_main.run (Lwt.join [ Lwt_io.printl ""; download_csv csv_301 ])
