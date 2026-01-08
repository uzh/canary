let load_dotenv () =
  let filename = "../.env" in
  if Sys.file_exists filename
  then
    CCIO.with_in filename CCIO.read_lines_l
    |> CCList.filter_map (fun line ->
      let line = CCString.trim line in
      if CCString.is_empty line || CCString.prefix ~pre:"#" line
      then None
      else CCString.Split.left ~by:"=" line)
    |> CCList.iter (fun (key, value) ->
      let key = CCString.trim key in
      let value = CCString.trim value in
      let value =
        match CCString.chop_prefix ~pre:"\"" value with
        | Some v ->
          CCString.chop_suffix ~suf:"\"" v |> CCOption.get_or ~default:v
        | None ->
          (match CCString.chop_prefix ~pre:"'" value with
           | Some v ->
             CCString.chop_suffix ~suf:"'" v |> CCOption.get_or ~default:v
           | None -> value)
      in
      Unix.putenv key value)
;;

let test_issue_create () =
  let exn = "TESTING ERROR" in
  let () = load_dotenv () in
  let module Gitlab_notify =
    Canary.Notifier.Gitlab (struct
      let token = Sys.getenv "GITLAB_TOKEN"
      let uri_base = Sys.getenv "GITLAB_API_BASE"
      let project_name = Sys.getenv "GITLAB_PROJECT_NAME"
      let project_id = int_of_string (Sys.getenv "GITLAB_PROJECT_ID")
    end)
  in
  Gitlab_notify.notify
    ~labels:[ "bug"; "exception" ]
    ~additional:"some testing additionals"
    (Failure exn)
    (Printexc.get_backtrace ())
  |> Lwt.map CCResult.get_or_failwith
;;

let () =
  Lwt_main.run
    (let%lwt (_ : int) = test_issue_create () in
     Lwt.return_unit)
;;
