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

let getenv_nonempty name =
  match Sys.getenv_opt name with
  | Some value when CCString.trim value <> "" -> Some value
  | _ -> None
;;

let test_issue_create () =
  let exn_msg = "Testing exception for Gitlab issue creation" in
  let () = load_dotenv () in
  match
    ( getenv_nonempty "GITLAB_TOKEN"
    , getenv_nonempty "GITLAB_API_BASE"
    , getenv_nonempty "GITLAB_PROJECT_NAME"
    , getenv_nonempty "GITLAB_PROJECT_ID" )
  with
  | Some _, Some _, Some _, Some project_id
    when int_of_string_opt project_id |> CCOption.is_none ->
    Format.eprintf
      "Skipping GitLab integration test: invalid GITLAB_PROJECT_ID=%S\n%!"
      project_id;
    Lwt.return 0
  | Some token, Some uri_base, Some project_name, Some project_id ->
    let module Gitlab_notify =
      Canary.Notifier.Gitlab (struct
        let token = token
        let uri_base = uri_base
        let project_name = project_name
        let project_id = int_of_string project_id
      end)
    in
    Printexc.record_backtrace true;
    let backtrace =
      (* Raise and catch a test exception to populate the backtrace *)
      try raise (Failure exn_msg) with
      | Failure _ -> Printexc.get_backtrace ()
    in
    Gitlab_notify.notify
      ~labels:[ "bug"; "exception" ]
      ~additional:"some testing additionals"
      (Failure exn_msg)
      backtrace
    |> Lwt.map CCResult.get_or_failwith
  | _ ->
    Format.eprintf
      "Skipping GitLab integration test: missing GitLab environment variables\n\
       %!";
    Lwt.return 0
;;

let () =
  Lwt_main.run
    (let%lwt (_ : int) = test_issue_create () in
     Lwt.return_unit)
;;
