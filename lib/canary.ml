open CCFun

let ( let* ) = Lwt_result.bind

type 'a notifier =
  ?search_params:(string * string list) list
  -> ?labels:string list
  -> additional:string
  -> exn
  -> string
  -> ('a, string) Lwt_result.t

let flatten
  :  ('a list, string) result
  -> 'a Ppx_deriving_yojson_runtime.error_or
  -> ('a list, string) result
  =
  fun acc x ->
  match acc, x with
  | Ok acc', Ok x -> Ok (x :: acc')
  | Error _, _ -> acc
  | Ok _, Error x' -> Error x'
;;

module Notifier = struct
  module type Notifier_s = sig
    type notifier_rv

    val notify : notifier_rv notifier
    val connection_test : unit -> (unit, string) Lwt_result.t
  end

  module type TextualConf = sig
    val printer : string -> unit Lwt.t
  end

  module Textual (Conf : TextualConf) :
    Notifier_s with type notifier_rv := unit = struct
    let notify ?search_params:_ ?labels:_ ~additional exn trace =
      let text =
        Format.asprintf
          "Exception: %s\n(%s)\n%s\n"
          (Printexc.to_string exn)
          additional
          trace
      in
      let%lwt () = Conf.printer text in
      Lwt.return_ok ()
    ;;

    let connection_test = Lwt.return_ok
  end

  module type GitlabConf = sig
    val token : string
    val uri_base : string
    val project_name : string
    val project_id : int
  end

  module Gitlab (Conf : GitlabConf) : Notifier_s with type notifier_rv := int =
  struct
    type gitlab_token_api_repr =
      { id : int
      ; name : string
      ; revoked : bool
      ; created_at : string
      ; scopes : string list
      ; user_id : int
      ; last_used_at : string
      ; active : bool
      ; expires_at : string
      }
    [@@deriving yojson { strict = false }]

    type gitlab_issue_api_repr =
      { description : string option
      ; id : int
      ; title : string
      ; iid : int
      ; project_id : int
      ; labels : string list
      }
    [@@deriving yojson { strict = false }]

    let make_api_call
          ~(meth :
             ?ctx:Cohttp_lwt_unix.Net.ctx
             -> ?headers:Cohttp.Header.t
             -> Uri.t
             -> (Cohttp.Response.t * Cohttp_lwt.Body.t) Lwt.t)
          ~resource
          ?(get_params = [])
          ?(headers = [])
          ()
      =
      let headers =
        headers
        @ [ "content-type", "application/x-www-form-urlencoded"
          ; "PRIVATE_TOKEN", Conf.token
          ]
        |> Cohttp.Header.of_list
      in
      let uri =
        Uri.of_string (Conf.uri_base ^ resource)
        |> flip Uri.add_query_params get_params
      in
      let%lwt resp, body = meth ~headers uri in
      match resp.Cohttp.Response.status with
      | x when Cohttp.Code.is_success (Cohttp.Code.code_of_status x) ->
        let%lwt body' = Cohttp_lwt.Body.to_string body in
        Lwt.return_ok body'
      | _ ->
        let%lwt msg = Cohttp_lwt.Body.to_string body in
        Lwt.return_error ("Request failed: " ^ msg)
    ;;

    let make_post_call ~post_params =
      let post_body =
        post_params
        |> CCList.map (fun (key, value) -> key, [ value ])
        |> Uri.encoded_of_query
      in
      let meth =
        Cohttp_lwt_unix.Client.post
          ~chunked:false
          ~body:(Cohttp_lwt.Body.of_string post_body)
      in
      make_api_call ~meth
    ;;

    let make_get_call = make_api_call ~meth:Cohttp_lwt_unix.Client.get

    let get_gitlab_issues ?(search_params = []) ?title ?iids ?description () =
      let get_params =
        let search_in, search =
          match title, description with
          | Some title, None -> Some [ "title" ], Some [ title ]
          | None, Some description ->
            Some [ "description" ], Some [ description ]
          | Some title, Some description ->
            Some [ "title"; "description" ], Some [ title ^ " " ^ description ]
          | None, None -> None, None
        in
        let add_opt key value_opt acc =
          match value_opt with
          | Some value -> (key, value) :: acc
          | None -> acc
        in
        []
        |> add_opt "iids[]" iids
        |> add_opt "search" search
        |> add_opt "search_in" search_in
        |> CCList.rev
      in
      let get_params = get_params @ search_params in
      let* resp = make_get_call ~resource:"/issues" ~get_params () in
      let resp' = Yojson.Safe.from_string resp in
      let* rv =
        match resp' with
        | `List ls ->
          let test =
            CCList.map gitlab_issue_api_repr_of_yojson ls
            |> CCList.fold_left flatten (Ok [])
          in
          test |> Lwt.return
        | _ -> Lwt.return_error "Retrieved value is not a list"
      in
      Lwt.return_ok rv
    ;;

    let create_gitlab_issue ~description title =
      let* resp =
        let resource = Format.asprintf "/projects/%d/issues" Conf.project_id in
        make_post_call
          ~post_params:[ "description", description ]
          ~get_params:[ "title", [ title ] ]
          ~resource
          ()
      in
      let resp' = Yojson.Safe.from_string resp in
      match Yojson.Safe.Util.member "iid" resp' with
      | `Int x -> Lwt.return_ok x
      | _ -> Lwt.return_error "Invalid issue ID"
    ;;

    let add_labels iid labels =
      let resource =
        Format.asprintf "/projects/%d/issues/%d" Conf.project_id iid
      in
      let meth = Cohttp_lwt_unix.Client.put ?body:None ?chunked:None in
      let get_params = [ "add_labels", labels ] in
      if CCList.is_empty labels |> not
      then
        let* (_ : string) = make_api_call ~meth ~resource ~get_params () in
        Lwt.return_ok ()
      else Lwt.return_ok ()
    ;;

    let reopen_issue ?(with_labels = []) iid =
      let resource =
        Format.asprintf "/projects/%d/issues/%d" Conf.project_id iid
      in
      let meth = Cohttp_lwt_unix.Client.put ?body:None ?chunked:None in
      let get_params =
        [ "state_event", [ "reopen" ] ]
        @
        if CCList.is_empty with_labels
        then []
        else [ "add_labels", with_labels ]
      in
      let* _resp = make_api_call ~meth ~resource ~get_params () in
      Lwt.return_ok ()
    ;;

    let comment_on_issue ~iid body =
      let resource =
        Format.asprintf "/projects/%d/issues/%d/notes" Conf.project_id iid
      in
      let* _resp = make_post_call ~post_params:[ "body", body ] ~resource () in
      Lwt.return_ok ()
    ;;

    let notify ?search_params ?labels ~additional exn trace =
      let title_max_length = 255 in
      let exn = Printexc.to_string exn in
      let trace_md5 =
        CCString.sub Digest.(to_hex (string (exn ^ trace))) 0 10
      in
      let description = Format.asprintf "```\n%s\n```" trace in
      let* existing = get_gitlab_issues ?search_params ~title:trace_md5 () in
      let title = trace_md5 ^ " | " ^ exn in
      let title =
        if CCString.length title > title_max_length
        then CCString.sub title 0 title_max_length
        else title
      in
      let* iid =
        match existing with
        | [ issue ] -> Lwt.return_ok issue.iid
        | [] -> create_gitlab_issue ~description title
        | _ ->
          Lwt.return_error
            "Multiple gitlab issues match this exception/description set."
      in
      let* () = comment_on_issue ~iid additional in
      let* () = reopen_issue ?with_labels:labels iid in
      let* () =
        match labels with
        | Some labels -> add_labels iid labels
        | None -> Lwt.return_ok ()
      in
      Lwt.return_ok iid
    ;;

    let connection_test () : (unit, string) Lwt_result.t =
      let* resp = make_get_call ~resource:"/personal_access_tokens/self" () in
      resp
      |> Yojson.Safe.from_string
      |> gitlab_token_api_repr_of_yojson
      |> CCResult.map (fun (_ : gitlab_token_api_repr) -> ())
      |> CCResult.map_err (Format.asprintf "Canary connection: %s")
      |> Lwt_result.lift
    ;;
  end
end

let handle ~notify (f : unit -> 'a Lwt.t) =
  Lwt.catch f (fun exn ->
    match%lwt notify (Printexc.to_string exn) (Printexc.get_backtrace ()) with
    | Ok _ -> Lwt.return ()
    | Error err -> Lwt_io.printlf "Error notifying exception: %s" err)
;;
