type 'a notifier =
  ?labels:string list
  -> additional:string
  -> exn
  -> string
  -> ('a, string) Lwt_result.t

module Notifier : sig
  module type TextualConf = sig
    val printer : string -> unit Lwt.t
  end

  module type GitlabConf = sig
    val token : string
    val uri_base : string
    val project_name : string
    val project_id : int
  end

  (** Pass a textual representation of the exception to a specified handler.
      This may be used in concert with a logging tool, or simply
      [Lwt_io.printf]. *)
  module Textual : functor (_ : TextualConf) -> sig
    val notify : unit notifier
  end

  (** Notify failures to a GitLab project in the form of issues. *)
  module Gitlab : functor (_ : GitlabConf) -> sig
    (** [notify ?labels ~additional exn trace] notify an unhandled exception to
        GitLab.

        This reporter generates a digest of the exception message and trace. The
        issue will be titled as
        [first 10 characters of digest | textual representation of exception].
        With a max length of 255 Each time the notifier is called, it searches
        GitLab for an exception whose title contains the first ten characters of
        the aforementioned digest. Each time the exception is caught, the
        notifier will comment with the [additional] information and add the
        [labels] passed. *)
    val notify : int notifier
  end
end

(** [handle ~notify f] executes the function [f], catching any exceptions and
    passing those exceptions to [notify.] *)
val handle
  :  notify:(string -> string -> ('a, string) result Lwt.t)
  -> (unit -> unit Lwt.t)
  -> unit Lwt.t
