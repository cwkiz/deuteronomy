open Lwt

module type S = sig
  type arguments

  val definition : Agent.tool
  val decode_arguments :
    Yojson.Safe.t -> (arguments, string) result
  val run : arguments -> (string, string) result Lwt.t
end

type implementation

type invocation_error =
  | Invalid_arguments of string
  | Tool_failed of string

val register : (module S) -> implementation
val definition : implementation -> Agent.tool
val invoke :
  implementation ->
  Yojson.Safe.t ->
  (string, invocation_error) result Lwt.t
