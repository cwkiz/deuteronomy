open Lwt

type id = string

type definition = {
  id : id;
  name : string;
  description : string;
}

type spec = {
  definition : definition;
  input_schema : Yojson.Safe.t;
}

module type S = sig
  type arguments

  val definition : definition
  val input_schema : Yojson.Safe.t
  val decode_arguments :
    Yojson.Safe.t -> (arguments, string) result
  val run : arguments -> (string, string) result Lwt.t
end

type implementation

type invocation_error =
  | Invalid_arguments of string
  | Tool_failed of string

val register : (module S) -> implementation
val definition : implementation -> definition
val spec : implementation -> spec
val invoke :
  implementation ->
  Yojson.Safe.t ->
  (string, invocation_error) result Lwt.t
