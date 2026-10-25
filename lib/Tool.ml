open Lwt
module type S = sig
  type arguments

  val definition : Agent.tool
  val decode_arguments :
    Yojson.Safe.t -> (arguments, string) result
  val run : arguments -> (string, string) result Lwt.t
end

type implementation = (module S)

type invocation_error =
  | Invalid_arguments of string
  | Tool_failed of string

let register (implementation : (module S)) : implementation = implementation

let definition (module Tool : S) = Tool.definition

let invoke (module Tool : S) json =
  match Tool.decode_arguments json with
  | Error message -> Lwt.return (Error (Invalid_arguments message))
  | Ok arguments ->
      Tool.run arguments
      |> Lwt.map (Result.map_error (fun message -> Tool_failed message))
