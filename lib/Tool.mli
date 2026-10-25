open Lwt

module type S = sig
  val definition : Agent.tool
  val run : string -> (string, string) result Lwt.t
end

type implementation

val register : (module S) -> implementation
val definition : implementation -> Agent.tool
