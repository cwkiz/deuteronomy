open Lwt
module type S = sig
  val definition : Agent.tool
  val run : string -> (string, string) result Lwt.t
end

type implementation = (module S)

let register (implementation : (module S)) : implementation = implementation

let definition (module Tool : S) = Tool.definition

