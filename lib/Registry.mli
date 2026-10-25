type t

type error =
  | Duplicate_agent of string
  | Duplicate_tool of string
  | Unknown_tool of { agent_id : string; tool_id : Tool.id }
  | Tool_not_found of Tool.id
  | Tool_in_use of { tool_id : Tool.id; agent_id : string }

type invocation_error =
  | Agent_not_found of string
  | Tool_not_allowed of { agent_id : string; tool_id : Tool.id }
  | Tool_not_registered of Tool.id
  | Invalid_arguments of string
  | Tool_failed of string

val empty : t

val add_tool : Tool.implementation -> t -> (t, error) result

val remove_tool : Tool.id -> t -> (t, error) result

val add_agent : Agent.agent -> t -> (t, error) result

val find_tool : Tool.id -> t -> Tool.definition option
val find_tool_implementation : Tool.id -> t -> Tool.implementation option
val find_agent : string -> t -> Agent.agent option
val tools_for_agent : Agent.agent -> t -> (Tool.spec list, error) result

val invoke :
  t ->
  agent_id:string ->
  tool_id:Tool.id ->
  arguments:Yojson.Safe.t ->
  (string, invocation_error) result Lwt.t
