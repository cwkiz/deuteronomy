type t

type error =
  | Duplicate_agent of string
  | Duplicate_tool of string
  | Unknown_tool of { agent_id : string; tool_id : Agent.tool_id }
  | Tool_not_found of Agent.tool_id
  | Tool_in_use of { tool_id : Agent.tool_id; agent_id : string }

type invocation_error =
  | Agent_not_found of string
  | Tool_not_allowed of { agent_id : string; tool_id : Agent.tool_id }
  | Tool_not_registered of Agent.tool_id
  | Invalid_arguments of string
  | Tool_failed of string

val empty : t

val add_tool : Tool.implementation -> t -> (t, error) result

val remove_tool : Agent.tool_id -> t -> (t, error) result

val add_agent : Agent.agent -> t -> (t, error) result

val find_tool : Agent.tool_id -> t -> Agent.tool option
val find_tool_implementation : Agent.tool_id -> t -> Tool.implementation option
val find_agent : string -> t -> Agent.agent option
val tools_for_agent : Agent.agent -> t -> (Tool.spec list, error) result

val invoke :
  t ->
  agent_id:string ->
  tool_id:Agent.tool_id ->
  arguments:Yojson.Safe.t ->
  (string, invocation_error) result Lwt.t
