module String_map = Map.Make (String)

type t = {
  tools : Tool.implementation String_map.t;
  agents : Agent.agent String_map.t;
}

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
  | Permission_denied of {
      agent_id : string;
      tool_id : Tool.id;
      reason : string;
    }
  | Invalid_arguments of string
  | Tool_failed of string

type permission_check =
  agent:Agent.agent ->
  tool:Tool.definition ->
  (unit, string) result Lwt.t

let empty = { tools = String_map.empty; agents = String_map.empty }

let allow_all ~agent:_ ~tool:_ = Lwt.return (Ok ())

let add_tool (tool : Tool.implementation) (registry : t) =
  let definition = Tool.definition tool in
  if String_map.mem definition.Tool.id registry.tools then
    Error (Duplicate_tool definition.Tool.id)
  else
    Ok
      {
        registry with
        tools = String_map.add definition.Tool.id tool registry.tools;
      }

let remove_tool (tool_id : Tool.id) (registry : t) =
  if not (String_map.mem tool_id registry.tools) then
    Error (Tool_not_found tool_id)
  else
    match
      String_map.bindings registry.agents
      |> List.find_opt (fun (_, (agent : Agent.agent)) ->
             List.mem tool_id agent.Agent.tools)
    with
    | Some (agent_id, _) -> Error (Tool_in_use { tool_id; agent_id })
    | None ->
        Ok { registry with tools = String_map.remove tool_id registry.tools }

let add_agent (agent : Agent.agent) (registry : t) =
  if String_map.mem agent.Agent.id registry.agents then
    Error (Duplicate_agent agent.Agent.id)
  else
    match
      List.find_opt
        (fun tool_id -> not (String_map.mem tool_id registry.tools))
        agent.Agent.tools
    with
    | Some tool_id ->
        Error (Unknown_tool { agent_id = agent.Agent.id; tool_id })
    | None ->
        Ok
          {
            registry with
            agents = String_map.add agent.Agent.id agent registry.agents;
          }

let find_tool id (registry : t) =
  Option.map Tool.definition (String_map.find_opt id registry.tools)

let find_tool_implementation id (registry : t) =
  String_map.find_opt id registry.tools

let find_agent id (registry : t) = String_map.find_opt id registry.agents

let tools_for_agent (agent : Agent.agent) (registry : t) =
  let rec collect specs = function
    | [] -> Ok (List.rev specs)
    | tool_id :: remaining -> (
        match find_tool_implementation tool_id registry with
        | None ->
            Error (Unknown_tool { agent_id = agent.Agent.id; tool_id })
        | Some implementation ->
            collect (Tool.spec implementation :: specs) remaining)
  in
  collect [] agent.Agent.tools

let invoke ?(permission_check = allow_all) (registry : t) ~(agent_id : string)
    ~(tool_id : Tool.id) ~arguments =
  let reject error = Lwt.return (Error error) in
  match find_agent agent_id registry with
  | None -> reject (Agent_not_found agent_id)
  | Some agent when not (List.mem tool_id agent.Agent.tools) ->
      reject (Tool_not_allowed { agent_id; tool_id })
  | Some agent -> (
      match find_tool_implementation tool_id registry with
      | None -> reject (Tool_not_registered tool_id)
      | Some implementation ->
          let definition = Tool.definition implementation in
          Lwt.bind (permission_check ~agent ~tool:definition) (function
            | Error reason ->
                reject (Permission_denied { agent_id; tool_id; reason })
            | Ok () ->
                Tool.invoke implementation arguments
                |> Lwt.map (function
                     | Ok result -> Ok result
                     | Error (Tool.Invalid_arguments message) ->
                         Error (Invalid_arguments message)
                     | Error (Tool.Tool_failed message) ->
                         Error (Tool_failed message))))
