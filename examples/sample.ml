open Deuteronomy

module Counter_tool : Tool.S = struct
  let definition : Agent.tool =
    {
      id = "count";
      name = "Count";
      description = "I can count the bytes in your query.";
    }
  let run query =
  Lwt.return (Ok (string_of_int (String.length query)))
end

let counter_tool = Tool.register (module Counter_tool)
let counter_tool_definition = Tool.definition counter_tool

let assistant : Agent.agent =
  {
    id = "counter";
    name = "Counter";
    description = "I'm a very nice counter.";
    model = "12345";
    tools = [ counter_tool_definition.id ];
  }

let role_name = function
  | Agent.System -> "system"
  | Agent.User -> "user"
  | Agent.Agent -> "assistant"
  | Agent.Tool -> "tool"

let transcript =
  [
    Message.create Agent.System "Count happily." None;
    Message.create Agent.User "Get to counting already." None;
  ]

let () =
  let result =
    match Registry.add_tool counter_tool Registry.empty with
    | Error error -> Error error
    | Ok registry -> Registry.add_agent assistant registry
  in
  match result with
  | Error (Registry.Duplicate_tool id) ->
      Printf.eprintf "Tool already registered: %s\n" id;
      exit 1
  | Error (Registry.Duplicate_agent id) ->
      Printf.eprintf "Agent already registered: %s\n" id;
      exit 1
  | Error (Registry.Unknown_tool { agent_id; tool_id }) ->
      Printf.eprintf "Agent %s references unknown tool %s\n" agent_id tool_id;
      exit 1
  | Error (Registry.Tool_not_found id) ->
      Printf.eprintf "Tool not found: %s\n" id;
      exit 1
  | Error (Registry.Tool_in_use { tool_id; agent_id }) ->
      Printf.eprintf "Tool %s is still used by agent %s\n" tool_id agent_id;
      exit 1
  | Ok registry ->
      (match Registry.find_agent assistant.id registry with
      | Some agent ->
          Printf.printf "Registered %s with %d tool(s)\n" agent.name
            (List.length agent.tools);
          (match
             Registry.find_tool counter_tool_definition.id registry
           with
          | Some definition ->
              Printf.printf "Registered tool: %s (%s)\n" definition.name
                definition.id
          | None -> failwith "Registered tool could not be found");
          print_endline "\nSample conversation:";
          List.iter
            (fun message ->
              Printf.printf "%s: %s\n" (role_name message.Message.role)
                message.Message.content)
            transcript
      | None -> failwith "Registered agent could not be found")