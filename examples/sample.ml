open Deuteronomy

module Counter_arguments = struct
  type arguments = { text : string } [@@deriving yojson]
end

module Counter_tool : Tool.S = struct
  type arguments = Counter_arguments.arguments

  let definition : Agent.tool =
    {
      id = "count";
      name = "Count";
      description = "String byte counter";
    }

  let decode_arguments = Counter_arguments.arguments_of_yojson

  let run { Counter_arguments.text } =
    Lwt.return (Ok (string_of_int (String.length text)))
end
let counter = Tool.register (module Counter_tool)

let assistant : Agent.agent =
  {
    id = "counter";
    name = "Counter";
    description = "LOVES counting text. Just can't stop counting.";
    model = "example";
    tools = [ (Tool.definition counter).id ];
  }

let registry =
  match Registry.add_tool counter Registry.empty with
  | Error _ -> failwith "tool registration failed"
  | Ok registry -> (
      match Registry.add_agent assistant registry with
      | Ok registry -> registry
      | Error _ -> failwith "agent registration failed")

let () =
  match
    Lwt_main.run
      (Registry.invoke registry ~agent_id:assistant.id ~tool_id:"count"
         ~arguments:(`Assoc [ ("text", `String "hello") ]))
  with
  | Ok output -> Printf.printf "Tool result: %s\n" output
  | Error (Registry.Agent_not_found id) ->
      Printf.eprintf "Agent not found: %s\n" id
  | Error (Registry.Tool_not_allowed { agent_id; tool_id }) ->
      Printf.eprintf "Agent %s cannot use tool %s\n" agent_id tool_id
  | Error (Registry.Tool_not_registered id) ->
      Printf.eprintf "Tool not registered: %s\n" id
  | Error (Registry.Invalid_arguments message) ->
      Printf.eprintf "Invalid arguments: %s\n" message
  | Error (Registry.Tool_failed message) ->
      Printf.eprintf "Tool failed: %s\n" message