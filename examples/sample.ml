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
      description = "Counts the bytes in the supplied text.";
    }

  let decode_arguments = Counter_arguments.arguments_of_yojson

  let run { Counter_arguments.text } =
    Lwt.return (Ok (string_of_int (String.length text)))
end

let counter_tool = Tool.register (module Counter_tool)
let counter_tool_definition = Tool.definition counter_tool

let assistant : Agent.agent =
  {
    id = "counter";
    name = "Counter";
    description = "Counts text.";
    model = "example";
    tools = [ counter_tool_definition.id ];
  }

let registry =
  match Registry.add_tool counter_tool Registry.empty with
  | Error _ -> failwith "Counter tool registration failed"
  | Ok registry -> (
      match Registry.add_agent assistant registry with
      | Ok registry -> registry
      | Error _ -> failwith "Counter agent registration failed")

let () =
  match
    Lwt_main.run
      (Registry.invoke registry ~agent_id:assistant.id
         ~tool_id:counter_tool_definition.id
         ~arguments:(`Assoc [ ("text", `String "hello") ]))
  with
  | Ok count -> Printf.printf "The text has %s bytes.\n" count
  | Error (Registry.Agent_not_found id) ->
      Printf.eprintf "Agent not found: %s\n" id;
      exit 1
  | Error (Registry.Tool_not_allowed { agent_id; tool_id }) ->
      Printf.eprintf "Agent %s is not allowed to use %s\n" agent_id tool_id;
      exit 1
  | Error (Registry.Tool_not_registered id) ->
      Printf.eprintf "Tool not registered: %s\n" id;
      exit 1
  | Error (Registry.Invalid_arguments message) ->
      Printf.eprintf "Invalid tool arguments: %s\n" message;
      exit 1
  | Error (Registry.Tool_failed message) ->
      Printf.eprintf "Tool failed: %s\n" message;
      exit 1
