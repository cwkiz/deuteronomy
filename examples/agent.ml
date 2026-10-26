open Deuteronomy
open Deuteronomy_orchestra.Loop

module Counter_arguments = struct
  type arguments = { text : string }
end

module Counter_tool : Tool.S = struct
  type arguments = Counter_arguments.arguments

  let definition : Tool.definition =
    {
      id = "count";
      name = "Count";
      description = "String byte counter";
    }

  let schema =
    Tool.Schema.map
      (fun (text, ()) -> { Counter_arguments.text })
      (Tool.Schema.object_
         (Tool.Schema.required "text" Tool.Schema.string Tool.Schema.empty))

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

let permission_check ~agent ~tool =
  Lwt.return
    (if agent.Agent.id = "counter" && tool.Tool.id = "count" then Ok ()
     else Error "This agent is not authorized to use this tool.")

let () =
  match
    Lwt_main.run
      (run ~permission_check ~provider:(module Deuteronomy_openai.Provider)
         registry assistant ~max_steps:8
         [ Connector.User_text "Please count the word hello." ])
  with
  | Ok answer -> Printf.printf "Assistant: %s\n" answer
  | Error (`Connection_error message) ->
      Printf.eprintf "Could not connect to provider: %s\n" message
  | Error (`Provider_error { status_code; message }) ->
      Printf.eprintf "Provider error";
      Option.iter (Printf.eprintf " (status %d)") status_code;
      Printf.eprintf ": %s\n" message
  | Error (`Bad_response message) ->
      Printf.eprintf "Unexpected provider response: %s\n" message
  | Error (`Tool_error (call_id, `Agent_not_found id)) ->
      Printf.eprintf "Tool call %s: agent not found: %s\n" call_id id
  | Error (`Tool_error (call_id, `Tool_not_allowed (agent_id, tool_id))) ->
      Printf.eprintf "Tool call %s: agent %s cannot use tool %s\n" call_id agent_id tool_id
  | Error (`Tool_error (call_id, `Permission_denied (_, _, reason))) ->
      Printf.eprintf "Tool call %s denied: %s\n" call_id reason
  | Error (`Tool_error (call_id, `Tool_not_registered id)) ->
      Printf.eprintf "Tool call %s: tool not registered: %s\n" call_id id
  | Error (`Tool_error (call_id, `Invalid_arguments message))
  | Error (`Tool_error (call_id, `Tool_failed message)) ->
      Printf.eprintf "Tool call %s failed: %s\n" call_id message
  | Error `Step_limit_reached ->
      Printf.eprintf "Agent reached the provider-call limit.\n"
  | Error _ ->
      Printf.eprintf "Agent tools are not registered correctly.\n"