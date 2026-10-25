open Deuteronomy
open Lwt.Infix

module Counter_arguments = struct
  type arguments = { text : string } [@@deriving yojson]
end

module Counter_tool : Tool.S = struct
  type arguments = Counter_arguments.arguments

  let definition : Tool.definition =
    {
      id = "count";
      name = "Count";
      description = "String byte counter";
    }

  let input_schema =
    `Assoc
      [
        ("type", `String "object");
        ("properties", `Assoc [ ("text", `Assoc [ ("type", `String "string") ]) ]);
        ("required", `List [ `String "text" ]);
        ("additionalProperties", `Bool false);
      ]

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

type run_error =
  | Registry_error of Registry.error
  | Provider_error of Connector.error
  | Tool_error of {
      call_id : string;
      error : Registry.invocation_error;
    }
  | Step_limit_reached

let run ~(provider : (module Agent.Provider)) registry agent ~max_steps messages =
  match Registry.tools_for_agent agent registry with
  | Error error -> Lwt.return (Error (Registry_error error))
  | Ok tools ->
      let rec complete remaining messages =
        if remaining < 1 then Lwt.return (Error Step_limit_reached)
        else
          Agent.run ~provider agent ~tools messages >>= function
          | Error error -> Lwt.return (Error (Provider_error error))
          | Ok (Connector.Assistant_reply answer) -> Lwt.return (Ok answer)
          | Ok (Connector.Tool_requests { text; calls }) ->
              if calls = [] then
                Lwt.return
                  (Error
                     (Provider_error
                        (Connector.Bad_response "Provider returned no tool calls")))
              else if remaining = 1 then
                Lwt.return (Error Step_limit_reached)
              else
                let rec invoke_calls results = function
                  | [] -> Lwt.return (Ok (List.rev results))
                  | call :: rest ->
                      Registry.invoke registry ~agent_id:agent.Agent.id
                        ~tool_id:call.Connector.tool_id
                        ~arguments:call.arguments
                      >>= (function
                       | Error error ->
                           Lwt.return
                             (Error
                                (Tool_error
                                   { call_id = call.call_id; error }))
                       | Ok content ->
                           let result =
                             Connector.Tool_result
                               {
                                 call_id = call.call_id;
                                 tool_id = call.tool_id;
                                 content;
                               }
                           in
                           invoke_calls (result :: results) rest)
                in
                invoke_calls [] calls >>= function
                | Error error -> Lwt.return (Error error)
                | Ok results ->
                    complete (remaining - 1)
                      (messages
                      @ (Connector.Assistant_tool_calls { text; calls }
                        :: results))
      in
      complete max_steps messages

let () =
  match
    Lwt_main.run
      (run ~provider:(module Deuteronomy_openai.Provider) registry assistant
         ~max_steps:8
         [ Connector.User_text "Please count the word hello." ])
  with
  | Ok answer -> Printf.printf "Assistant: %s\n" answer
  | Error (Provider_error error) -> (
      match error with
      | Connector.Connection_error message ->
          Printf.eprintf "Could not connect to provider: %s\n" message
      | Connector.Provider_error { status_code; message } ->
          Printf.eprintf "Provider error";
          Option.iter (Printf.eprintf " (status %d)") status_code;
          Printf.eprintf ": %s\n" message
      | Connector.Bad_response message ->
          Printf.eprintf "Unexpected provider response: %s\n" message)
  | Error (Registry_error _) ->
      Printf.eprintf "Agent tools are not registered correctly.\n"
  | Error (Tool_error { call_id; error = Registry.Agent_not_found id }) ->
      Printf.eprintf "Tool call %s: agent not found: %s\n" call_id id
  | Error (Tool_error { call_id; error = Registry.Tool_not_allowed { agent_id; tool_id } }) ->
      Printf.eprintf "Tool call %s: agent %s cannot use tool %s\n" call_id agent_id tool_id
  | Error (Tool_error { call_id; error = Registry.Tool_not_registered id }) ->
      Printf.eprintf "Tool call %s: tool not registered: %s\n" call_id id
  | Error (Tool_error { call_id; error = Registry.Invalid_arguments message })
  | Error (Tool_error { call_id; error = Registry.Tool_failed message }) ->
      Printf.eprintf "Tool call %s failed: %s\n" call_id message
  | Error Step_limit_reached ->
      Printf.eprintf "Agent reached the provider-call limit.\n"