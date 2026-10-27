open Deuteronomy

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

let () =
  match Registry.tools_for_agent assistant registry with
  | Error _ -> Printf.eprintf "Agent tools are not registered correctly.\n"
  | Ok tools -> (
      match
        Lwt_main.run
          (Agent.run ~provider:(module Deuteronomy_openai.Provider) assistant
             ~tools
             (List.to_seq
                [ Connector.User_text "Please count the word hello." ]))
      with
      | Ok (Agent.Assistant_reply answer) ->
          Printf.printf "Assistant: %s\n" answer
      | Ok (Agent.Tool_requests { calls; _ }) ->
          Printf.printf "Assistant requested %d tool call(s).\n"
            (List.length calls)
      | Error (`Connection_error message) ->
          Printf.eprintf "Could not connect to provider: %s\n" message
      | Error (`Provider_error { status_code; message }) ->
          Printf.eprintf "Provider error";
          Option.iter (Printf.eprintf " (status %d)") status_code;
          Printf.eprintf ": %s\n" message
      | Error (`Bad_response message) ->
          Printf.eprintf "Unexpected provider response: %s\n" message)