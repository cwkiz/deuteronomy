open Deuteronomy
open Deuteronomy_orchestra

module Arguments = struct
  type arguments = { query : string } [@@deriving yojson]
end

let tool : Tool.definition =
  { id = "tool"; name = "Tool"; description = "Returns the supplied query." }

let lookup_tool_runs = ref 0

module Lookup_tool : Tool.S = struct
  type arguments = Arguments.arguments

  let definition = tool
  let input_schema =
    `Assoc
      [
        ("type", `String "object");
        ("properties", `Assoc [ ("query", `Assoc [ ("type", `String "string") ]) ]);
        ("required", `List [ `String "query" ]);
        ("additionalProperties", `Bool false);
      ]
  let decode_arguments = Arguments.arguments_of_yojson
  let run { Arguments.query } =
    incr lookup_tool_runs;
    Lwt.return (Ok query)
end

let tool_implementation = Tool.register (module Lookup_tool)

let failing_tool : Tool.definition =
  { id = "failing"; name = "Failing"; description = "Always fails." }

module Failing_tool : Tool.S = struct
  type arguments = Arguments.arguments

  let definition = failing_tool
  let input_schema = `Assoc []
  let decode_arguments = Arguments.arguments_of_yojson
  let run _ = Lwt.return (Error "deliberate failure")
end

let failing_tool_implementation = Tool.register (module Failing_tool)

module Agent_provider = struct
  let calls = ref 0

  let complete (request : Connector.request) =
    incr calls;
    assert (request.model = "model");
    assert (List.length request.tools = 2);
    assert (
      match request.tools with
      | first :: _ ->
          first.Tool.definition.id = tool.id
          && first.input_schema = Lookup_tool.input_schema
      | [] -> false);
    match !calls with
    | 1 ->
        Lwt.return
          (Ok
             (Connector.Tool_requests
                {
                  text = None;
                  calls =
                    [
                      {
                        Connector.call_id = "agent-call-1";
                        tool_id = tool.id;
                        arguments = `Assoc [ ("query", `String "hello") ];
                      };
                    ];
                }))
    | _ ->
        assert (
          List.exists
            (function
              | Connector.Tool_result { call_id; content; _ } ->
                  call_id = "agent-call-1" && content = "hello"
              | _ -> false)
            request.messages);
        Lwt.return (Ok (Connector.Assistant_reply "Finished with hello"))
end

module Test_connector : Connector.S = struct
  let complete (request : Connector.request) =
    Lwt.return
      (Ok
         (Connector.Assistant_reply
            (Printf.sprintf "%s:%d" request.model
               (List.length request.messages))))
end

module Connection_failure : Connector.S = struct
  let complete _ =
    Lwt.return (Error (Connector.Connection_error "offline"))
end

module Provider_failure : Connector.S = struct
  let complete _ =
    Lwt.return
      (Error
         (Connector.Provider_error
            { status_code = Some 429; message = "rate limited" }))
end

module Bad_provider_response : Connector.S = struct
  let complete _ =
    Lwt.return (Error (Connector.Bad_response "missing assistant content"))
end

module Openai_provider : Connector.S = Deuteronomy_openai.Provider

let deny_permissions ~agent:_ ~tool:_ =
  Lwt.return (Error "requires approval")

let permission_recovery_provider_calls = ref 0

module Permission_recovery_provider : Connector.S = struct
  let complete (request : Connector.request) =
    incr permission_recovery_provider_calls;
    match !permission_recovery_provider_calls with
    | 1 ->
        Lwt.return
          (Ok
             (Connector.Tool_requests
                {
                  text = None;
                  calls =
                    [
                      {
                        Connector.call_id = "permission-call";
                        tool_id = tool.id;
                        arguments = `Assoc [ ("query", `String "hello") ];
                      };
                    ];
                }))
    | 2 ->
        assert (
          List.exists
            (function
              | Connector.Tool_result
                  { call_id; content; tool_id = result_tool_id } ->
                  call_id = "permission-call"
                  && result_tool_id = tool.id
                  && content = "Permission denied: requires approval"
              | _ -> false)
            request.messages);
        Lwt.return (Ok (Connector.Assistant_reply "recovered"))
    | _ ->
        Lwt.return
          (Error (Connector.Bad_response "unexpected provider call"))
end

let malformed_call_provider_calls = ref 0

module Malformed_call_provider : Connector.S = struct
  let complete (_request : Connector.request) =
    incr malformed_call_provider_calls;
    Lwt.return
      (Ok
         (Connector.Tool_requests
            {
              text = None;
              calls =
                [
                  {
                    Connector.call_id = "invalid-call";
                    tool_id = tool.id;
                    arguments = `Assoc [ ("query", `Int 4) ];
                  };
                ];
            }))
end

let agent : Agent.agent =
  {
    id = "agent";
    name = "Agent";
    description = "Uses the test tools.";
    model = "model";
    tools = [ tool.id; failing_tool.id ];
  }

let expect_error expected = function
  | Error actual when actual = expected -> ()
  | Error _ -> failwith "Unexpected error"
  | Ok _ -> failwith "Expected an error"

let expect_invocation_error predicate promise =
  match Lwt_main.run promise with
  | Error error when predicate error -> ()
  | Error _ -> failwith "Unexpected invocation error"
  | Ok _ -> failwith "Expected an invocation error"

let expect_connector_error expected (module Provider : Connector.S)
    (request : Connector.request) =
  match Lwt_main.run (Provider.complete request) with
  | Error actual when actual = expected -> ()
  | Error _ -> failwith "Unexpected connector error"
  | Ok _ -> failwith "Expected a connector error"

let () =
  let user_message = Message.create Agent.User "1" None in
  assert (user_message.role = Agent.User);
  assert (user_message.content = "1");
  assert (user_message.tool = None);

  let model_call : Connector.message =
    Connector.Assistant_tool_calls
      {
        text = None;
        calls =
          [
            {
              call_id = "call-1";
              tool_id = "tool";
              arguments = `Assoc [ ("query", `String "hello") ];
            };
          ];
      }
  in
  let tool_answer : Connector.message =
    Connector.Tool_result
      { call_id = "call-1"; tool_id = "tool"; content = "hello" }
  in
  let request : Connector.request =
    {
      model = "example-model";
      messages = [ Connector.User_text "hello" ];
      tools = [];
    }
  in
  assert (request.model = "example-model");
  assert (request.messages = [ Connector.User_text "hello" ]);
  assert (
    Lwt_main.run (Test_connector.complete request)
    = Ok (Connector.Assistant_reply "example-model:1"));
  expect_connector_error
    (Connector.Connection_error "offline")
    (module Connection_failure) request;
  expect_connector_error
    (Connector.Provider_error
       { status_code = Some 429; message = "rate limited" })
    (module Provider_failure) request;
  expect_connector_error
    (Connector.Bad_response "missing assistant content")
    (module Bad_provider_response) request;
  expect_connector_error
    (Connector.Provider_error
       {
         status_code = None;
         message = "OpenAI provider is not implemented yet";
       })
    (module Openai_provider) request;
  assert (
    match (model_call, tool_answer) with
    | ( Connector.Assistant_tool_calls { calls = [ call ]; _ },
        Connector.Tool_result { call_id; _ } ) ->
        call.call_id = call_id
    | _ -> false);

  let with_tool =
    match Registry.add_tool tool_implementation Registry.empty with
    | Ok registry -> registry
    | Error _ -> failwith "tool registration failed"
  in
  assert (Registry.find_tool tool.id with_tool = Some tool);
  assert (
    Option.map Tool.definition
      (Registry.find_tool_implementation tool.id with_tool)
    = Some tool);
  let conflicting_tool =
    { tool with name = "different definition"; description = "test same id" }
  in
  let module Conflicting_tool : Tool.S = struct
    type arguments = Arguments.arguments

    let definition = conflicting_tool
    let input_schema = `Assoc []
    let decode_arguments = Arguments.arguments_of_yojson
    let run { Arguments.query } = Lwt.return (Ok query)
  end in
  let conflicting_implementation = Tool.register (module Conflicting_tool) in
  expect_error (Registry.Duplicate_tool tool.id)
    (Registry.add_tool conflicting_implementation with_tool);
  assert (Registry.find_tool tool.id with_tool = Some tool);
  expect_error (Registry.Tool_not_found "missing")
    (Registry.remove_tool "missing" with_tool);

  let with_tools =
    match Registry.add_tool failing_tool_implementation with_tool with
    | Ok registry -> registry
    | Error _ -> failwith "failing tool registration failed"
  in
  let with_agent =
    match Registry.add_agent agent with_tools with
    | Ok registry -> registry
    | Error _ -> failwith "agent registration failed"
  in
  Agent_provider.calls := 0;
  assert (
    Lwt_main.run
      (Agent.run ~provider:(module Agent_provider) agent
         ~tools:
           (match Registry.tools_for_agent agent with_tools with
           | Ok tools -> tools
           | Error _ -> failwith "agent tool lookup failed")
         [ Connector.User_text "hello" ])
    = Ok
        (Connector.Tool_requests
           {
             text = None;
             calls =
               [
                 {
                   Connector.call_id = "agent-call-1";
                   tool_id = tool.id;
                   arguments = `Assoc [ ("query", `String "hello") ];
                 };
               ];
           }));
  assert (!Agent_provider.calls = 1);
  assert (Registry.find_agent agent.id with_agent = Some agent);
  assert (Registry.find_agent agent.id Registry.empty = None);
  expect_error (Registry.Duplicate_agent agent.id)
    (Registry.add_agent agent with_agent);
  expect_error
    (Registry.Tool_in_use { tool_id = tool.id; agent_id = agent.id })
    (Registry.remove_tool tool.id with_agent);
  (match Registry.remove_tool tool.id with_tool with
  | Ok registry -> assert (Registry.find_tool tool.id registry = None)
  | Error _ -> failwith "tool removal failed");

  let missing_tool_agent = { agent with tools = [ "missing" ] } in
  expect_error
    (Registry.Unknown_tool { agent_id = agent.id; tool_id = "missing" })
    (Registry.add_agent missing_tool_agent with_tool);

  let arguments = `Assoc [ ("query", `String "hello") ] in
  assert (
    Lwt_main.run
      (Registry.invoke with_agent ~agent_id:agent.id ~tool_id:tool.id
         ~arguments)
    = Ok "hello");
  expect_invocation_error
    (function Registry.Invalid_arguments _ -> true | _ -> false)
    (Registry.invoke with_agent ~agent_id:agent.id ~tool_id:tool.id
       ~arguments:(`Assoc [ ("query", `Int 4) ]));
  expect_invocation_error
    (function Registry.Agent_not_found "missing" -> true | _ -> false)
    (Registry.invoke with_agent ~agent_id:"missing" ~tool_id:tool.id
       ~arguments);
  let restricted_agent = { agent with id = "restricted"; tools = [] } in
  let with_restricted_agent =
    match Registry.add_agent restricted_agent with_agent with
    | Ok registry -> registry
    | Error _ -> failwith "restricted agent registration failed"
  in
  expect_invocation_error
    (function
      | Registry.Tool_not_allowed { agent_id; tool_id } ->
          agent_id = restricted_agent.id && tool_id = tool.id
      | _ -> false)
    (Registry.invoke with_restricted_agent ~agent_id:restricted_agent.id
       ~tool_id:tool.id ~arguments);
  expect_invocation_error
    (function Registry.Tool_failed "deliberate failure" -> true | _ -> false)
    (Registry.invoke with_agent ~agent_id:agent.id ~tool_id:failing_tool.id
       ~arguments);

  let runs_before_denial = !lookup_tool_runs in
  expect_invocation_error
    (function
      | Registry.Permission_denied { agent_id; tool_id; reason } ->
          agent_id = agent.id && tool_id = tool.id
          && reason = "requires approval"
      | _ -> false)
    (Registry.invoke ~permission_check:deny_permissions with_agent
       ~agent_id:agent.id ~tool_id:tool.id ~arguments);
  assert (!lookup_tool_runs = runs_before_denial);

  permission_recovery_provider_calls := 0;
  assert (
    Lwt_main.run
      (Loop.run ~permission_check:deny_permissions
         ~provider:(module Permission_recovery_provider) with_agent agent
         ~max_steps:3 [ Connector.User_text "hello" ])
    = Ok "recovered");
  assert (!permission_recovery_provider_calls = 2);
  assert (!lookup_tool_runs = runs_before_denial);

  malformed_call_provider_calls := 0;
  (match
     Lwt_main.run
       (Loop.run ~provider:(module Malformed_call_provider) with_agent
          agent ~max_steps:3 [ Connector.User_text "hello" ])
   with
  | Error
      (Loop.Tool_error
        { call_id = "invalid-call"; error = Registry.Invalid_arguments _ }) ->
      ()
  | _ -> failwith "Malformed tool arguments should abort the agent loop");
  assert (!malformed_call_provider_calls = 1);
  assert (!lookup_tool_runs = runs_before_denial)
