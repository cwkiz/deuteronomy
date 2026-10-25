open Deuteronomy

module Arguments = struct
  type arguments = { query : string } [@@deriving yojson]
end

let tool : Agent.tool =
  { id = "tool"; name = "Tool"; description = "Returns the supplied query." }

module Lookup_tool : Tool.S = struct
  type arguments = Arguments.arguments

  let definition = tool
  let decode_arguments = Arguments.arguments_of_yojson
  let run { Arguments.query } = Lwt.return (Ok query)
end

let tool_implementation = Tool.register (module Lookup_tool)

let failing_tool : Agent.tool =
  { id = "failing"; name = "Failing"; description = "Always fails." }

module Failing_tool : Tool.S = struct
  type arguments = Arguments.arguments

  let definition = failing_tool
  let decode_arguments = Arguments.arguments_of_yojson
  let run _ = Lwt.return (Error "deliberate failure")
end

let failing_tool_implementation = Tool.register (module Failing_tool)

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

let () =
  let user_message = Message.create Agent.User "1" None in
  assert (user_message.role = Agent.User);
  assert (user_message.content = "1");
  assert (user_message.tool = None);

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
       ~arguments)
