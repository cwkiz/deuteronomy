open Deuteronomy
open Lwt 

let tool : Agent.tool =
  { id = "tool"; name = "Tool-chan"; description = "hi im tool" }

module Lookup_tool : Tool.S = struct
  let definition = tool
  let run query = Lwt.return (Ok query)
end

let tool_implementation = Tool.register (module Lookup_tool)

let agent : Agent.agent =
  {
    id = "agent";
    name = "Agent";
    description = "hi im agent";
    model = "model";
    tools = [ tool.id ];
  }

let expect_error expected = function
  | Error actual when actual = expected -> ()
  | Error _ -> failwith "Unexpected error"
  | Ok _ -> failwith "Expected an error"

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
    let definition = conflicting_tool
    let run query = Lwt.return (Ok query)
  end in
  let conflicting_implementation = Tool.register (module Conflicting_tool) in
  expect_error (Registry.Duplicate_tool tool.id)
    (Registry.add_tool conflicting_implementation with_tool);
  assert (Registry.find_tool tool.id with_tool = Some tool);
  expect_error (Registry.Tool_not_found "missing")
    (Registry.remove_tool "missing" with_tool);

  let with_agent =
    match Registry.add_agent agent with_tool with
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
    (Registry.add_agent missing_tool_agent with_tool)