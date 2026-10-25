# Deuteronomy

Currently rudimentary no-batteries-included typed building blocks for composing agentic pipelines in OCaml. 

## A Tour of Deuteronomy

```ocaml
open Deuteronomy
```

A tool is a module implemented by satisfying the Tool.S interface and is defined as such: 
```ocaml
module type S = sig
  val definition : Agent.tool
  val run : string -> (string, string) result Lwt.t
end
```

The underlying definition of the tool is defined by the ``Agent`` module. 

```ocaml
type tool = {
  id : tool_id;
  name : string;
  description : string;
}
```

And the tool itself's query is wrapped around an Lwt promise. Here's a sample of a valid tool module:

```ocaml
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
```

We must now register our tool in the ```Registry``` to eventually be invoked by an agent. The ```Registry``` wraps around a ```Map```, and provides useful definitions and checks to manage multiple tools and agents.

Let us first register our tool, then define an agent that'll use it,

```ocaml

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

```

and utilities for the array of ```Messages```, alongside ```Roles``` for usage with providers APIs:

```ocaml


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

```

Let us then define a registry, and then our counter tool for future usage by our nice counting agent. Let's also define our agent once our tool is registered in the Map:

```ocaml

let registry = Registry.empty

let registry_result =
  match Registry.add_tool counter_tool Registry.empty with
  | Error error -> Error error
  | Ok registry -> Registry.add_agent assistant registry

let () =
  match registry_result with
  | Error (Registry.Duplicate_tool id) ->
      Printf.eprintf "Tool already registered: %s\n" id
  | Error (Registry.Duplicate_agent id) ->
      Printf.eprintf "Agent already registered: %s\n" id
  | Error (Registry.Unknown_tool { agent_id; tool_id }) ->
      Printf.eprintf "Agent %s references unknown tool %s\n" agent_id tool_id
  | Error (Registry.Tool_not_found id) ->
      Printf.eprintf "Tool not found: %s\n" id
  | Error (Registry.Tool_in_use { tool_id; agent_id }) ->
      Printf.eprintf "Tool %s is still used by agent %s\n" tool_id agent_id
  | Ok registry ->
      Printf.printf "Registered %s with %d tool(s)\n"
        assistant.name (List.length assistant.tools);
      match Registry.find_tool counter_tool_definition.id registry with
      | Some definition ->
          Printf.printf "Registered tool: %s (%s)\n"
            definition.name definition.id
      | None ->
          failwith "Registered tool could not be found"
```





