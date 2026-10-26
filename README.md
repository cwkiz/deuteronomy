# Deuteronomy

Currently rudimentary no-providers-included typed building blocks for composing agentic pipelines in OCaml. 

## A Tour of Deuteronomy

```ocaml
open Deuteronomy
```

A tool is a module implemented by satisfying the Tool.S interface.  
Tools accept JSON arguments decoded by a typed schema and is compiled to Yojson for the provider. The tool itself's function is wrapped around an Lwt promise. Here's a sample of a valid tool module (for this example we're not using OpenAI schema specification):

```ocaml
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
```


We must now register our tool in the ```Registry``` to eventually be invoked by an agent. The ```Registry``` wraps around a ```Map```, and provides useful definitions and checks to manage multiple tools and agents.

Let us first register our tool, then define an agent that'll use it. Let's match to each error case as well.

```ocaml
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
  | Error (`Agent_not_found id) ->
      Printf.eprintf "Agent not found: %s\n" id
  | Error (`Tool_not_allowed (agent_id, tool_id)) ->
      Printf.eprintf "Agent %s cannot use tool %s\n" agent_id tool_id
  | Error (`Tool_not_registered id) ->
      Printf.eprintf "Tool not registered: %s\n" id
  | Error (`Invalid_arguments message) ->
      Printf.eprintf "Invalid arguments: %s\n" message
  | Error (`Tool_failed message) ->
      Printf.eprintf "Tool failed: %s\n" message
```

The agent's ```tools``` list is an allow-list, we can check if an agent is permitted to use a tool at runtime as such:

```ocaml
let permission_check ~agent ~tool =
  Lwt.return
    (if agent.Agent.id = "counter" && tool.Tool.id = "count" then Ok ()
     else Error "This agent is not authorized to use this tool.")
```

This is optionally passable to 
```Registry.invoke``` as ```~permission_check```.

and utilities for the array of ```Messages```, alongside ```Roles``` for usage with providers APIs:

```ocaml

let role_name = function
  | Agent.System -> "system"
  | Agent.User -> "user"
  | Agent.Agent -> "assistant"
  | Agent.Tool -> "tool"

let transcript =
  [
    Message.System_text "Count happily.";
    Message.User_text "Get to counting already.";
  ]

```
