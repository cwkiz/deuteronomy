open Deuteronomy
open Lwt.Infix

type run_error =
  | Registry_error of Registry.error
  | Provider_error of Agent.provider_error
  | Tool_error of {
      call_id : string;
      error : Registry.invocation_error;
    }
  | Step_limit_reached

let run ?(permission_check = Registry.allow_all)
    ~(provider : (module Agent.Provider)) registry agent ~max_steps messages =
  match Registry.tools_for_agent agent registry with
  | Error error -> Lwt.return (Error (Registry_error error))
  | Ok tools ->
      let rec complete remaining messages =
        if remaining < 1 then Lwt.return (Error Step_limit_reached)
        else
          Agent.run ~provider agent ~tools messages >>= function
          | Error error -> Lwt.return (Error (Provider_error error))
          | Ok (Agent.Assistant_reply answer) -> Lwt.return (Ok answer)
          | Ok (Agent.Tool_requests { text; calls }) ->
              if calls = [] then
                Lwt.return
                  (Error
                     (Provider_error
                        (Agent.Bad_response "Agent: Provider returned no tool calls")))
              else if remaining = 1 then
                Lwt.return (Error Step_limit_reached)
              else
                let rec invoke_calls results = function
                  | [] -> Lwt.return (Ok (List.rev results))
                  | call :: rest ->
                      Registry.invoke ~permission_check registry
                        ~agent_id:agent.Agent.id ~tool_id:call.Agent.tool_id
                        ~arguments:call.arguments
                      >>= (function
                       | Error (Registry.Permission_denied { reason; _ }) ->
                           let result =
                             Agent.Tool_result
                               {
                                 call_id = call.call_id;
                                 tool_id = call.tool_id;
                                 content = "Agent: Permission denied: " ^ reason;
                               }
                           in
                           invoke_calls (result :: results) rest
                       | Error error ->
                           Lwt.return
                             (Error
                                (Tool_error
                                   { call_id = call.call_id; error }))
                       | Ok content ->
                           let result =
                             Agent.Tool_result
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
                      @ (Agent.Assistant_tool_calls { text; calls }
                        :: results))
      in
      complete max_steps messages
