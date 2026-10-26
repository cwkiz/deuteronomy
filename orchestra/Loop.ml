open Deuteronomy
open Lwt.Infix

type run_error =
  [ Registry.error
  | Agent.provider_error
  | `Tool_error of string * Registry.invocation_error
  | `Step_limit_reached ]

type transcript = {
  mutable contents : Agent.message option array;
  mutable length : int;
}

let create_transcript messages =
  let transcript =
    {
      contents = Array.make (max 1 (List.length messages)) None;
      length = 0;
    }
  in
  List.iter
    (fun message ->
      transcript.contents.(transcript.length) <- Some message;
      transcript.length <- transcript.length + 1)
    messages;
  transcript

let append_message transcript message =
  if transcript.length = Array.length transcript.contents then (
    let capacity = max 4 (Array.length transcript.contents * 2) in
    let contents = Array.make capacity None in
    Array.blit transcript.contents 0 contents 0 transcript.length;
    transcript.contents <- contents);
  transcript.contents.(transcript.length) <- Some message;
  transcript.length <- transcript.length + 1

let transcript_sequence transcript =
  let length = transcript.length in
  let rec sequence index () =
    if index >= length then Seq.Nil
    else
      match transcript.contents.(index) with
      | Some message -> Seq.Cons (message, sequence (index + 1))
      | None -> invalid_arg "Loop.transcript_sequence: missing message"
  in
  sequence 0

let run ?(permission_check = Registry.allow_all)
    ~(provider : (module Agent.Provider)) registry agent ~max_steps messages =
  match Registry.tools_for_agent agent registry with
  | Error error -> Lwt.return (Error (error :> run_error))
  | Ok tools ->
      let transcript = create_transcript messages in
      let rec complete remaining : (string, run_error) result Lwt.t =
        if remaining < 1 then Lwt.return (Error `Step_limit_reached)
        else
          Agent.run ~provider agent ~tools (transcript_sequence transcript)
          >>= function
          | Error error -> Lwt.return (Error (error :> run_error))
          | Ok (Agent.Assistant_reply answer) -> Lwt.return (Ok answer)
          | Ok (Agent.Tool_requests { text; calls }) ->
              if calls = [] then
                Lwt.return
                  (Error
                     (`Bad_response
                       "Agent: Provider returned no tool calls"))
              else if remaining = 1 then
                Lwt.return (Error `Step_limit_reached)
              else
                let rec invoke_calls results = function
                  | [] -> Lwt.return (Ok (List.rev results))
                  | call :: rest ->
                      Registry.invoke ~permission_check registry
                        ~agent_id:agent.Agent.id ~tool_id:call.Agent.tool_id
                        ~arguments:call.arguments
                      >>= (function
                       | Error (`Permission_denied (_, _, reason)) ->
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
                             (Error (`Tool_error (call.call_id, error)))
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
                    List.iter (append_message transcript)
                      (Agent.Assistant_tool_calls { text; calls } :: results);
                    complete (remaining - 1)
      in
      complete max_steps
