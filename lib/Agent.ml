open Lwt.Infix

type role =
  | Agent
  | User
  | System
  | Tool

type tool_id = string

type tool = {
  id : tool_id;
  name : string;
  description : string;
}

type agent = {
  id : string;
  name : string;
  description : string;
  model : string;
  tools : tool_id list;
}

type tool_spec = {
  definition : tool;
  input_schema : Yojson.Safe.t;
}

type tool_call = {
  call_id : string;
  tool_id : tool_id;
  arguments : Yojson.Safe.t;
}

type message =
  | System_text of string
  | User_text of string
  | Assistant_text of string
  | Assistant_tool_calls of {
      text : string option;
      calls : tool_call list;
    }
  | Tool_result of {
      call_id : string;
      tool_id : tool_id;
      content : string;
    }

type request = {
  model : string;
  messages : message list;
  tools : tool_spec list;
}

type response =
  | Assistant_reply of string
  | Tool_requests of {
      text : string option;
      calls : tool_call list;
    }

type provider_error =
  | Connection_error of string
  | Provider_error of { status_code : int option; message : string }
  | Bad_response of string

module type Provider = sig
  val complete : request -> (response, provider_error) result Lwt.t
end

let run ~(provider : (module Provider)) (agent : agent) ~tools messages =
  let (module Provider : Provider) = provider in
  Provider.complete { model = agent.model; messages; tools }
