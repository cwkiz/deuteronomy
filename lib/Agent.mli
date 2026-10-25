
type role =
  | Agent
  | User
  | System
  | Tool

type agent = {
  id : string;
  name : string;
  description : string;
  model : string;
  tools : Tool.id list;
}

type tool_call = {
  call_id : string;
  tool_id : Tool.id;
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
      tool_id : Tool.id;
      content : string;
    }

type request = {
  model : string;
  messages : message list;
  tools : Tool.spec list;
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

val run :
  provider:(module Provider) ->
  agent ->
  tools:Tool.spec list ->
  message list ->
  (response, provider_error) result Lwt.t
