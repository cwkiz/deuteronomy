type tool_call = Agent.tool_call = {
  call_id : string;
  tool_id : Agent.tool_id;
  arguments : Yojson.Safe.t;
}

type message = Agent.message =
  | System_text of string
  | User_text of string
  | Assistant_text of string
  | Assistant_tool_calls of {
      text : string option;
      calls : tool_call list;
    }
  | Tool_result of {
      call_id : string;
      tool_id : Agent.tool_id;
      content : string;
    }

type request = Agent.request = {
  model : string;
  messages : message list;
  tools : Agent.tool_spec list;
}

type response = Agent.response =
  | Assistant_reply of string
  | Tool_requests of {
      text : string option;
      calls : tool_call list;
    }

type error = Agent.provider_error =
  | Connection_error of string
  | Provider_error of { status_code : int option; message : string }
  | Bad_response of string

module type S = Agent.Provider
