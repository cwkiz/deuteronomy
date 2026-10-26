type tool_call = Agent.tool_call = {
  call_id : string;
  tool_id : Tool.id;
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
      tool_id : Tool.id;
      content : string;
    }

type request = Agent.request = {
  model : string;
  messages : message Seq.t;
  tools : Tool.spec list;
}

type response = Agent.response =
  | Assistant_reply of string
  | Tool_requests of {
      text : string option;
      calls : tool_call list;
    }

type error = Agent.provider_error

module type S = Agent.Provider
