type tool_call = {
  call_id : string;
  tool_id : Agent.tool_id;
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
      tool_id : Agent.tool_id;
      content : string;
    }

type request = {
  model : string;
  messages : message list;
}

type response =
  | Assistant_reply of string
  | Tool_requests of {
      text : string option;
      calls : tool_call list;
    }

module type S = sig
  val complete : request -> (response, string) result Lwt.t
end
