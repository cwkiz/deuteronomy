type role =
  | Agent
  | User
  | System
  | Tool

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
