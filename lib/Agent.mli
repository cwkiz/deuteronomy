
type role = Message.role = Agent | User | System | Tool

type agent = {
  id : string;
  mutable name : string;
  mutable description : string;
  mutable model : string;
  mutable tools : Tool.id list;
}

type tool_call = Message.tool_call = {
  call_id : string;
  tool_id : Tool.id;
  arguments : Yojson.Safe.t;
}

type message = Message.message =
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
  messages : message Seq.t;
  tools : Tool.spec list;
}

type response =
  | Assistant_reply of string
  | Tool_requests of {
      text : string option;
      calls : tool_call list;
    }

type provider_error_detail = { status_code : int option; message : string }

type provider_error =
  [ `Connection_error of string
  | `Provider_error of provider_error_detail
  | `Bad_response of string ]

module type Provider = sig
  val complete : request -> (response, provider_error) result Lwt.t
end

val run :
  provider:(module Provider) ->
  agent ->
  tools:Tool.spec list ->
  message Seq.t ->
  (response, provider_error) result Lwt.t
