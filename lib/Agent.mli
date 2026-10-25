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

