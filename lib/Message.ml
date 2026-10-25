type message = {
  role : Agent.role;
  content : string;
  tool : Tool.definition option;
}

let create role content tool = { role; content; tool }
