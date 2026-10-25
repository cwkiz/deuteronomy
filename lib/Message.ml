type message = {
  role : Agent.role;
  content : string;
  tool : Agent.tool option;
}

let create role content tool = { role; content; tool }

