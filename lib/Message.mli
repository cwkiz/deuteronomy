type message = {
  role : Agent.role;
  content : string;
  tool : Agent.tool option;
}

val create : Agent.role -> string -> Agent.tool option -> message
