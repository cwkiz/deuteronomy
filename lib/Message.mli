type message = {
  role : Agent.role;
  content : string;
  tool : Tool.definition option;
}

val create : Agent.role -> string -> Tool.definition option -> message
