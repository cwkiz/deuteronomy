open Deuteronomy.Connector
open Lwt
open Cohttp_lwt_unix

let encode_tool (tool : Deuteronomy.Tool.spec) =
  let definition = tool.definition in
  `Assoc
    [
      ("type", `String "function");
      ("name", `String definition.id);
      ("description", `String definition.description);
      ("parameters", tool.input_schema);
    ]

let encode_tools tools = `List (List.map encode_tool tools)

let complete (_request : request) =
  Lwt.return
    (Error
       (`Provider_error
          {
            Deuteronomy.Agent.status_code = None;
            message = "Stub provider";
          }))
