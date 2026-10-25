open Deuteronomy.Connector

let complete (_request : request) =
  Lwt.return
    (Error
       (Provider_error
          {
            status_code = None;
            message = "Stub provider";
          }))
