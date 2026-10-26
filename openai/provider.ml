open Deuteronomy.Connector
open Lwt
open Cohttp_lwt_unix


let complete (_request : request) =
  Lwt.return
    (Error
       (Provider_error
          {
            status_code = None;
            message = "Stub provider";
          }))
