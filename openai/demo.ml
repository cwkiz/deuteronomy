open Deuteronomy.Connector

let complete request =
  let message_count = List.length request.messages in
  Lwt.return
    (Ok
       (Assistant_reply
          (Printf.sprintf "I'm fake openai %s %d"
             request.model message_count)))
