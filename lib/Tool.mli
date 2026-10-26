open Lwt

type id = string

type definition = {
  id : id;
  name : string;
  description : string;
}

type spec = {
  definition : definition;
  input_schema : Yojson.Safe.t;
}

module Schema : sig
  type _ t
  type _ fields

  val string : string t
  val integer : int t
  val number : float t
  val boolean : bool t
  val array : 'a t -> 'a list t

  val empty : unit fields
  val required : string -> 'a t -> 'b fields -> ('a * 'b) fields
  val optional : string -> 'a t -> 'b fields -> ('a option * 'b) fields
  val object_ : 'a fields -> 'a t
  val map : ('a -> 'b) -> 'a t -> 'b t

  val to_yojson : 'a t -> Yojson.Safe.t
  val decode : 'a t -> Yojson.Safe.t -> ('a, string) result
end

module type S = sig
  type arguments

  val definition : definition
  val schema : arguments Schema.t
  val run : arguments -> (string, string) result Lwt.t
end

type implementation

type invocation_error =
  [ `Invalid_arguments of string
  | `Tool_failed of string ]

val register : (module S) -> implementation
val definition : implementation -> definition
val spec : implementation -> spec
val invoke :
  implementation ->
  Yojson.Safe.t ->
  (string, invocation_error) result Lwt.t
