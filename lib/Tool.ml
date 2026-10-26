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

module Schema = struct
  type _ t =
    | String : string t
    | Integer : int t
    | Number : float t
    | Boolean : bool t
    | Array : 'a t -> 'a list t
    | Object : 'a fields -> 'a t
    | Map : ('a -> 'b) * 'a t -> 'b t

  and _ fields =
    | Empty : unit fields
    | Required : string * 'a t * 'b fields -> ('a * 'b) fields
    | Optional : string * 'a t * 'b fields -> ('a option * 'b) fields

  let string = String
  let integer = Integer
  let number = Number
  let boolean = Boolean
  let array schema = Array schema
  let empty = Empty
  let required name schema rest = Required (name, schema, rest)
  let optional name schema rest = Optional (name, schema, rest)
  let object_ fields = Object fields
  let map transform schema = Map (transform, schema)

  let rec to_yojson : type a. a t -> Yojson.Safe.t = function
    | String -> `Assoc [ ("type", `String "string") ]
    | Integer -> `Assoc [ ("type", `String "integer") ]
    | Number -> `Assoc [ ("type", `String "number") ]
    | Boolean -> `Assoc [ ("type", `String "boolean") ]
    | Array schema ->
        `Assoc
          [
            ("type", `String "array");
            ("items", to_yojson schema);
          ]
    | Object fields ->
        let properties, required = fields_to_yojson fields in
        `Assoc
          [
            ("type", `String "object");
            ("properties", `Assoc properties);
            ("required", `List (List.map (fun name -> `String name) required));
            ("additionalProperties", `Bool false);
          ]
    | Map (_, schema) -> to_yojson schema

  and fields_to_yojson : type a. a fields -> (string * Yojson.Safe.t) list * string list
      = function
    | Empty -> ([], [])
    | Required (name, schema, rest) ->
        let properties, required = fields_to_yojson rest in
        ((name, to_yojson schema) :: properties, name :: required)
    | Optional (name, schema, rest) ->
        let properties, required = fields_to_yojson rest in
        ((name, to_yojson schema) :: properties, required)

  let rec field_names : type a. a fields -> string list = function
    | Empty -> []
    | Required (name, _, rest) -> name :: field_names rest
    | Optional (name, _, rest) -> name :: field_names rest

  let rec decode : type a. a t -> Yojson.Safe.t -> (a, string) result =
   fun schema json ->
    match (schema, json) with
    | String, `String value -> Ok value
    | Integer, `Int value -> Ok value
    | Integer, `Float value
      when Float.is_finite value
           && Float.trunc value = value
           && value >= float_of_int min_int
           && value < float_of_int max_int ->
        Ok (int_of_float value)
    | Number, `Int value -> Ok (float_of_int value)
    | Number, `Float value -> Ok value
    | Boolean, `Bool value -> Ok value
    | Array schema, `List values ->
        let rec decode_values acc = function
          | [] -> Ok (List.rev acc)
          | value :: rest -> (
              match decode schema value with
              | Ok value -> decode_values (value :: acc) rest
              | Error error -> Error error)
        in
        decode_values [] values
    | Object fields, `Assoc values ->
        let keys = List.map fst values in
        if List.length keys <> List.length (List.sort_uniq String.compare keys)
        then Error "Object contains duplicate fields"
        else
          let unknown =
            List.find_opt (fun key -> not (List.mem key (field_names fields))) keys
          in
          (match unknown with
          | Some key -> Error ("Unexpected field: " ^ key)
          | None -> decode_fields fields values)
    | Map (transform, schema), json ->
        Result.map transform (decode schema json)
    | String, _ -> Error "Expected a string"
    | Integer, _ -> Error "Expected an integer"
    | Number, _ -> Error "Expected a number"
    | Boolean, _ -> Error "Expected a boolean"
    | Array _, _ -> Error "Expected an array"
    | Object _, _ -> Error "Expected an object"

  and decode_fields : type a. a fields -> (string * Yojson.Safe.t) list -> (a, string) result
      =
   fun fields values ->
    match fields with
    | Empty -> Ok ()
    | Required (name, schema, rest) -> (
        match List.assoc_opt name values with
        | None -> Error ("Missing required field: " ^ name)
        | Some json -> (
            match decode schema json with
            | Error error -> Error (name ^ ": " ^ error)
            | Ok value ->
                Result.map
                  (fun rest_value -> (value, rest_value))
                  (decode_fields rest values)))
    | Optional (name, schema, rest) -> (
        match List.assoc_opt name values with
        | None ->
            Result.map
              (fun rest_value -> (None, rest_value))
              (decode_fields rest values)
        | Some json -> (
            match decode schema json with
            | Error error -> Error (name ^ ": " ^ error)
            | Ok value ->
                Result.map
                  (fun rest_value -> (Some value, rest_value))
                  (decode_fields rest values)))
end

module type S = sig
  type arguments

  val definition : definition
  val schema : arguments Schema.t
  val run : arguments -> (string, string) result Lwt.t
end

type implementation = (module S)

type invocation_error =
  [ `Invalid_arguments of string
  | `Tool_failed of string ]

let register (implementation : (module S)) : implementation = implementation

let definition (module Tool : S) = Tool.definition

let spec (module Tool : S) =
  {
    definition = Tool.definition;
    input_schema = Schema.to_yojson Tool.schema;
  }

let invoke (module Tool : S) json =
  match Schema.decode Tool.schema json with
  | Error message -> Lwt.return (Error (`Invalid_arguments message))
  | Ok arguments ->
      Tool.run arguments
      |> Lwt.map (Result.map_error (fun message -> `Tool_failed message))
