open Deuteronomy

type run_error =
  [ Registry.error
  | Agent.provider_error
  | `Tool_error of string * Registry.invocation_error
  | `Step_limit_reached ]

val run :
  ?permission_check:Registry.permission_check ->
  provider:(module Agent.Provider) ->
  Registry.t ->
  Agent.agent ->
  max_steps:int ->
  Agent.message list ->
  (string, run_error) result Lwt.t
