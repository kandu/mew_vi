type t=
  | Dummy
  | Bypass of Key.t list
  | Vi of Vi_action.t list

