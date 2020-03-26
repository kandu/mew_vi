type vi_object=
  | Up of int
  | Down of int
  | Left of int
  | Right of int
  | Word of int
  | WORD of int
  | Word_back of int
  | WORD_back of int
  | Word_end of int
  | WORD_end of int

type action=
  | Bypass of Key.t
  | Dummy
  | Move of vi_object
  | Delete of vi_object

