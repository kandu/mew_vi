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
  | Dummy
  | Move of vi_object
  | Delete of vi_object

module Make (Concurrent:Mew.Concurrent.S) =
struct
  module MsgBox = Concurrent.MsgBox
  module Thread = Concurrent.Thread

  let (>>=)= Thread.bind

  let rec interpret (msgBox: Modal.Key.t MsgBox.t)=
    MsgBox.get msgBox >>= fun key->
      (match key.code with
        | Char _uchar-> Dummy
        | Enter-> Dummy
        | Escape-> Dummy
        | Tab-> Dummy
        | Up-> Dummy
        | Down-> Dummy
        | Left-> Dummy
        | Right-> Dummy
        | F1-> Dummy
        | F2-> Dummy
        | F3-> Dummy
        | F4-> Dummy
        | F5-> Dummy
        | F6-> Dummy
        | F7-> Dummy
        | F8-> Dummy
        | F9-> Dummy
        | F10-> Dummy
        | F11-> Dummy
        | F12-> Dummy
        | Next_page-> Dummy
        | Prev_page-> Dummy
        | Home-> Dummy
        | End-> Dummy
        | Insert-> Dummy
        | Delete-> Dummy
        | Backspace-> Dummy);
      interpret msgBox
end

