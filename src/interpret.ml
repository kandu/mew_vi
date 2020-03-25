open Vi_action

module Make (Concurrent:Mew.Concurrent.S) =
struct
  module MsgBox = Concurrent.MsgBox
  module Thread = Concurrent.Thread

  let (>>=)= Thread.bind

  let rec interpret (keyIn: Modal.Key.t MsgBox.t) (action: action MsgBox.t) ()=
    MsgBox.get keyIn >>= fun key->
      (match key.code with
        | Char _uchar-> MsgBox.put action Dummy
        | Enter-> MsgBox.put action Dummy
        | Escape-> MsgBox.put action Dummy
        | Tab-> MsgBox.put action Dummy
        | Up-> MsgBox.put action @@ Move (Up 1)
        | Down-> MsgBox.put action @@ Move (Down 1)
        | Left-> MsgBox.put action @@ Move (Left 1)
        | Right-> MsgBox.put action @@ Move (Right 1)
        | F1-> MsgBox.put action Dummy
        | F2-> MsgBox.put action Dummy
        | F3-> MsgBox.put action Dummy
        | F4-> MsgBox.put action Dummy
        | F5-> MsgBox.put action Dummy
        | F6-> MsgBox.put action Dummy
        | F7-> MsgBox.put action Dummy
        | F8-> MsgBox.put action Dummy
        | F9-> MsgBox.put action Dummy
        | F10-> MsgBox.put action Dummy
        | F11-> MsgBox.put action Dummy
        | F12-> MsgBox.put action Dummy
        | Next_page-> MsgBox.put action Dummy
        | Prev_page-> MsgBox.put action Dummy
        | Home-> MsgBox.put action Dummy
        | End-> MsgBox.put action Dummy
        | Insert-> MsgBox.put action Dummy
        | Delete-> MsgBox.put action Dummy
        | Backspace-> MsgBox.put action Dummy) >>=
      interpret keyIn action
end

