open Vi_action
open Edit_action

module Make (Concurrent:Mew.Concurrent.S) =
struct
  module MsgBox = Concurrent.MsgBox
  module Thread = Concurrent.Thread

  let (>>=)= Thread.bind

  let rec interpret (keyIn: Modal.Key.t MsgBox.t) (action: Edit_action.t MsgBox.t) ()=
    MsgBox.get keyIn >>= fun key->
      (match key.code with
        | Char _uchar-> MsgBox.put action (Bypass key)
        | Enter-> MsgBox.put action (Bypass key)
        | Escape-> MsgBox.put action (Bypass key)
        | Tab-> MsgBox.put action (Bypass key)
        | Up-> MsgBox.put action @@ (Vi (Motion ((Upword 1), 1)))
        | Down-> MsgBox.put action @@ (Vi (Motion ((Downword 1), 1)))
        | Left-> MsgBox.put action @@ (Bypass key)
        | Right-> MsgBox.put action @@ (Bypass key)
        | F1-> MsgBox.put action (Bypass key)
        | F2-> MsgBox.put action (Bypass key)
        | F3-> MsgBox.put action (Bypass key)
        | F4-> MsgBox.put action (Bypass key)
        | F5-> MsgBox.put action (Bypass key)
        | F6-> MsgBox.put action (Bypass key)
        | F7-> MsgBox.put action (Bypass key)
        | F8-> MsgBox.put action (Bypass key)
        | F9-> MsgBox.put action (Bypass key)
        | F10-> MsgBox.put action (Bypass key)
        | F11-> MsgBox.put action (Bypass key)
        | F12-> MsgBox.put action (Bypass key)
        | Next_page-> MsgBox.put action (Bypass key)
        | Prev_page-> MsgBox.put action (Bypass key)
        | Home-> MsgBox.put action (Bypass key)
        | End-> MsgBox.put action (Bypass key)
        | Insert-> MsgBox.put action (Bypass key)
        | Delete-> MsgBox.put action (Bypass key)
        | Backspace-> MsgBox.put action (Bypass key)) >>=
      interpret keyIn action
end

