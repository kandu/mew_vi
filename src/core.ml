module Make (Concurrent:Mew.Concurrent.S) =
struct
  module Core = Mew.Make(Modal)(Concurrent)
  module Interpret = Interpret.Make(Concurrent)

  class edit state =object
    inherit Core.edit state

    val action_output
      : Vi_action.action Concurrent.MsgBox.t
      = Concurrent.MsgBox.create ()

    method action_output= action_output

    initializer
      Concurrent.Thread.async (Interpret.interpret i action_output)
  end

  class state=
    let modes=
      let open Mode in
      Modes.singleton
        Name.Normal
        { name= Name.Normal;
          timeout= None;
          bindings= Mode.KeyTrie.create None}
    in
  object
    inherit Core.state modes
  end
end

