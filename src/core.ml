module Make (Concurrent:Mew.Concurrent.S) =
struct
  module Base = Mew.Make(Modal)(Concurrent)
  module Interpret = Interpret.Make(Concurrent)
  module Edit_action = Edit_action
  module Vi_action = Vi_action

  class edit state =object
    inherit Base.edit state

    val action_output
      : Edit_action.t Concurrent.MsgBox.t
      = Concurrent.MsgBox.create ()

    method action_output= action_output

    initializer
      Concurrent.Thread.async (Interpret.interpret o action_output)
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
  object(self)
    inherit Base.state modes
    method vi_edit= new edit self
  end
end

