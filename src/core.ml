(*
 * core.ml
 * -----------
 * Copyright : (c) 2019 - 2020, ZAN DoYe <zandoye@gmail.com>
 * Licence   : MIT
 *
 * This file is a part of mew_vi.
 *)


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

    val config= Interpret.Resolver.make_config ()

    method get_register name=
      let name= if name = "" then "\"" else name in
      let content=
        try Some (Interpret.RegisterMap.find
          name
          config.registers)
        with _-> None
      in content

    method set_register name content=
      let name= if name= "" then "\"" else name in
      config.registers <-
        Interpret.RegisterMap.add name content config.registers

    initializer
      let status= let open Interpret.Resolver in { register= None; count= None } in
      Concurrent.Thread.async (Interpret.Resolver.interpret config status o action_output)
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

