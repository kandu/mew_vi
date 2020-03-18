(*
 * mew_vi.ml
 * -----------
 * Copyright : (c) 2019, ZAN DoYe <zandoye@gmail.com>
 * Licence   : MIT
 *
 * This file is a part of mew_vi.
 *)


module Make (Concurrent:Mew.Concurrent.S) =
struct
  module Core = Mew.Make(Modal)(Concurrent)

  class edit state =object
    inherit Core.edit state
  end

  class state modes =object
    inherit Core.state modes
  end
end

