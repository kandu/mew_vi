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
