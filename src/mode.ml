module Name = struct
  type t=
    | Normal
    | Insert
    | Commandline

  let compare= compare
end

include Mew.Mode.Make(Key)(Name)

