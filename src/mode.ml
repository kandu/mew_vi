module Name = struct
  type t=
    | Normal
    | Insert
    | Command

  let compare= compare
end

include Mew.Mode.Make(Key)(Name)

