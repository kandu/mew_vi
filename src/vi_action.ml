type insert=
  | Append of string (* a *)
  | AppendEol of string (* A *)
  | Insert of string (* i *)
  | InsertBol of string (* I *)
  | Newline_below of string (* o *)
  | Newline_above of string (* O *)

type motion=
  (* left right *)
  | Left of int (* h *)
  | Right of int (* l *)
  | Line_FirstChar of int (* 0 *)
  | Line_FirstNonBlank of int (* ^ *)
  | Line_LastChar of int (* $ *)
  | Line_LastNonBlank of int (* g_ *)

  (* up down *)
  | Upword of int (* k *)
  | Downword of int (* j *)
  | GotoLine of int (* gg or G *)
  | GotoLine_first (* gg *)
  | GotoLine_last (* G *)

  (* word *)
  | Word of int (* w *)
  | WORD of int (* W *)
  | Word_end of int (* e *)
  | WORD_end of int (* E *)
  | Word_back of int (* b *)
  | WORD_back of int (* B *)
  | Word_back_end of int (* ge *)
  | WORD_back_end of int (* gE *)

  (* text object *)
  | Sentence_backword of int (* ( *)
  | Sentence_forward of int (* ) *)
  | Paragraph_backward of int (* { *)
  | Paragraph_forward of int (* } *)

  (* text object selection *)
  | Word_include of int (* aw *)
  | Word_inner of int (* iw *)
  | WORD_include of int (* aW *)
  | WORD_inner of int (* iW *)
  | Sentence_include of int (* as *)
  | Sentence_inner of int (* is *)
  | Paragraph_include of int (* ap *)
  | Paragraph_inner of int (* ip *)

type t=
  | Insert of insert * int
  | Motion of motion * int
  | Delete of motion * int
  | ChangeMode of Mode.Name.t

