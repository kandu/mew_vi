type insert=
  | Append of string
  | AppendEol of string
  | Insert of string
  | InsertBol of string
  | Newline_below of string
  | Newline_above of string

type motion=
  (* left right *)
  | Left of int
  | Right of int
  | Line_FirstChar of int
  | Line_FirstNonBlank of int
  | Line_LastChar of int
  | Line_LastNonBlank of int

  (* up down *)
  | Upword of int
  | Downword of int
  | GotoLine of int
  | GotoLine_first
  | GotoLine_last

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
  | Sentence_backword of int
  | Sentence_forward of int
  | Paragraph_backward of int
  | Paragraph_forward of int

  (* text object selection *)
  | Word_include of int
  | Word_inner of int
  | WORD_include of int
  | WORD_inner of int
  | Sentence_include of int
  | Sentence_inner of int
  | Paragraph_include of int
  | Paragraph_inner of int

type t=
  | Insert of insert * int
  | Motion of motion * int
  | Delete of motion * int
  | ChangeMode of Mode.Name.t

