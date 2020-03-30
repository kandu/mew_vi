type mode=
  | Normal
  | Insert
  | CommandLine

type motion=
  (* left right *)
  | Left of int
  | Right of int
  | Line_FirstChar
  | Line_FirstNonBlank
  | Line_LastChar
  | Line_LastNonBlank

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
  | Sentence_backword
  | Sentence_forward
  | Paragraph_backward
  | Paragraph_forward

  (* text object selection *)
  | Word_include
  | Word_inner
  | WORD_include
  | WORD_inner
  | Sentence_include
  | Sentence_inner
  | Paragraph_include
  | Paragraph_inner

type t=
  | Insert of string
  | Motion of motion * int
  | Delete of motion * int
  | ChangeMode of mode

