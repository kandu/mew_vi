(*
 * interpret.ml
 * -----------
 * Copyright : (c) 2019 - 2020, ZAN DoYe <zandoye@gmail.com>
 * Licence   : MIT
 *
 * This file is a part of mew_vi.
 *)


open Vi_action
open Edit_action
open React

module Make (Concurrent:Mew.Concurrent.S) =
struct
  module MsgBox = Concurrent.MsgBox
  module Thread = Concurrent.Thread

  let (>>=)= Thread.bind

  type keyseq= Modal.Key.t list

  module Resolver = struct
    type t= status -> keyseq -> result

    and result=
      | Accept of (Edit_action.t * keyseq * Mode.Name.t)
      | Continue of (t * keyseq)
      | Rejected of keyseq

    and status= {
      mode: Mode.Name.t signal;
      set_mode: ?step:step -> Mode.Name.t -> unit;
      keyseq: keyseq signal;
      set_keyseq: ?step:step -> keyseq -> unit;
      mutable resolver_insert: t;
      mutable resolver_normal: t;
      mutable resolver_visual: t;
      mutable resolver_command: t;
    }

    let resolver_dummy= fun _status keyseq-> Rejected keyseq

    let resolver_insert status keyseq=
      match keyseq with
      | []-> Rejected []
      | key::tl->
        if key.Key.control && key.code = Char "[" then
          (status.set_mode Mode.Name.Normal;
          Accept (
            Vi [Motion ((Left 1), 1); ChangeMode Normal]
            , tl
            , Mode.Name.Normal))
        else if key.code = Escape then
          (status.set_mode Mode.Name.Normal;
          Accept (
            Vi [Motion ((Left 1), 1); ChangeMode Normal]
            , tl
            , Mode.Name.Normal))
        else
          Accept (
            Bypass [key]
            , tl
            , Mode.Name.Insert)

    module Common = struct
      let try_count continuation status keyseq=
        let get_count numseq=
          match numseq with
          | ""-> None
          | _-> Some (int_of_string numseq)

        in
        let rec other_num numseq _status keyseq=
          match keyseq with
          | []-> Rejected keyseq
          | key::tl->
            match key.Key.code with
            | Char code->
              if String.length code = 1 && code >= "0" && code <= "9"
                && not (key.Key.control || key.Key.meta || key.Key.shift)
              then
                let resolver= other_num (numseq ^ code) in
                Continue (resolver, tl)
              else
                continuation (get_count numseq) status keyseq
            | Escape-> Rejected tl
            | _->
              continuation (get_count numseq) status keyseq
        in
        let first_num ()=
          match keyseq with
          | []-> Rejected keyseq
          | key::tl->
            match key.Key.code with
            | Char code->
              if String.length code = 1 && not (key.Key.control || key.Key.meta || key.Key.shift) then
                if code >= "1" && code <= "9" then
                  let resolver= other_num code in
                  Continue (resolver, tl)
                else
                  continuation (get_count "") status keyseq
              else
                continuation (get_count "") status keyseq
            | Escape-> Rejected tl
            | _->
              continuation (get_count "") status keyseq
        in
        first_num ()

      let try_motion next_mode count=
        let count=
          match count with
          | Some count-> count
          | None-> 1
        in
        let try_motion_g count num _status keyseq=
          match keyseq with
          | []-> Rejected []
          | key::tl->
            if not (key.Key.control || key.Key.meta || key.Key.shift) then
              match key.Key.code with
              | Char "e"-> Accept (
                  Vi [Motion ((Word_back_end num), count)]
                  , tl
                  , next_mode)
              | Char "E"-> Accept (
                  Vi [Motion ((WORD_back_end num), count)]
                  , tl
                  , next_mode)
              | Char "g"-> Accept (
                  Vi [Motion (GotoLine_first, count)]
                  , tl
                  , next_mode)
              | _-> Rejected keyseq
            else
              Accept (Bypass [key], tl, next_mode)
        in
        let try_motion_occurence ?(backward=false) count num _status keyseq=
          match keyseq with
          | []-> Rejected []
          | key::tl->
            if not (key.Key.control || key.Key.meta || key.Key.shift) then
              match key.Key.code with
              | Char chr->
                if backward then
                  Accept (
                    Vi [Motion (Occurrence_inline_back chr, count*num)]
                    , tl
                    , next_mode)
                else
                  Accept (
                    Vi [Motion (Occurrence_inline chr, count*num)]
                    , tl
                    , next_mode)
              | _-> Rejected keyseq
            else
              Accept (Bypass [key], tl, next_mode)
        in
        let try_motion_occurence_till ?(backward=false) count num _status keyseq=
          match keyseq with
          | []-> Rejected []
          | key::tl->
            if not (key.Key.control || key.Key.meta || key.Key.shift) then
              match key.Key.code with
              | Char chr->
                if backward then
                  Accept (
                    Vi [Motion (Occurrence_inline_till_back chr, count*num)]
                    , tl
                    , next_mode)
                else
                  Accept (
                    Vi [Motion (Occurrence_inline_till chr, count*num)]
                    , tl
                    , next_mode)
              | _-> Rejected keyseq
            else
              Accept (Bypass [key], tl, next_mode)
        in
        let try_motion_n num _status keyseq=
          let num= match num with
            | Some n-> n
            | None-> 1
          in
          match keyseq with
          | []-> Rejected []
          | key::tl->
            if not (key.Key.control || key.Key.meta || key.Key.shift) then
              match key.Key.code with
              | Char "h"-> Accept (
                  Vi [Motion ((Left num), count)]
                  , tl
                  , next_mode)
              | Char "l"-> Accept (
                  Vi [Motion ((Right num), count)]
                  , tl
                  , next_mode)
              | Char "j"-> Accept (
                  Vi [Motion ((Downward num), count)]
                  , tl
                  , next_mode)
              | Char "k"-> Accept (
                  Vi [Motion ((Upward num), count)]
                  , tl
                  , next_mode)
              | Char "0"-> Accept (
                  Vi [Motion ((Line_FirstChar num), count)]
                  , tl
                  , next_mode)
              | Char "$"-> Accept (
                  Vi [Motion ((Line_LastChar num), count)]
                  , tl
                  , next_mode)
              | Char "^"-> Accept (
                  Vi [Motion ((Line_FirstNonBlank num), count)]
                  , tl
                  , next_mode)
              | Char "w"-> Accept (
                  Vi [Motion ((Word num), count)]
                  , tl
                  , next_mode)
              | Char "W"-> Accept (
                  Vi [Motion ((WORD num), count)]
                  , tl
                  , next_mode)
              | Char "b"-> Accept (
                  Vi [Motion ((Word_back num), count)]
                  , tl
                  , next_mode)
              | Char "B"-> Accept (
                  Vi [Motion ((WORD_back num), count)]
                  , tl
                  , next_mode)
              | Char "e"-> Accept (
                  Vi [Motion ((Word_end num), count)]
                  , tl
                  , next_mode)
              | Char "E"-> Accept (
                  Vi [Motion ((WORD_end num), count)]
                  , tl
                  , next_mode)
              | Char "G"-> Accept (
                  Vi [Motion (GotoLine_last, count)]
                  , tl
                  , next_mode)
              | Char "g"->
                let resolver= try_motion_g count num in
                Continue (resolver, tl)
              | Char "f"->
                let resolver= try_motion_occurence count num in
                Continue (resolver, tl)
              | Char "F"->
                let backward= true in
                let resolver= try_motion_occurence ~backward count num in
                Continue (resolver, tl)
              | Char "t"->
                let resolver= try_motion_occurence_till count num in
                Continue (resolver, tl)
              | Char "T"->
                let backward= true in
                let resolver= try_motion_occurence_till ~backward count num in
                Continue (resolver, tl)
              | Char "%"-> Accept (
                  Vi [Motion (Match, 1)]
                  , tl
                  , next_mode)
              | _-> Rejected keyseq
            else
              Accept (Bypass [key], tl, next_mode)
        in
        try_count try_motion_n
    end

    module Normal = struct
      let try_change_mode status keyseq=
        match keyseq with
        | []-> Rejected []
        | key::tl->
          if not (key.Key.control || key.Key.meta || key.Key.shift) then
            match key.Key.code with
            | Char "i"->
              (status.set_mode Mode.Name.Insert;
              Accept (
                Vi [ChangeMode Insert]
                , tl
                , Mode.Name.Insert))
            | Char "I"->
              (status.set_mode Mode.Name.Insert;
              Accept (
                Vi [
                  Motion (Line_FirstNonBlank 1, 1);
                  ChangeMode Insert]
                , tl
                , Mode.Name.Insert))
            | Char "a"->
              (status.set_mode Mode.Name.Insert;
              Accept (
                Vi [
                  Motion (Right_nl 1, 1);
                  ChangeMode Insert]
                , tl
                , Mode.Name.Insert))
            | Char "A"->
              (status.set_mode Mode.Name.Insert;
              Accept (
                Vi [
                  Motion (Line_LastChar_nl 1, 1);
                  ChangeMode Insert]
                , tl
                , Mode.Name.Insert))
            | Char "v"->
              (status.set_mode Mode.Name.Visual;
              Accept (
                Vi [ ChangeMode Visual]
                , tl
                , Mode.Name.Visual))
            | _-> Rejected keyseq
          else
            Rejected keyseq

      let try_modify count=
        let count=
          match count with
          | Some count-> count
          | None->1
        in
        let try_motion_n
            ~action
            count num _status keyseq
          =
          let num= match num with
            | Some n-> n
            | None-> 1
          and next_mode=
            if action = `Change
            then Mode.Name.Insert
            else Mode.Name.Normal
          in
          let make_actions tl motion count=
            let action=
              match action with
              | `Change-> Change (motion, count)
              | `Delete-> Delete (motion, count)
              | `Yank-> Yank (motion, count)
            in
            Accept (
              Vi [action]
              , tl
              , next_mode)
          in
          let try_motion_g count num _status keyseq=
            match keyseq with
            | []-> Rejected []
            | key::tl->
              if not (key.Key.control || key.Key.meta || key.Key.shift) then
                match key.Key.code with
                | Char "e"-> make_actions tl (Word_back_end num) count
                | Char "E"-> make_actions tl (WORD_back_end num) count
                | _-> Rejected keyseq
              else
                Accept (Bypass [key], tl, Mode.Name.Normal)
          in
          let try_motion_quote ?(inner=false) count num _status keyseq=
            match keyseq with
            | []-> Rejected []
            | key::tl->
              if not (key.Key.control || key.Key.meta || key.Key.shift) then
                match key.Key.code with
                | Char chr->
                  if inner then
                    make_actions tl (Quote_inner (chr, num)) count
                  else
                    make_actions tl (Quote_include (chr, num)) count
                | _-> Rejected keyseq
              else
                Accept (Bypass [key], tl, Mode.Name.Normal)
          in
          let try_motion_object ?(inner=false) count num _status keyseq=
            match keyseq with
            | []-> Rejected []
            | key::tl->
              if not (key.Key.control || key.Key.meta || key.Key.shift) then
                match key.Key.code with
                | Char "(" | Char ")"->
                  if inner then
                    make_actions tl (Parenthesis_inner num) count
                  else
                    make_actions tl (Parenthesis_include num) count
                | Char "[" | Char "]"->
                  if inner then
                    make_actions tl (Bracket_inner num) count
                  else
                    make_actions tl (Bracket_include num) count
                | Char "<" | Char ">"->
                  if inner then
                    make_actions tl (AngleBracket_inner num) count
                  else
                    make_actions tl (AngleBracket_include num) count
                | Char "{" | Char "}"->
                  if inner then
                    make_actions tl (Brace_inner num) count
                  else
                    make_actions tl (Brace_include num) count
                | Char "'"->
                  if inner then
                    make_actions tl (Quote_inner ("'", num)) count
                  else
                    make_actions tl (Quote_include ("'", num)) count
                | Char "\""->
                  if inner then
                    make_actions tl (Quote_inner ("\"", num)) count
                  else
                    make_actions tl (Quote_include ("\"", num)) count
                | Char "w"->
                  if inner then
                    make_actions tl (Word_inner num) count
                  else
                    make_actions tl (Word_include num) count
                | Char "W"->
                  if inner then
                    make_actions tl (WORD_inner num) count
                  else
                    make_actions tl (WORD_include num) count
                | Char "q"->
                  let resolver= try_motion_quote ~inner count num in
                  Continue (resolver, tl)
                | _-> Rejected keyseq
              else
                Accept (Bypass [key], tl, Mode.Name.Normal)
          in
          let try_motion_occurence ?(backward=false) count num _status keyseq=
            match keyseq with
            | []-> Rejected []
            | key::tl->
              if not (key.Key.control || key.Key.meta || key.Key.shift) then
                match key.Key.code with
                | Char chr->
                  if backward then
                    make_actions
                      tl
                      (Occurrence_inline_back chr)
                      (count * num)
                  else
                    make_actions tl (Occurrence_inline chr) (count * num)
                | _-> Rejected keyseq
              else
                Accept (Bypass [key], tl, Mode.Name.Normal)
          in
          let try_motion_occurence_till ?(backward=false) count num _status keyseq=
            match keyseq with
            | []-> Rejected []
            | key::tl->
              if not (key.Key.control || key.Key.meta || key.Key.shift) then
                match key.Key.code with
                | Char chr->
                  if backward then
                    make_actions
                      tl
                      (Occurrence_inline_till_back chr)
                      (count * num)
                  else
                    make_actions tl (Occurrence_inline_till chr) (count * num)
                | _-> Rejected keyseq
              else
                Accept (Bypass [key], tl, Mode.Name.Normal)
          in
          match keyseq with
          | []-> Rejected []
          | key::tl->
            if not (key.Key.control || key.Key.meta || key.Key.shift) then
              match key.Key.code with
              | Char "h"-> make_actions tl (Left num) count
              | Char "l"-> make_actions tl (Right num) count
              | Char "j"-> make_actions tl (Downward num) count
              | Char "k"-> make_actions tl (Upward num) count
              | Char "0"-> make_actions tl (Line_FirstChar num) count
              | Char "$"-> make_actions tl (Line_LastChar num) count
              | Char "^"-> make_actions tl (Line_FirstNonBlank num) count
              | Char "w"-> make_actions tl (Word num) count
              | Char "W"-> make_actions tl (WORD num) count
              | Char "b"-> make_actions tl (Word_back num) count
              | Char "B"-> make_actions tl (WORD_back num) count
              | Char "e"-> make_actions tl (Word_end num) count
              | Char "E"-> make_actions tl (WORD_end num) count
              | Char "g"->
                let resolver= try_motion_g count num in
                Continue (resolver, tl)
              | Char "d"-> if action = `Delete then
                  make_actions tl Line count
                else Rejected keyseq
              | Char "a"->
                let resolver= try_motion_object count num in
                Continue (resolver, tl)
              | Char "i"->
                let inner= true in
                let resolver= try_motion_object ~inner count num in
                Continue (resolver, tl)
              | Char "f"->
                let resolver= try_motion_occurence count num in
                Continue (resolver, tl)
              | Char "F"->
                let backward= true in
                let resolver= try_motion_occurence ~backward count num in
                Continue (resolver, tl)
              | Char "t"->
                let resolver= try_motion_occurence_till count num in
                Continue (resolver, tl)
              | Char "T"->
                let backward= true in
                let resolver= try_motion_occurence_till ~backward count num in
                Continue (resolver, tl)
              | Char "%"-> make_actions tl Match 1
              | Char "y"-> if action = `Yank then
                  make_actions tl Line count
                else Rejected keyseq
              | _->
                Rejected keyseq
            else
              Accept (Bypass [key], tl, Mode.Name.Normal)
        in
        let determin count _status keyseq=
          match keyseq with
          | []-> Rejected []
          | key::tl->
            if not (key.Key.control || key.Key.meta || key.Key.shift) then
              match key.Key.code with
              | Char "u"-> Accept (Vi [Undo count], tl, Mode.Name.Normal)
              | Char "p"-> Accept (Vi [Paste_after count], tl, Mode.Name.Normal)
              | Char "P"-> Accept (Vi [Paste_before count], tl, Mode.Name.Normal)
              | Char "d"->
                let resolver= Common.try_count
                  (try_motion_n ~action:`Delete count) in
                Continue (resolver, tl)
              | Char "c"->
                let resolver= Common.try_count
                  (try_motion_n ~action:`Change count) in
                Continue (resolver, tl)
              | Char "D"-> Accept (
                  Vi [Delete ((Line_LastChar 1), count)],
                  tl,
                  Mode.Name.Normal)
              | Char "C"-> Accept (
                  Vi [Delete ((Line_LastChar 1), count)],
                  tl,
                  Mode.Name.Insert)
              | Char "x"->
                Accept (
                  Vi [Delete ((Right 1), count)]
                  , tl
                  , Mode.Name.Normal)
              | Char "s"->
                Accept (
                  Vi [Delete ((Right 1), count)]
                  , tl
                  , Mode.Name.Insert)
              | Char "J"->
                Accept (
                  Vi [(Join count)]
                  , tl
                  , Mode.Name.Normal)
              | Char "y"->
                let resolver= Common.try_count
                  (try_motion_n ~action:`Yank count) in
                Continue (resolver, tl)
              | _-> Rejected keyseq
            else
              Accept (Bypass [key], tl, Mode.Name.Normal)
        in
        determin count

      let try_insert count _status keyseq=
        let count=
          match count with
          | Some count-> count
          | None-> 1
        in
        match keyseq with
        | []-> Rejected []
        | key::tl->
          if not (key.Key.control || key.Key.meta || key.Key.shift) then
            match key.Key.code with
            | Char "o"-> Accept (
                Vi [Insert ((Newline_below ""), count)]
                , tl
                , Mode.Name.Insert)
            | Char "O"-> Accept (
                Vi [Insert ((Newline_above ""), count)]
                , tl
                , Mode.Name.Insert)
            | _-> Rejected keyseq
          else
            Accept (Bypass [key], tl, Mode.Name.Normal)

      let try_motion_modify_insert count status keyseq=
        match Common.try_motion Mode.Name.Normal count status keyseq with
        | Rejected keyseq->
          let resolver status keyseq=
            match try_modify count status keyseq with
            | Rejected keyseq->
              let resolver= try_insert count in
              Continue (resolver, keyseq)
            | r-> r
          in
          Continue (resolver, keyseq)
        | r-> r

      let resolver_normal status keyseq=
        match keyseq with
        | []-> Rejected []
        | _->
          match try_change_mode status keyseq with
          | Rejected keyseq->
            Common.try_count try_motion_modify_insert status keyseq
          | r-> r

    end

    module Visual = struct
      let try_change_mode status keyseq=
        match keyseq with
        | []-> Rejected []
        | key::tl->
          if key.Key.control && key.code = Char "[" then
            (status.set_mode Mode.Name.Normal;
            Accept (
              Vi [ChangeMode Normal]
              , tl
              , Mode.Name.Normal))
          else if key.code = Escape then
            (status.set_mode Mode.Name.Normal;
            Accept (
              Vi [ChangeMode Normal]
              , tl
              , Mode.Name.Normal))
          else
          if not (key.Key.control || key.Key.meta || key.Key.shift) then
            match key.Key.code with
            | Char "v"->
              (status.set_mode Mode.Name.Normal;
              Accept (
                Vi [ ChangeMode Normal]
                , tl
                , Mode.Name.Normal))
            | _-> Rejected keyseq
          else
            Rejected keyseq

      let try_motion= Common.try_motion Mode.Name.Visual

      let try_modify _status keyseq=
        match keyseq with
        | []-> Rejected []
        | key::tl->
          if not (key.Key.control || key.Key.meta || key.Key.shift) then
            match key.Key.code with
            | Char "c" | Char "s"->
              Accept (
                Vi [ DeleteSelected; ChangeMode Insert]
                , tl
                , Mode.Name.Insert)
            | Char "d" | Char "x"->
              Accept (
                Vi [ DeleteSelected; ChangeMode Normal ]
                , tl
                , Mode.Name.Normal)
            | Char "y"->
              Accept (
                Vi [ YankSelected; ChangeMode Normal ]
                , tl
                , Mode.Name.Normal)
            | _-> Rejected keyseq
          else
            Rejected keyseq

      let try_motion_modify count status keyseq=
        match try_motion count status keyseq with
        | Rejected keyseq->
          Continue (try_modify, keyseq)
        | r-> r

      let resolver_visual status keyseq=
        match keyseq with
        | []-> Rejected []
        | _->
          match try_change_mode status keyseq with
          | Rejected keyseq->
            Common.try_count try_motion_modify status keyseq
          | r-> r

    end

    let make_status
        ?(mode= Mode.Name.Insert)
        ?(keyseq=[])
        ?(resolver_insert= resolver_insert)
        ?(resolver_normal= Normal.resolver_normal)
        ?(resolver_visual= Visual.resolver_visual)
        ?(resolver_command= resolver_dummy)
        ()
      =
      let mode, set_mode= React.S.create mode in
      let keyseq, set_keyseq= React.S.create keyseq in
      {
        mode;
        set_mode;
        keyseq;
        set_keyseq;
        resolver_insert;
        resolver_normal;
        resolver_visual;
        resolver_command;
      }

    let rec interpret
      ?resolver ?(keyseq=[])
      status
      (keyIn: Modal.Key.t MsgBox.t) (action: Edit_action.t MsgBox.t) ()
      =
      let resolver=
        match resolver with
        | Some resolver-> resolver
        | None-> match S.value status.mode with
          | Mode.Name.Insert-> status.resolver_insert
          | Mode.Name.Visual-> status.resolver_visual
          | _-> status.resolver_normal
      in
      (match keyseq with
      | []-> MsgBox.get keyIn >>= fun key-> Thread.return [key]
      | _-> Thread.return keyseq)
      >>= fun keyseq->
        match resolver status keyseq with
        | Accept (edit, keyseq, next_mode)->
          status.set_mode next_mode;
          MsgBox.put action edit >>=
          interpret status ~keyseq keyIn action
        | Continue (resolver, keyseq)->
          interpret status ~resolver ~keyseq keyIn action ()
        | Rejected _keyseq->
          MsgBox.put action Dummy >>=
          interpret status keyIn action
  end
end

