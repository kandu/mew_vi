open Vi_action
open Edit_action

module Make (Concurrent:Mew.Concurrent.S) =
struct
  module MsgBox = Concurrent.MsgBox
  module Thread = Concurrent.Thread

  let (>>=)= Thread.bind

  type keyseq= Modal.Key.t list

  module Resolver = struct
    type t= keyseq -> result

    and result=
      | Accept of (Edit_action.t * keyseq * t)
      | Continue of (t * keyseq)
      | Rejected of keyseq

    let resolver_dummy= fun keyseq-> Rejected keyseq
    let default_resolver_insert= ref resolver_dummy
    let default_resolver_normal= ref resolver_dummy
    let current_mode= ref Mode.Name.Normal

    let resolver_insert keyseq=
      match keyseq with
      | []-> Rejected []
      | key::tl->
        if key.Key.control && key.code = Char "[" then
          (current_mode:= Mode.Name.Normal;
          Accept (
            Vi [Motion ((Left 1), 1); ChangeMode Normal]
            , tl
            , !default_resolver_normal))
        else if key.code = Escape then
          (current_mode:= Mode.Name.Normal;
          Accept (
            Vi [Motion ((Left 1), 1); ChangeMode Normal]
            , tl
            , !default_resolver_normal))
        else
          Accept (
            Bypass [key]
            , tl
            , !default_resolver_insert)

    module Normal = struct
      let rec try_count numseq continuation keyseq=
        let get_count numseq=
          match numseq with
          | ""-> None
          | _-> Some (int_of_string numseq)

        in
        let continue ()=
          let count= numseq |> get_count in
          let resolver= continuation count in
          Continue (resolver, keyseq)
        in
        match keyseq with
        | []-> Rejected keyseq
        | key::tl->
          match key.Key.code with
          | Char code->
            if String.length code = 1 && code >= "0" && code <= "9"
              && not (key.Key.control || key.Key.meta || key.Key.shift)
            then
              let resolver= try_count (numseq ^ code) continuation in
              Continue (resolver , tl)
            else
              continue ()
          | Escape-> Rejected tl
          | _->
            continue ()

      let try_motion ?(count=1) keyseq=
        let try_motion_n num keyseq=
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
                  , !default_resolver_normal)
              | Char "l"-> Accept (
                  Vi [Motion ((Right num), count)]
                  , tl
                  , !default_resolver_normal)
              | Char "j"-> Accept (
                  Vi [Motion ((Downward num), count)]
                  , tl
                  , !default_resolver_normal)
              | Char "k"-> Accept (
                  Vi [Motion ((Upward num), count)]
                  , tl
                  , !default_resolver_normal)
              | _-> Rejected keyseq
            else
              Accept (Bypass [key], tl, !default_resolver_normal)
        in
        try_count "" try_motion_n keyseq

      let try_change_mode keyseq=
        match keyseq with
        | []-> Rejected []
        | key::tl->
          if not (key.Key.control || key.Key.meta || key.Key.shift) then
            match key.Key.code with
            | Char "i"->
              (current_mode:= Mode.Name.Insert;
              Accept (
                Vi [ChangeMode Insert]
                , tl
                , !default_resolver_insert))
            | Char "I"->
              (current_mode:= Mode.Name.Insert;
              Accept (
                Vi [
                  Motion (Line_FirstNonBlank 1, 1);
                  ChangeMode Insert]
                , tl
                , !default_resolver_insert))
            | Char "a"->
              (current_mode:= Mode.Name.Insert;
              Accept (
                Vi [
                  Motion (Right 1, 1);
                  ChangeMode Insert]
                , tl
                , !default_resolver_insert))
            | Char "A"->
              (current_mode:= Mode.Name.Insert;
              Accept (
                Vi [
                  Motion (Line_LastChar 1, 1);
                  ChangeMode Insert]
                , tl
                , !default_resolver_insert))
            | _-> Rejected keyseq
          else
            Rejected keyseq

      let try_action count keyseq=
        let drop_first_key= function
          | Rejected [] as r-> r
          | Rejected (_::tl)-> Rejected tl
          | r-> r
        in
        match keyseq with
        | []-> Rejected []
        | _-> drop_first_key
          (match try_change_mode keyseq with
          | Rejected keyseq-> try_motion ?count keyseq
          | r-> r)
    end

    let set_mode= function
      | Mode.Name.Insert-> current_mode:= Mode.Name.Insert
      | _-> current_mode:= Mode.Name.Normal

    let ()=
      set_mode Mode.Name.Normal;
      default_resolver_insert:= resolver_insert;
      default_resolver_normal:= Normal.try_count "" Normal.try_action
  end

  let rec interpret
    ?resolver ?(keyseq=[])
    (keyIn: Modal.Key.t MsgBox.t) (action: Edit_action.t MsgBox.t) ()
    =
    let resolver=
      match resolver with
      | Some resolver-> resolver
      | None-> match !Resolver.current_mode with
        | Mode.Name.Insert-> !Resolver.default_resolver_insert
        | _-> !Resolver.default_resolver_normal
    in
    (match keyseq with
    | []-> MsgBox.get keyIn >>= fun key-> Thread.return [key]
    | _-> Thread.return keyseq)
    >>= fun keyseq->
      match resolver keyseq with
      | Accept (edit, keyseq, resolver)->
        MsgBox.put action edit >>= fun ()->
        interpret ~resolver ~keyseq keyIn action ()
      | Continue (resolver, keyseq)->
        interpret ~resolver ~keyseq keyIn action ()
      | Rejected _keyseq->
        MsgBox.put action Dummy >>= fun ()->
        interpret ~resolver keyIn action ()
end

