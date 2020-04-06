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
      | Accept of (Edit_action.t * keyseq * t)
      | Continue of (t * keyseq)
      | Rejected of keyseq

    and status= {
      mode: Mode.Name.t signal;
      set_mode: ?step:step -> Mode.Name.t -> unit;
      keyseq: keyseq signal;
      set_keyseq: ?step:step -> keyseq -> unit;
      mutable resolver_insert: t;
      mutable resolver_normal: t;
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
            , status.resolver_normal))
        else if key.code = Escape then
          (status.set_mode Mode.Name.Normal;
          Accept (
            Vi [Motion ((Left 1), 1); ChangeMode Normal]
            , tl
            , status.resolver_normal))
        else
          Accept (
            Bypass [key]
            , tl
            , status.resolver_insert)

    module Normal = struct
      let try_count continuation _status keyseq=
        let get_count numseq=
          match numseq with
          | ""-> None
          | _-> Some (int_of_string numseq)

        in
        let continue numseq keyseq=
          let count= numseq |> get_count in
          let resolver= continuation count in
          Continue (resolver, keyseq)
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
                Continue (resolver , tl)
              else
                continue numseq keyseq
            | Escape-> Rejected tl
            | _->
              continue numseq keyseq
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
                  continue "" keyseq
              else
                continue "" keyseq
            | Escape-> Rejected tl
            | _->
              continue "" keyseq
        in
        first_num ()

      let try_motion count=
        let count=
          match count with
          | Some count-> count
          | None-> 1
        in
        let try_motion_n num status keyseq=
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
                  , status.resolver_normal)
              | Char "l"-> Accept (
                  Vi [Motion ((Right num), count)]
                  , tl
                  , status.resolver_normal)
              | Char "j"-> Accept (
                  Vi [Motion ((Downward num), count)]
                  , tl
                  , status.resolver_normal)
              | Char "k"-> Accept (
                  Vi [Motion ((Upward num), count)]
                  , tl
                  , status.resolver_normal)
              | Char "0"-> Accept (
                  Vi [Motion ((Line_FirstChar num), count)]
                  , tl
                  , status.resolver_normal)
              | Char "$"-> Accept (
                  Vi [Motion ((Line_LastChar num), count)]
                  , tl
                  , status.resolver_normal)
              | Char "w"-> Accept (
                  Vi [Motion ((Word num), count)]
                  , tl
                  , status.resolver_normal)
              | Char "b"-> Accept (
                  Vi [Motion ((Word_back num), count)]
                  , tl
                  , status.resolver_normal)
              | _-> Rejected keyseq
            else
              Accept (Bypass [key], tl, status.resolver_normal)
        in
        try_count try_motion_n

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
                , status.resolver_insert))
            | Char "I"->
              (status.set_mode Mode.Name.Insert;
              Accept (
                Vi [
                  Motion (Line_FirstNonBlank 1, 1);
                  ChangeMode Insert]
                , tl
                , status.resolver_insert))
            | Char "a"->
              (status.set_mode Mode.Name.Insert;
              Accept (
                Vi [
                  Motion (Right_nl 1, 1);
                  ChangeMode Insert]
                , tl
                , status.resolver_insert))
            | Char "A"->
              (status.set_mode Mode.Name.Insert;
              Accept (
                Vi [
                  Motion (Line_LastChar_nl 1, 1);
                  ChangeMode Insert]
                , tl
                , status.resolver_insert))
            | _-> Rejected keyseq
          else
            Rejected keyseq

      let try_modify _count=
        let try_motion_n _num status keyseq=
          match keyseq with
          | []-> Rejected []
          | key::tl->
              Accept (Bypass [key], tl, status.resolver_normal)
        in
        try_count try_motion_n

      let resolver_normal status keyseq=
        match keyseq with
        | []-> Rejected []
        | _->
          match try_change_mode status keyseq with
          | Rejected keyseq->
            (match try_count try_motion status keyseq with
            | Rejected keyseq-> try_count try_modify status keyseq
            | r-> r)
          | r-> r

    end

    let make_status
        ?(mode= Mode.Name.Insert)
        ?(keyseq=[])
        ?(resolver_insert= resolver_insert)
        ?(resolver_normal= Normal.resolver_normal)
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
          | _-> status.resolver_normal
      in
      (match keyseq with
      | []-> MsgBox.get keyIn >>= fun key-> Thread.return [key]
      | _-> Thread.return keyseq)
      >>= fun keyseq->
        match resolver status keyseq with
        | Accept (edit, keyseq, resolver)->
          MsgBox.put action edit >>= fun ()->
          interpret status ~resolver ~keyseq keyIn action ()
        | Continue (resolver, keyseq)->
          interpret status ~resolver ~keyseq keyIn action ()
        | Rejected _keyseq->
          let resolver=
            match S.value status.mode with
            | Mode.Name.Insert-> status.resolver_insert
            | _-> status.resolver_normal
          in
          MsgBox.put action Dummy >>= fun ()->
          interpret status ~resolver keyIn action ()
  end
end

