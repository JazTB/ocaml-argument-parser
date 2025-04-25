type expected_t = PStr | PBool

type result_t =
  | String of string
  | Bool of bool
  | Undefined

type flagtype = Long | Short

let rec get_flag_data flag flag_t expected_list =
  match expected_list with
  | [] -> None
  | v::rest ->
    let short, long, _ = v in
    let check_flag = match flag_t with
    | Long -> long
    | Short -> short
    in
    match check_flag = flag with
    | true -> Some v
    | false -> get_flag_data flag flag_t rest

let chars str =
  let rec loop str accum =
    match String.length str > 0 with
    | false -> accum
    | true ->
      let chr = str.[0] in
      let str =
        String.sub str 1 (String.length str - 1)
      in
      loop str (accum@[chr])
  in
  loop str []

(* TODO: clean up the nesting here it's nasty *)
let argument_crawl arguments expected =
  let rec loop
    args exp res in_flag flag_value pos = 
    match args with
    | [] ->
      if in_flag then failwith "Expected a flag!"
      else (res, pos)
    | v::rest ->
      if in_flag then
        (* check if we are a flag value *)
        loop
          rest exp
          (res @ [(flag_value, String v)])
          false "" pos
      else if
        try ((String.sub v 0 2) = "--") with
        | _ -> false
      then
        (* Check if long flag *)
        let name = try
          (String.sub v 2 ((String.length v)-2))
          with _ -> ""
        in
        (* Check the expected type *)
        let fd = get_flag_data name Long exp in
        match fd with
        | None ->
          Printf.sprintf
            "Flag --%s not found" name
          |> failwith
        | Some (_, long, typ) ->
          match typ with
          | PBool ->
            (* found bool flag *)
            loop
              rest exp
              (res@[(long,Bool true)])
              false "" pos
          | PStr ->
            (* look for a string value *)
            loop rest exp res true long pos
      else if v.[0] == '-' then
        (* Check if short flag *)
        let names = try
          (String.sub v 1 ((String.length v)-1))
          with _ -> ""
        in
        match (List.rev (chars names)) with
        | [] -> failwith "Blank short flag"
        | last::names ->
            (* iterate over `names` and treat
               them each as flags *)
          let rec loop_names names_left res =
            match names_left with
            | [] -> res
            | v::rest ->
              let fd =
                get_flag_data
                  (String.make 1 v) Short exp
              in
              match fd with
              | None ->
                Printf.sprintf
                  "Flag -%c not found" v
                |> failwith
              | Some (_, long, typ) ->
                match typ with
                | PStr ->
                  Printf.sprintf
                    "Flag -%s expects value" long
                  |> failwith
                | PBool ->
                  loop_names
                    rest (res@[(long,Bool true)])
          in
          let rest_res = loop_names names [] in
          let res = res @ rest_res in
          let fd =
            get_flag_data
              (String.make 1 last) Short exp
          in
          match fd with
          | None ->
            Printf.sprintf
              "Flag -%c not found" last
            |> failwith
          | Some (_, long, typ) ->
            match typ with
            | PBool ->
              (* found bool flag *)
              loop
                rest exp
                (res@[(long,Bool true)])
                false "" pos
            | PStr ->
              (* look for a string value *)
              loop rest exp res true long pos
      else
        (* Positional value *)
        loop rest exp res false "" (pos@[v])
  in
  loop arguments expected [] false "" []

let parse_arguments arguments expected =
  let res, pos =
    argument_crawl arguments expected
  in
  let hash = Hashtbl.create 20 in
  let rec find_in_res = fun res long ->
    match res with
    | [] -> None
    | (name, value)::rest ->
      match name = long with
      | true -> Some value
      | false -> find_in_res rest long
  in
  let rec loop = fun res exp ->
    match exp with
    | [] -> ()
    | (_, name, _)::rest ->
      let v = match
          find_in_res (List.rev res) name
        with
        | None -> Undefined
        | Some v -> v
      in
      Hashtbl.add hash name v;
      loop res rest
  in
  loop res expected;
  (hash, pos)
