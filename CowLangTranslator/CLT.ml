open Printf
let maxv = 200
let maxif = 100
let maxloop = 100
let maxbody = 4000
let maxout = 20000
let direct_print = ref false
let trace = ref false
let to_lower s = String.lowercase_ascii s
let lstrip_ws s =
  let len = String.length s in
  let i = ref 0 in
  while !i < len && (s.[!i] = ' ' || s.[!i] = '\t') do incr i done;
  String.sub s !i (len - !i)
let rstrip s =
  let len = String.length s in
  let i = ref (len - 1) in
  while !i >= 0 && (s.[!i] = ' ' || s.[!i] = '\t' || s.[!i] = '\r' || s.[!i] = '\n') do decr i done;
  if !i < 0 then "" else String.sub s 0 (!i + 1)
let trim s = rstrip (lstrip_ws s)
let normalize_html s =
  let buf = Buffer.create (String.length s) in
  let rec loop i =
    if i >= String.length s then ()
    else
      let remaining = String.sub s i (String.length s - i) in
      if String.length remaining >= 4 && String.sub remaining 0 4 = "&lt;" then
        (Buffer.add_char buf '<'; loop (i+4))
      else if String.length remaining >= 4 && String.sub remaining 0 4 = "&gt;" then
        (Buffer.add_char buf '>'; loop (i+4))
      else if String.length remaining >= 5 && String.sub remaining 0 5 = "&amp;" then
        (Buffer.add_char buf '&'; loop (i+5))
      else (Buffer.add_char buf s.[i]; loop (i+1))
  in
  loop 0; Buffer.contents buf
let strip_any_prompt s =
  let t = lstrip_ws s in
  if String.length t >= 4 && String.sub t 0 4 = ">>> " then trim (String.sub t 4 (String.length t - 4))
  else if String.length t >= 4 && String.sub t 0 4 = "... " then trim (String.sub t 4 (String.length t - 4))
  else trim t
let starts_with_keyword s kw =
  let s2 = to_lower (lstrip_ws s) in
  let kw2 = trim kw in
  let lenk = String.length kw2 in
  String.length s2 >= lenk && String.sub s2 0 lenk = kw2
let extract_name_after_keyword s kw =
  let ks = String.length kw in
  trim (String.sub s ks (String.length s - ks))
let strip_quotes s =
  let s = trim s in
  if String.length s >= 2 && s.[0] = '"' && s.[String.length s - 1] = '"' then
    String.sub s 1 (String.length s - 2)
  else s
let rstrip_kw s kw =
  let s = trim s in
  let n = String.length s and k = String.length kw in
  if n = 0 then ""
  else
    let low = to_lower s in
    if n >= k && String.sub low (n-k) k = to_lower kw then trim (String.sub s 0 (n-k))
    else if n >= k+1 && String.sub low (n-k-1) (k+1) = " " ^ to_lower kw then trim (String.sub s 0 (n-k-1))
    else s
let strip_ctrl_tail s =
  rstrip_kw (rstrip_kw (trim s) "then") "do"
let bool_str b = if b then "true" else "false"
let int2s i = string_of_int i
type vtype = VStr | VInt | VFloat | VChar | VBool | VUnknown
let vtype_of_string s = match to_lower (trim s) with
  | "str" | "string" -> VStr
  | "int" | "integer" -> VInt
  | "float" -> VFloat
  | "char" | "character" -> VChar
  | "bool" | "boolean" | "booleon" -> VBool
  | _ -> VUnknown
let string_of_vtype = function
  | VStr -> "str" | VInt -> "int" | VFloat -> "float" | VChar -> "char" | VBool -> "bool" | VUnknown -> "unknown"
module VarTable = struct
  type t = (string, (vtype * string)) Hashtbl.t
  let create () = Hashtbl.create 97
  let table = create ()
  let vcount = ref 0
  let reset () = Hashtbl.clear table; vcount := 0
  let get name = try Some (Hashtbl.find table (to_lower (trim name))) with Not_found -> None
  let set name typ value =
    let n = to_lower (trim name) in
    if not (Hashtbl.mem table n) then incr vcount;
    Hashtbl.replace table n (typ, value)
  let get_indexed name = get name
  let keys () = Hashtbl.fold (fun k _ acc -> k :: acc) table []
end
let outbuf = ref []
let outcount = ref 0
let push_line s =
  if !direct_print then
    (print_endline (trim s); flush stdout)
  else
    if !outcount < maxout then
      (outbuf := !outbuf @ [trim s]; incr outcount)
    else
      print_endline "Error: output buffer full"

let flush_output () =
  if !direct_print then (outbuf := []; outcount := 0)
  else begin
    List.iter print_endline !outbuf;
    outbuf := []; outcount := 0;
  end
let if_active = ref []
let if_matched = ref []
type loop_struct = {
  mutable ltype: string;
  mutable var: string;
  mutable startv: float;
  mutable endv: float;
  mutable stepv: float;
  mutable cond: string;
  mutable body: string list;
  mutable nest: int;
}
let loops = ref []
let current_prog_name = ref ""
let in_prog = ref false
let dbg msg = if !trace then printf "[TRACE] %s\n%!" msg
let resolve_value tok =
  let ttrim = trim tok in
  let tl = to_lower ttrim in
  let len = String.length ttrim in
  if len >= 2 && (ttrim.[0] = '"' && ttrim.[len-1] = '"'
                  || (len >= 2 && ttrim.[0] = '\'' && ttrim.[len-1] = '\'')) then
    String.sub ttrim 1 (len-2)
  else
    match VarTable.get ttrim with
    | Some (VStr, v) -> v
    | Some (VInt, v) -> v
    | Some (VFloat, v) -> v
    | Some (VChar, v) -> v
    | Some (VBool, v) -> v
    | Some (_, v) -> v
    | None -> ttrim
exception NumParseError of string
let get_val_dp expr =
  let e = trim expr in
  match VarTable.get e with
  | Some (VInt, v) ->
      (try (float_of_string v, 0) with _ -> (0.0, 0))
  | Some (VFloat, v) ->
      (try
         let f = float_of_string v in
         let dp =
           try
             let idx = String.index v '.' in
             String.length v - idx - 1
           with Not_found -> 0
         in
         (f, dp)
       with _ -> (0.0, 0))
  | _ ->
      (try
         let f = float_of_string e in
         let dp =
           try
             let idx = String.index e '.' in
             String.length e - idx - 1
           with Not_found -> 0
         in
         (f, dp)
       with _ -> (0.0, 0))
let rec eval_num_expr expr =
  let ex = trim (normalize_html expr) in
  let find_op pat =
    try Some (String.index_from (to_lower ex) 0 pat) with _ -> None
  in
  let find_sub s =
    let pat = " " ^ s ^ " " in
    try
      let idx = String.index_from ex 0 pat.[0]; in
      None
    with _ -> None
  in
  let find_spaceop op =
    try Some (String.index ex ' ') with _ -> None
  in
  let index_of s sub =
    let rec loop i =
      if i + String.length sub > String.length s then -1
      else if String.sub s i (String.length sub) = sub then i
      else loop (i+1)
    in loop 0
  in
  let try_split op =
    let pat = " " ^ op ^ " " in
    let p = index_of ex pat in
    if p >= 0 then
      let left = String.sub ex 0 p in
      let right = String.sub ex (p + String.length pat) (String.length ex - p - String.length pat) in
      Some (left, right)
    else None
  in
  match try_split "+" with
  | Some (l, r) ->
      let (vl, dpl) = eval_num_expr l in
      let (vr, dpr) = eval_num_expr r in
      (vl +. vr, max dpl dpr)
  | None ->
    begin match try_split "-" with
    | Some (l, r) ->
        let (vl, dpl) = eval_num_expr l in
        let (vr, dpr) = eval_num_expr r in
        (vl -. vr, max dpl dpr)
    | None ->
      begin match try_split "*" with
      | Some (l, r) ->
          let (vl, dpl) = eval_num_expr l in
          let (vr, dpr) = eval_num_expr r in
          (vl *. vr, max dpl dpr)
      | None ->
        begin match try_split "/" with
        | Some (l, r) ->
            let (vl, dpl) = eval_num_expr l in
            let (vr, dpr) = eval_num_expr r in
            (vl /. vr, max dpl dpr)
        | None ->
            get_val_dp ex
        end
      end
    end
and eval_num_expr expr = eval_num_expr expr
let eval_num_expr_val expr =
  let (v, _) = eval_num_expr expr in v
let rec eval_bool_expr expr =
  let ex = trim (normalize_html expr) in
  let low = to_lower ex in
  let rec split_on_word s word =
    let pat = " " ^ word ^ " " in
    let rec loop i =
      if i + String.length pat > String.length s then None
      else if String.sub s i (String.length pat) = pat then
        Some (String.sub s 0 i, String.sub s (i + String.length pat) (String.length s - i - String.length pat))
      else loop (i+1)
    in loop 0
  in
  match split_on_word low " or " with
  | Some (l, r) -> (eval_bool_expr (String.sub ex 0 (String.length l))) || (eval_bool_expr (String.sub ex (String.length l + 4) (String.length ex - String.length l - 4)))
  | None ->
    begin match split_on_word low " and " with
    | Some (l, r) -> (eval_bool_expr (String.sub ex 0 (String.length l))) && (eval_bool_expr (String.sub ex (String.length l + 5) (String.length ex - String.length l - 5)))
    | None -> begin
        let ex_trim = trim ex in
        let find_cmp ops =
          let rec loop = function
            | [] -> None
            | op :: rest ->
                (try
                   let p = index_of ex_trim op in
                   if p >= 0 then Some (op, p) else loop rest
                 with _ -> loop rest)
          in loop ops
        in
        let ops = ["<="; ">="; "!="; "=="; "<"; ">"] in
        let rec find_sub s sub =
          let rec loop i =
            if i + String.length sub > String.length s then -1
            else if String.sub s i (String.length sub) = sub then i
            else loop (i+1)
          in loop 0
        in
        let rec find_first opslist =
          match opslist with
          | [] -> None
          | op :: rest ->
              let idx = find_sub ex_trim op in
              if idx >= 0 then Some (op, idx) else find_first rest
        in
        match find_first ops with
        | None -> false
        | Some (op, p) ->
            let oplen = String.length op in
            let l = trim (String.sub ex_trim 0 p) in
            let r = trim (String.sub ex_trim (p + oplen) (String.length ex_trim - p - oplen)) in
            let vl = eval_num_expr_val l in
            let vr = eval_num_expr_val r in
            begin match op with
            | "<" -> vl < vr
            | ">" -> vl > vr
            | "<=" -> vl <= vr
            | ">=" -> vl >= vr
            | "==" -> vl = vr
            | "!=" -> vl <> vr
            | _ -> false
            end
      end
    end
let next_token_and_op s_ref =
  let s = ref (lstrip_ws (normalize_html !s_ref)) in
  let token = ref "" in
  let op = ref '\000' in
  if String.length !s = 0 then (s_ref := ""; ("", '\000'))
  else
    let n = String.length !s in
    let in_squote = ref false and in_dquote = ref false in
    let i = ref 0 in
    while !i < n do
      let ch = !s.[!i] in
      if not !in_squote && not !in_dquote then
        (if ch = '\'' then in_squote := true
         else if ch = '"' then in_dquote := true
         else if ch = '&' || ch = '+' then
           begin
             token := trim (String.sub !s 0 !i);
             op := ch;
             if !i + 1 <= n - 1 then s_ref := lstrip_ws (String.sub !s (!i + 1) (n - !i - 1)) else s_ref := "";
             raise Exit
           end
         else
           (* detect literal "&amp;" *)
           if !i + 4 < n && String.sub !s !i 5 = "&amp;" then
             begin
               token := trim (String.sub !s 0 !i);
               op := '&';
               if !i + 5 <= n - 1 then s_ref := lstrip_ws (String.sub !s (!i + 5) (n - !i - 5)) else s_ref := "";
               raise Exit
             end
        )
      else
        (if !in_squote && ch = '\'' then in_squote := false;
         if !in_dquote && ch = '"' then in_dquote := false);
      incr i
    done;
    !s_ref := "";
    (trim !s, '\000')
let do_print expr =
  let rest = ref (lstrip_ws (normalize_html expr)) in
  let first = ref true in
  let last_op = ref '\000' in
  let lineout = Buffer.create 200 in
  try
    while true do
      let (token, op) = next_token_and_op rest in
      if token = "" && op = '\000' then raise Exit;
      let piece = trim (resolve_value token) in
      if not !first then
        if !last_op = '&' then Buffer.add_char lineout ' ';
      if String.length piece > 0 then Buffer.add_string lineout piece;
      if op = '\000' then raise Exit;
      last_op := op;
      first := false
    done
  with Exit -> ();
  push_line (Buffer.contents lineout)
let set_or_create_var name typ_str value =
  let typ = vtype_of_string typ_str in
  VarTable.set name typ value
let set_variable name typ value =
  let typ = vtype_of_string typ in
  match VarTable.get name with
  | Some _ -> VarTable.set name typ value
  | None -> ()
let store_variable cmd =
  try
    let p2 = String.index cmd ':' in
    let p2 =
      let rec findp i =
        if i + 1 >= String.length cmd then raise Not_found
        else if cmd.[i] = ':' && cmd.[i+1] = ':' then i else findp (i+1)
      in findp 0
    in
    let p3 = try String.index cmd '=' with Not_found -> raise (Failure "store_variable: missing =") in
    let typ = trim (String.sub cmd 0 p2) in
    let name = trim (String.sub cmd (p2+2) (p3 - (p2+2))) in
    let value = trim (String.sub cmd (p3+1) (String.length cmd - p3 - 1)) in
    VarTable.set name (vtype_of_string typ) (strip_quotes value);
    ()
  with _ -> push_line "Error: bad store_variable syntax"
let handle_assignment line =
  try
    let p = String.index line '=' in
    let name = trim (String.sub line 0 p) in
    let expr = trim (String.sub line (p+1) (String.length line - p - 1)) in
    if String.length expr >= 1 && expr.[0] = '"' then
      let sval = strip_quotes expr in
      set_or_create_var name "str" sval
    else
      let (valf, dp) = eval_num_expr expr in
      if dp > 0 then
        set_or_create_var name "float" (sprintf "%.*f" (max 1 dp) valf)
      else
        if abs_float (valf -. (float_of_int (int_of_float (floor (valf +. 0.5))))) < 1e-12 then
          set_or_create_var name "int" (string_of_int (int_of_float (floor (valf +. 0.5))))
        else
          set_or_create_var name "float" (sprintf "%.6f" valf)
  with _ -> push_line "Error: bad assignment"
let append_to_current_body s =
  match !loops with
  | [] -> ()
  | top :: rest ->
      if List.length top.body < maxbody then
        top.body <- top.body @ [s]
      else
        push_line "Error: loop body too large"
let run_loop ls =
  match String.trim ls.ltype with
  | "for" ->
      let rec loop_float v =
        if (ls.stepv >= 0.0 && v <= ls.endv) || (ls.stepv < 0.0 && v >= ls.endv) then
          begin
            set_or_create_var ls.var "int" (string_of_int (int_of_float (floor (v +. 0.5))));
            (* execute block *)
            let rec exec_block lines =
              let rec iter j =
                if j >= List.length lines then ()
                else
                  let s = trim (normalize_html (List.nth lines j)) in
                  parse_line s; iter (j+1)
              in iter 0
            in
            exec_block ls.body;
            loop_float (v +. ls.stepv)
          end
        else ()
      in
      loop_float ls.startv
  | "while" ->
      while eval_bool_expr ls.cond do
        List.iter (fun s -> parse_line (trim (normalize_html s))) ls.body
      done
  | "dowhile" ->
      begin
        while true do
          List.iter (fun s -> parse_line (trim (normalize_html s))) ls.body;
          if not (eval_bool_expr ls.cond) then raise Exit
        done
      with Exit -> ()
  | _ -> push_line "Error: unknown loop type"
and handle_loop_start l =
  let low = to_lower l in
  if List.length !loops >= maxloop then (push_line "Error: too many nested loops"; ())
  else
    let new_ls = { ltype = "unknown"; var = ""; startv = 0.0; endv = 0.0; stepv = 1.0; cond = ""; body = []; nest = 0 } in
    if starts_with_keyword low "for" then
      begin
        new_ls.ltype <- "for";
        (* parse: for <var> range <start> to <end> [step <step>] *)
        try
          let p_range = index_of (to_lower l) "range" in
          let p_to = index_of (to_lower l) "to" in
          let p_step = try Some (index_of (to_lower l) "step") with _ -> None in
          new_ls.var <- trim (String.sub l 4 (p_range - 4));
          let tmpstart = trim (String.sub l (p_range + 5) (p_to - (p_range + 5))) in
          let startv = float_of_string tmpstart in
          let (endv, stepv) =
            match p_step with
            | Some p3 ->
                let tmpend = trim (String.sub l (p_to + 2) (p3 - (p_to + 2))) in
                let tmpstep = trim (String.sub l (p3 + 4) (String.length l - p3 - 4)) in
                (float_of_string tmpend, float_of_string tmpstep)
            | None ->
                let tmpend = trim (String.sub l (p_to + 2) (String.length l - p_to - 2)) in
                (float_of_string tmpend, 1.0)
          in
          new_ls.startv <- startv;
          new_ls.endv <- endv;
          new_ls.stepv <- stepv;
          dbg (sprintf "FOR %s from %s to %s step %s" new_ls.var (string_of_float startv) (string_of_float endv) (string_of_float stepv));
        with _ -> push_line "Error: bad for syntax"
      end
    else if starts_with_keyword low "while" then
      begin
        new_ls.ltype <- "while";
        new_ls.cond <- strip_ctrl_tail (String.sub l 5 (String.length l - 5))
      end
    else if starts_with_keyword low "dowhile" || starts_with_keyword low "do while" then
      begin
        new_ls.ltype <- "dowhile";
        let pos = index_of (to_lower l) "while" in
        new_ls.cond <- strip_ctrl_tail (String.sub l (pos + 5) (String.length l - pos - 5))
      end
    else new_ls.ltype <- "unknown";
    loops := !loops @ [new_ls]

and index_of s sub =
  let rec loop i =
    if i + String.length sub > String.length s then raise Not_found
    else if String.sub s i (String.length sub) = sub then i else loop (i+1)
  in loop 0

(* append_to_current_body is defined above *)

(* execute_block: executes a list of lines. It will handle nested loops captured inline. We replicated the Fortran approach. *)
and execute_block body =
  let rec process_lines lines =
    let rec loop j =
      if j >= List.length lines then ()
      else
        let s = trim (normalize_html (List.nth lines j)) in
        let low = to_lower s in
        (* nested loop inside body: detect and capture *)
        if starts_with_keyword low "for" || starts_with_keyword low "while" || starts_with_keyword low "dowhile" || starts_with_keyword low "do while" then
          begin
            (* capture inner loop and its body until matching end ... consider nested count *)
            let inner = { ltype = ""; var = ""; startv = 0.0; endv = 0.0; stepv = 1.0; cond = ""; body = []; nest = 0 } in
            (* parse header *)
            if starts_with_keyword low "for" then
              begin
                inner.ltype <- "for";
                (try
                   let p_range = index_of (to_lower s) "range" in
                   let p_to = index_of (to_lower s) "to" in
                   let p_step = try Some (index_of (to_lower s) "step") with _ -> None in
                   inner.var <- trim (String.sub s 4 (p_range - 4));
                   let a = trim (String.sub s (p_range + 5) (p_to - (p_range + 5))) in
                   inner.startv <- float_of_string a;
                   (match p_step with
                    | Some p3 ->
                        let b = trim (String.sub s (p_to + 2) (p3 - (p_to + 2))) in
                        let c = trim (String.sub s (p3 + 4) (String.length s - p3 - 4)) in
                        inner.endv <- float_of_string b; inner.stepv <- float_of_string c
                    | None ->
                        let b = trim (String.sub s (p_to + 2) (String.length s - p_to - 2)) in
                        inner.endv <- float_of_string b; inner.stepv <- 1.0))
                 with _ -> push_line "Error: bad inner for syntax")
              end
            else if starts_with_keyword low "while" then
              inner.ltype <- "while"; inner.cond <- strip_ctrl_tail (String.sub s 6 (String.length s - 6))
            else inner.ltype <- "dowhile"; inner.cond <- strip_ctrl_tail (String.sub s (index_of (to_lower s) "while" + 5) (String.length s - index_of (to_lower s) "while" - 5));
            let start_idx = j + 1 in
            let nest = ref 0 in
            let k = ref start_idx in
            while !k < List.length lines do
              let linek = trim (List.nth lines !k) in
              let lowk = to_lower (lstrip_ws linek) in
              if (starts_with_keyword lowk "for" || starts_with_keyword lowk "while" || starts_with_keyword lowk "dowhile" || starts_with_keyword lowk "do while") then
                (nest := !nest + 1; inner.body <- inner.body @ [linek])
              else if starts_with_keyword lowk "end for" || starts_with_keyword lowk "end while" || starts_with_keyword lowk "end do" then
                if !nest = 0 then (break := true; raise Exit) else (nest := !nest - 1; inner.body <- inner.body @ [linek])
              else inner.body <- inner.body @ [linek];
              incr k
            done;
            (* execute inner loop *)
            run_loop inner;
            j := !k + 1; loop j
          end
        else
          (* declarations, assignment, print, or just increment *)
          begin
            if String.contains s ':' && String.contains s '=' then
              (store_variable s; loop (j+1))
            else if String.contains s '=' && (not (String.contains s '=' && String.contains s '=')) then
              (handle_assignment s; loop (j+1))
            else if starts_with_keyword s "print" then
              let rest = trim (if String.length s >= 6 then String.sub s 5 (String.length s - 5) else "") in
              let rest =
                if String.length rest > 0 && rest.[0] = ',' then lstrip_ws (String.sub rest 1 (String.length rest - 1)) else rest
              in
              do_print rest; loop (j+1)
            else loop (j+1)
          end
    in
    try loop 0 with Exit -> ()
  in
  process_lines body
let handle_if line =
  let cond = strip_ctrl_tail (String.sub line 2 (String.length line - 2)) in
  let r = eval_bool_expr cond in
  if_active := !if_active @ [r];
  if_matched := !if_matched @ [r];
  dbg ("IF -> " ^ bool_str r)
let handle_elif line =
  if List.length !if_active = 0 then ()
  else
    let last_matched = List.nth !if_matched ((List.length !if_matched) - 1) in
    if last_matched then
      (* previous matched; disable active *)
      if_active := (List.rev (List.tl (List.rev !if_active)))
    else
      let cond = strip_ctrl_tail (String.sub line 4 (String.length line - 4)) in
      let r = eval_bool_expr cond in
      (* replace top of stacks *)
      let n = List.length !if_active - 1 in
      let new_active = (List.rev (List.tl (List.rev !if_active))) @ [r] in
      let new_matched = (List.rev (List.tl (List.rev !if_matched))) @ [ (List.nth !if_matched n) || r ] in
      if_active := new_active; if_matched := new_matched;
      dbg ("ELIF -> " ^ bool_str r)
let handle_else () =
  if List.length !if_active = 0 then ()
  else
    let n = List.length !if_active - 1 in
    let matched = List.nth !if_matched n in
    if not matched then
      begin
        if_active := (List.rev (List.tl (List.rev !if_active))) @ [true];
        if_matched := (List.rev (List.tl (List.rev !if_matched))) @ [true];
      end
    else
      if_active := (List.rev (List.tl (List.rev !if_active))) @ [false];
  dbg ("ELSE active=" ^ (bool_str (List.nth !if_active ((List.length !if_active) - 1))))
let handle_end_if () =
  if List.length !if_active > 0 then begin if_active := (List.rev (List.tl (List.rev !if_active))); if_matched := (List.rev (List.tl (List.rev !if_matched))) end;
  dbg "END IF"
and parse_line cmd =
  let l = lstrip_ws (normalize_html cmd) in
  (* If capturing a loop body *)
  if List.length !loops > 0 then
    begin
      let top = List.nth !loops ((List.length !loops) - 1) in
      if starts_with_keyword l "for" || starts_with_keyword l "while" || starts_with_keyword l "dowhile" || starts_with_keyword l "do while" then
        (top.nest <- top.nest + 1; append_to_current_body l; ())
      else if starts_with_keyword l "end for" || starts_with_keyword l "end while" || starts_with_keyword l "end do" then
        if top.nest > 0 then (top.nest <- top.nest - 1; append_to_current_body l)
        else
          begin
            (* run loop and pop *)
            run_loop top;
            loops := (List.rev (List.tl (List.rev !loops)))
          end
      else append_to_current_body l
    end
  else
    begin
      (* IF blocks (top-level gating only) *)
      if starts_with_keyword l "if" then (handle_if l; ())
      else if starts_with_keyword l "elif" then (handle_elif l; ())
      else if starts_with_keyword l "else" then (handle_else (); ())
      else if starts_with_keyword l "end if" then (handle_end_if (); ())
      else
        begin
          (* if an IF gating is active and false, skip top-level commands *)
          if List.length !if_active > 0 && (not (List.nth !if_active ((List.length !if_active)-1))) then ()
          else
            (* LOOP starts *)
            if starts_with_keyword l "for" || starts_with_keyword l "while" || starts_with_keyword l "dowhile" || starts_with_keyword l "do while" then
              handle_loop_start l
            else if starts_with_keyword l "end for" || starts_with_keyword l "end while" || starts_with_keyword l "end do" then ()
            else if (String.contains l ':' && String.contains l '=') then store_variable l
            else if String.contains l '=' && (not (String.contains l '=' && String.contains l '=')) then handle_assignment l
            else if starts_with_keyword l "print" then
              let rest = if String.length l >= 6 then String.sub l 5 (String.length l - 5) else "" in
              let rest = if String.length rest > 0 && rest.[0] = ',' then lstrip_ws (String.sub rest 1 (String.length rest - 1)) else rest in
              do_print rest
            else ()  (* ignore unknown *)
        end
    end
let handle_end_prog () =
  printf "Program %s ended.\n" (trim !current_prog_name);
  printf "Output buffer:\n";
  flush_output ();
  (* reset state for next program (REPL stays) *)
  VarTable.reset ();
  outbuf := []; outcount := 0;
  if_active := []; if_matched := [];
  loops := []; in_prog := false; current_prog_name := ""
let () =
  (* init *)
  VarTable.reset ();
  outbuf := []; outcount := 0;
  if_active := []; if_matched := [];
  loops := [];
  in_prog := false;
  current_prog_name := "";
  let prompt_main = ">>> " and prompt_cont = "... " in
  try
    while true do
      let depth = (List.length !loops) + (List.length !if_active) in
      if depth > 0 then
        (print_string prompt_cont; for _ = 1 to depth-1 do print_string "    " done)
      else print_string prompt_main;
      flush stdout;
      let line = read_line () in
      let line = normalize_html line |> strip_any_prompt |> lstrip_ws in
      if starts_with_keyword line "prog" then
        (current_prog_name := trim (extract_name_after_keyword line "prog"); in_prog := true; ())
      else if starts_with_keyword line "end prog" then
        (handle_end_prog (); raise Exit)
      else if not !in_prog then ()
      else if String.length (trim line) = 0 then ()
      else parse_line line
    done
  with End_of_file ->
    if !in_prog then handle_end_prog ();
    ()
  | Exit -> ()
