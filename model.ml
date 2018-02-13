(* op_t : ２項演算子 *)
type op_t = Plus | Minus | Times | Divided | Equal | NotEqual | Less | LessEqual

(* sy_t : 構文 *)
type sy_t = Number of int
          | Bool of bool
          | Op of sy_t * op_t * sy_t
          | If of sy_t * sy_t * sy_t
          | Variable of string
          | Let of string * sy_t * sy_t
          | LetRec of string * string * sy_t * sy_t
          | Fun of string * sy_t
          | App of sy_t * sy_t
          | Match of sy_t * sy_t * string * string * sy_t
          | Try of sy_t * string * sy_t
          | Empty
          | Cons of sy_t * sy_t
          | Raise of sy_t
          | Ref of sy_t
          | Exclamation of sy_t
          | Substitute of string * sy_t

(* env_t : 環境 *)
type ('a, 'b) env_t = ('a * 'b) list

(* vl_t : 値 *)
type vl_t = VNumber of int
          | VBool of bool
          | VFun of string * sy_t * (string, vl_t) env_t
          | VRecFun of string * string * sy_t * (string, vl_t) env_t
          | VEmpty
          | VCons of vl_t * vl_t
          | VRaise of vl_t
          | VRef of vl_t

let empty = []

(* 環境から変数の値を返す *)
let rec get env var = match env with
    [] -> raise Not_found
  | (f_var, f_value) :: rest ->
     if var = f_var
     then f_value
     else get rest var

(* 環境の追加 *)     
let extend env var value = (var, value) :: env
         
let env' = ref (extend empty "" (VNumber 0))

(* 演算子を文字列に変換する *)
let op_to_string op = match op with
    Plus -> " + "
  | Minus -> " - "
  | Times -> " * "
  | Divided -> " / "
  | Equal -> " = "
  | NotEqual -> " <> "
  | Less -> " < "
  | LessEqual -> " <= "

(* 構文を文字列に変換する *)
let rec sy_to_string exp = match exp with
  | Number (n) -> string_of_int n
  | Bool (b) -> if b then "true" else "false"
  | Op (arg1, op, arg2) ->
     "(" ^ sy_to_string arg1
     ^ op_to_string op
     ^ sy_to_string arg2 ^ ")"
  | If (arg0, arg1, arg2) ->
     "if " ^ sy_to_string arg0
     ^ " then " ^ sy_to_string arg1
     ^ " else " ^ sy_to_string arg2
  | Variable (v) -> v
  | Let (x, arg1, arg2) ->
     "let " ^ x ^ " = "
     ^ sy_to_string arg1 ^ " in "
     ^ sy_to_string arg2
  | LetRec (x, n, arg1, arg2) ->
       "let rec " ^ x ^ " " ^ n ^ " = "
       ^ sy_to_string arg1 ^ " in "
       ^ sy_to_string arg2
  | Fun (x, arg) ->
     "fun " ^ x ^ " -> " ^ sy_to_string arg
  | App (arg1, arg2) ->
     "(" ^ sy_to_string arg1 ^ ") " ^ sy_to_string arg2
  | Empty -> "[]"
  | Cons (first, rest) -> sy_to_string first ^ " :: " ^ sy_to_string rest
  | Match (arg0, arg1, first, rest, arg2) ->
     "match " ^ sy_to_string arg0 ^ " with [] -> " ^ sy_to_string arg1 ^ " | "
     ^ first ^ " :: " ^ rest ^ " -> " ^ sy_to_string arg2
  | Try (arg1, exn, arg2) ->
     "try " ^ sy_to_string arg1 ^ " with \n"
     ^ "Error " ^ exn ^ " -> " ^sy_to_string arg1
  | Raise (exn) -> "raise (Error" ^ sy_to_string exn ^ ")"
  | Ref (t) -> "ref " ^ sy_to_string t
  | Exclamation (t) -> "!" ^ sy_to_string t
  | Substitute (v, arg) ->
     v ^ " := " ^ sy_to_string arg

(* 値を文字列に変換する *)
let rec vl_to_string value = match value with
  | VNumber (n) -> string_of_int n
  | VBool (b) -> string_of_bool b
  | VFun (_) -> "<fun>"
  | VRecFun (_) -> "<rec>"
  | VEmpty -> "[]"
  | VCons (first, rest) -> vl_to_string first ^ " :: " ^ vl_to_string rest
  | VRaise (exn) -> "Error " ^ vl_to_string exn
  | VRef (v) -> "ref " ^ vl_to_string v


(* メイン関数 *)
let rec f expr env =
 
  match expr with
  | Number (n) -> VNumber (n)
  | Bool (b) -> VBool (b)
  | Op (Number n1, Plus, Number n2) -> VNumber (n1 + n2)
  | Op (Number n1, Minus, Number n2) -> VNumber (n1 - n2)
  | Op (Number n1, Times, Number n2) -> VNumber (n1 * n2)
  | Op (Number n1, Divided, Number n2) ->
     if n2 = 0 then (VRaise (VNumber 0)) else VNumber (n1 / n2)
  | Op (Number n1, Equal, Number n2) -> VBool (n1 = n2)
  | Op (Number n1, Less, Number n2) -> VBool (n1 < n2)
  | Op (Number n1, NotEqual, Number n2) -> VBool (n1 <> n2)
  | Op (Number n1, LessEqual, Number n2) -> VBool (n1 <= n2)
  | Op (arg1, op, arg2) ->
     let v1 = f arg1 env in
     let v2 = f arg2 env in
     begin
       match (v1, v2) with
       | (VRaise _, _) -> v1
       | (_, VRaise _) -> v2
       | (VNumber n1, VNumber n2) ->
	  f (Op (Number n1, op, Number n2)) !env'
       | _ ->
	  failwith (
	      "Bad arguments to "
	      ^  (if op = Plus then "+"
		  else if op = Minus then "-"
		  else if op = Times then "*"
		  else if op = Divided then "/"
		  else if op = Equal then "="
		  else if op = Less then "<"
		  else if op = NotEqual then "<>"
		  else "<=") ^ ": "
	      ^ vl_to_string v1 ^ ", " ^ vl_to_string v2
	    )
     end
  | If (arg0, arg1, arg2) ->
     let v0 = f arg0 env in
     begin
       match v0 with
       | VBool (true) -> (f arg1 env)
       | VBool (false) -> (f arg2 env)
       | _ -> failwith ("Bad argument to if: " ^
			  vl_to_string v0)
     end
  | Variable (v) ->
     (try get env v
      with Not_found -> failwith ("Unbound variable: " ^ v))
  | Let (x, arg1, arg2) ->
     let v = f arg1 env in
     let env'' = extend env x v in
     begin
       match v with
       | VRaise _ -> v
       | _ -> f arg2 env''
     end
  | Fun (x, arg) -> VFun (x, arg, env)
  | LetRec (x, n, arg1, arg2) ->
     let v = VRecFun (x, n, arg1, env) in
     let env'' = (env' := extend env x v; !env') in
     f arg2 env''
  | App (arg1, arg2) ->
     let fv = f arg1 env in
     begin
       match fv with
       | VFun (v, arg', env'') ->
	  let a2' = f arg2 env in
	  f arg' (extend env'' v a2')
       | VRecFun (v, n, arg', env'') ->
	  let a2' = f arg2 env in
	  let env''' = extend env'' n a2' in
	  f arg' (extend env''' v fv)
       |  _-> failwith ("Not a function: " ^
			  vl_to_string fv)
     end
  | Empty -> VEmpty
  | Cons (first, rest) ->
     let first' = f first env in
     let rest' = f rest env in
     begin
       match rest' with
       | VEmpty | VCons _ -> VCons (first', rest')
       | _ -> failwith ("Not a list: " ^ vl_to_string rest')
     end
  | Match (arg0, arg1, first, rest, arg2) ->
     let v = f arg0 env in
     begin
       match v with
       | VEmpty -> f arg1 env
       | VCons (first', rest') ->
          f arg2 (extend (extend env first first') rest rest')
       | _ ->
	  failwith ("Not a list: " ^ vl_to_string v)
     end
  | Raise (exn) ->
     let exn' = f exn env in
     VRaise (exn')
  | Try (arg1, exn, arg2) ->
     let v = f arg1 env in
     begin
       match v with
       | VRaise e' -> f arg2 (extend env exn e')
       | _ -> v
     end
  | Ref (arg) ->
     let arg' = f arg env in
     VRef (arg')
  | Exclamation (arg) ->
     let arg' = f arg env in
     begin
       match arg' with
       | VRef v -> v
       | _ -> failwith ("Not a location: " ^ vl_to_string arg')
     end
  | Substitute (v, arg) ->
     let arg' = f arg env in
     f arg (env' := extend env v (VRef arg'); !env')
