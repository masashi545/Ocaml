(* 
Syntax...データ構造 
*)
type id = string (* 変数の型 *)

type binOp = Plus | Minus | Mult | Eq | Lt (* 二項演算子 *)

type exp =
      Var of id (* 変数 *)
    | ILit of int (* 整数値 *)
    | BLit of bool (* 真偽値 *)
    | BinOp of binOp * exp * exp (* 二項演算式 *)
    | LetExp of id * exp * exp (*　Let式 *)
    | IfExp of exp * exp * exp (* 条件式 *)

type program =
    Exp of exp

(*
Environment...環境
*)
type 'a t = (id * 'a) list (* 環境の型 *)

exception Not_bound (* 環境に変数の情報がないときに投げられる例外 *)

let empty = [] (* 空の環境 *)
let extend x v env = (x,v)::env (* 環境envに新しい変数の束縛情報(x,v)を追加する関数 *)
let rec lookup x env = (* 環境envから、変数xの束縛情報を探す関数 *)
    try List.assoc x env with Not_found -> raise Not_bound (* あれば束縛している値vを、なければ例外を、返す *)

(*
Eval...解釈部
*)
type exval = (* 式の値の型 *)
      IntV of int
    | BoolV of bool
and dnval = exval (* 変数が指す値の型 *)

exception Error (* 未定義動作のときに投げられる例外 *)
let err = raise Error

let rec apply_prim op arg1 arg2 = match op, arg1, arg2 with (* 二項演算式を評価する関数 *)
      Plus, IntV i1, IntV i2 -> IntV (i1 + i2)
    | Plus, _, _ -> err
    | Minus, IntV i1, IntV i2 -> IntV (i1 - i2)
    | Minus, _, _ -> err
    | Mult, IntV i1, IntV i2 -> IntV (i1 * i2)
    | Mult, _, _ -> err
    | Eq, IntV i1, IntV i2 -> BoolV (i1 = i2)
    | Eq, _, _ -> err
    | Lt, IntV i1, IntV i2 -> BoolV (i1 <= i2)
    | Lt, _, _ -> err

let rec eval_exp env = function (* 式を評価する関数 *)
      Var x  -> (try lookup x env with Not_bound -> err)
    | ILit i -> IntV i (* 整数値の評価 *)
    | BLit b -> Bool b (* 真偽値の評価 *)
    | BinOp (op, exp1, exp2) -> (* 二項演算式の評価 *)
        let arg1 = eval_exp env exp1 in 
        let arg2 = eval_exp env exp2 in
        apply_prim op arg1 arg2
    | LetExp (id, exp1, exp2) -> (* Let式の評価 *)
        let value = eval_exp env exp1 in 
        eval_exp (extend id value env) exp2
    | IfExp (exp, exp1, exp2) -> (* 条件式の評価 *)
        let test = eval_exp env exp in 
        (match test with
            BoolV true -> eval_exp env exp1
          | BoolV false -> eval_exp env exp2
          | _ -> err)