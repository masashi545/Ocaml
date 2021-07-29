(* a *)
type node =
      Const of int (* 整数定数 *)
    | Var of string (* 変数 *)
​
type tree =
      Leaf of node
    | BranchS of {left: tree; right: tree}
    | BranchM of {left: tree; right: tree}
​
let t1 = BranchM {left=Leaf (Const 4); right=Leaf (Var "x")}
let t2 = BranchS {left=t1;           right=Leaf (Const 3)}
​
​
(* b *)
let rec simpl0 t = match t with
      Leaf _ -> t
    | BranchS {left=l; right=r} -> 
        (let ll = simpl0 l in
        let rr = simpl0 r in
        match ll, rr with
          Leaf (Const 0), _ -> rr
        | _, Leaf (Const 0) -> ll
        | _, _ -> BranchS {left=ll; right=rr})
    | BranchM {left=l; right=r} -> 
        (let ll = simpl0 l in
        let rr = simpl0 r in
        match ll, rr with
          Leaf (Const 0), _ -> Leaf (Const 0)
        | _, Leaf (Const 0) -> Leaf (Const 0)
        | _, _ -> BranchM {left=ll; right=rr})
​
let rec string_of_tree t = match t with
      Leaf n -> (match n with
          Const i -> string_of_int i
        | Var x -> x)
    | BranchS {left=l; right=r} -> string_of_tree l ^ "+" ^ string_of_tree r
    | BranchM {left=l; right=r} -> string_of_tree l ^ "*" ^ string_of_tree r
​
let main t = string_of_tree (simpl0 t)
​
let t3 = BranchM {left=t2; right=BranchM {left=Leaf(Const 0); right=t1}}