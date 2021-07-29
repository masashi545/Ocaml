(*
データ構造
*)
type tree =
      Lf (* Leaf *)
    | Br of { (* Branch *)
        left: tree;
        value: int;
        right: tree
    }

(*
関数
*)
let rec find(t, n) = match t with (* 探索 *)
      Lf -> false
    | Br {left=l; value=v; right=r} ->
        if n = v then true
        else if n < v then find(l, n)
        else (* n > v *)   fing(r, n)

let rec insert(t, n) = match t with (* 挿入 *)
      Lf -> Br {left=Lf; value=n; right=Lf}
    | Br {left=l; value=v; right=r} ->
        if n = v then t
        else if n < v then Br {left=insert(l, n); value=n; right=r}
        else (* n > v *)   Br {left=l;            value=n; right=insert(r, n)}

let rec min t = match t with (* 木の最小値を求める関数 *)
      Lf -> -255
    | Br {left=Lf; value=v; right=_} -> v
    | Br {left=l;  value=_; right=_} -> min left

let rec delete(t, n) = match t with (* 削除 *)
      Lf -> t
    | Br {left=l; value=v; right=r} ->
        if n = v then match l, r with
              Lf, Lf -> Lf
            | Br _, Lf -> l
            | Lf, Br _ -> r
            | Br _, Br _ -> let m = min r in Br {left=l; value=m; right=delete(r, m)}
        else if n < v then Br{left=delete(l, n); value=v; right=r}
        else (* n < v *)   Br{left=l;            value=v; right=delete(r, n)}
