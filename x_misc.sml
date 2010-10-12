fun rndSelLP rnd (x::xs: (real * 'a) list) = let
    val u = Random.randReal rnd
    fun loop (_ ,y,nil) = #2 y
      | loop (cw,y,x::xs) = 
      if (cw > u) then #2 y else loop (cw + #1 x, x, xs)
    and trick() = loop (#1 x, x, xs)
  in
    trick()
  end
  (* テストコード
    datatype tag = C1 | C2 | C3 | C4
    val prL = [(0.1,C1),(0.4,C4),(0.2,C2),(0.3,C3)]
    val xs = List.tabulate(100000, fn _ => X_Misc.rndSelLP rnd prL)

    val c1 = List.filter (fn c => c = C1) xs
    val c2 = List.filter (fn c => c = C2) xs
    val c3 = List.filter (fn c => c = C3) xs
    val c4 = List.filter (fn c => c = C4) xs
  *)

  (* ポアソン分布 *)
  fun rpoisson rnd lambda = let
    val s = ref (exp lambda)
    val n = ref 0
  in
    (while (!s > 1.0) do
      (s := !s * Random.randReal rnd
      ;n := !n + 1
(*
      ;if !n > 200 then 
        (print (csvList "," (fG 16) [lambda,!s] ^ "\n")
        ;TextIO.input1 (TextIO.stdIn);())
       else ()
*)
      )
    ;!n
    )
  end
  fun rpoisson' rnd lambda = Real.fromInt (rpoisson rnd lambda)

  fun lnGamma z = 
    0.5*(ln (pi+pi) - ln z + z*(2.0*ln z + ln (z*sinh(1.0/z) + 1.0/810.0/pow (z,6.0)) - 2.0))
  fun lnPo m x = ~m + x * ln m - lnGamma (x + 1.0)

  (* 何らかの時系列を書き出す *)
  fun outSeq (show: 'a -> string) (os: TextIO.outstream) (xs: 'a list) =
    app (fn x => ignore (os << show x << "\n")) xs
  fun writeSeq show f xs =
    (fn os => outSeq show os xs before TextIO.closeOut os) 
    (TextIO.openOut f)
  fun showNull a = ""

  (* xs から cを満たすものを探して、残りを返す *)
  fun findpop c xs = let
    fun lp (acc,x::xs) = 
      if (c x) then (acc,x::xs)
      else lp (x::acc, xs)
      | lp (acc, nil) = (acc,nil)
  in
    case lp (nil,xs) 
      of (rs,x::xs) => (SOME x, List.revAppend (rs,xs))
       | (rs,nil  ) => (NONE  , xs)
  end

  (* リストから n番目の要素を取り出す *)
  fun popAt(x::xs,0) = (x,xs)
    | popAt(x::xs,n) = (fn (y,ys) => (y, x::ys)) (popAt(xs,n-1))

  (* 非復元サンプル *)
  fun uniqRndSample rnd seq n =
    Iterator.applyN (fn (xs,ys) => (fn (z,zs) => (z::xs,zs)) 
      (popAt(ys, Random.randInt rnd mod (length ys)))) n (nil,seq)
end
end
