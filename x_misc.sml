structure X_Misc = struct
  (* ガウス乱数 *)
local
  open Alice
  open EasyPrint; infix 1 <<
  fun op <> (x,y) = x y; infix 1 <>;
  val op $ = Vector.sub; infix 9 $;
in
  fun save (i,x) = let val os = BinIO.openOut (sI i) in (BinIO.output(os,Unsafe.blastWrite x); BinIO.closeOut os) end ;
  fun load i = let val is = BinIO.openIn (sI i) in Unsafe.blastRead (BinIO.input is before BinIO.closeIn is) end;

  fun splitR x = let val M = 67108864.0 val a = (x + M) - M val b = x - a in (a,b) end ;

  fun joinR (a,b) = let val (a,b) = ((#1 o splitR) a, b) in a + b end;

  fun sRR (a,b) = sR a ^":"^sR b
  (* 乱数 *)
  val saveCnt = ref 0
  fun getcnt() = !saveCnt before saveCnt := !saveCnt + 1
  fun rgauss rnd = let
    val u1 = Random.randReal rnd
    val u2 = Random.randReal rnd

    fun chk x = 
      if Real.isNan x then 
        (print ("rgauss: "^sRR (splitR u1) ^ " " ^ sRR (splitR u2)^ "\n")
        ; save (getcnt(), #[x,u1,u2])
        ; sqrt(~2.0*ln u1)*cos(2.0*pi*u2)
        )
      else x
  in
    chk(  sqrt(~2.0*ln u1)*cos(2.0*pi*u2) )
  end

  fun irndIn rnd (x,y) = 
    x + (Random.randInt rnd) mod (y - x)  

  fun rndSel rnd p (x,y) = 
    if (Random.randReal rnd < p) 
      then x
      else y

  fun rndSelV rnd v =
    Vector.sub(v, Int.mod (Random.randInt rnd, Vector.length v))

  fun rndSelL rnd l = let
    val j = Int.mod (Random.randInt rnd, length l)
    val i = ref 0
  in
    valOf o List.find (fn _ => !i = j before i := !i + 1) <> l
  end

  fun rndSelLP rnd (x::xs: (real * 'a) list) = let
    val u = Random.randReal rnd
    fun loop (_ ,y,nil) = #2 y
      | loop (cw,y,x::xs) = 
      if (cw > u) then #2 y else loop (cw + #1 x, x, xs)
    and trick() = loop (#1 x, x, xs)
  in
    trick()
  end

  (* パレート分布に従う人員規模をつくり出すための場所indexのランダム抽出 *)
  fun paretoSel {alpha, gamma, n, m, beta} = let
    open Alice; infix 9 *^;
    val n  = Real.max(0.1,n - 1.0) (* trick to get n + m - 1 as the maximal index *)
    val Q1 = 1.0 - (alpha/gamma)*^beta
    val P1 = alpha*beta/(beta - 1.0)*(1.0 - (alpha/gamma)*^(beta - 1.0))
    val P2 = P1 + gamma*Q1*m/n
    fun u rnd = P2*Random.randReal rnd
    fun clip x = Real.round x  handle s => raise s

  in
    fn rnd => let
      val u = u rnd
      val z = 
        if u < P1 
        then 1.0 - (1.0 - (beta - 1.0)*u/alpha/beta)*^(beta/(beta-1.0))
        else Q1 + (u - P1)/gamma
    in
      clip (n*z/Q1)
    end
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

  (* ディレクリ区切りを含むパスを受けとり、中間パスが存在しなければ、これもつくる *)
  fun mkDir path = let
    val org = OS.FileSys.getDir()
    val arcs = CSV.split {spcs="/\\", seps=""} path
    fun dig (x::xs) = 
      (OS.FileSys.mkDir x handle _ => (); OS.FileSys.chDir x; dig xs; OS.FileSys.chDir "../")
      | dig nil = ()
  in
    (dig arcs; OS.FileSys.chDir org)
  end

  (* ディレクリ区切りを含むパスを受けとり、中間パスが存在しなければ
   * これを作ったうえで、指定パスを新規ファイルとして作成する *)
  fun openOutDig path = let
    open OS.Path
    val cleanup = mkCanonical o fromUnixPath o toUnixPath
    val path = cleanup path
  in
    (mkDir (dir path); TextIO.openOut path)
  end

  fun readF s = let
    val is = TextIO.openIn s
    fun loop () = 
      case TextIO.inputLine is
        of SOME x => x :: loop ()
         | NONE   => nil
  in
    loop () before TextIO.closeIn is
  end
end
end
