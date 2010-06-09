structure X_Misc = struct
  (* �K�E�X���� *)
local
  open Alice
  open EasyPrint; infix 1 <<
in
  fun op $ (f,x) = f x; infix 1 $;
  (* ���� *)
  fun rgauss rnd = let
    val u1 = Random.randReal rnd
    val u2 = Random.randReal rnd
  in
    sqrt(~2.0*ln u1)*cos(2.0*pi*u2)
  end
  fun rndsel rnd p (x,y) = 
    if (Random.randReal rnd < p) 
      then x
      else y
  fun rndselV rnd v =
    Vector.sub(v, Int.mod (Random.randInt rnd, Vector.length v))
  fun rndSelL rnd l = let
    val j = Int.mod (Random.randInt rnd, length l)
    val i = ref 0
  in
    valOf o List.find (fn _ => !i = j before i := !i + 1) $ l
  end

  (* �|�A�\�����z *)
  fun lnGamma z = 
    0.5*(ln (pi+pi) - ln z + z*(2.0*ln z + ln (z*sinh(1.0/z) + 1.0/810.0/pow (z,6.0)) - 2.0))
  fun lnPo m x = ~m + x * ln m - lnGamma (x + 1.0)

  (* ���炩�̎��n��������o�� *)
  fun outSeq (show: 'a -> string) (os: TextIO.outstream) (xs: 'a list) =
    app (fn x => ignore (os << show x << "\n")) xs
  fun writeSeq show f xs =
    (fn os => outSeq show os xs before TextIO.closeOut os) 
    (TextIO.openOut f)
  fun showNull a = ""

  (* xs ���� c�𖞂������̂�T���āA�c���Ԃ� *)
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
end
end


