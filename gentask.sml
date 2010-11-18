(* 1�s/1�p�����[�^�E�Z�b�g�̌`���Ń^�X�N���X�g�𐶐��B*)
structure GenTask = struct
  val gamma = Type.gamma
  fun setConf ([p1,p2,p3,p4], mcid)  =
    {betaNPark  = 0.5 * gamma
    ,betaNHome  = p1  * gamma
    ,betaNSuper = p2  * gamma
    ,betaNSch   = p3  * gamma
    ,betaNCorp  = p3  * gamma
    ,betaNTrain = p4  * gamma
    ,e0_JOJ     = 30
    ,nPop       = 3000
    ,tag  = String.concatWith "_" (map Alice.sR [p1,p2,p3,p4])
    ,mcid = let open StringCvt in padLeft #"0" 3 (Int.fmt DEC mcid) end
    }
    | setConf _ = (print "# of elements don't agree!, Isn't is bug?\n"; raise Bind)

  (* �d���g�ݍ��킹�𖇋����� *)
  fun enumHP (nil, _) = nil
    | enumHP (set, 0) = nil
    | enumHP (set, 1) = map (fn x => [x]) set
    | enumHP (set, m) = 
    map (fn rest => hd set :: rest) (enumHP (set, m - 1)) @
    enumHP (tl set, m)
 
  (* �����e�J�����̕������������� *)
  fun dup (xs,n) = let
    fun dd 0 x = nil
      | dd n x = x :: dd (n-1) x
  in
    List.concat (map (dd n) xs)
  end

  (* ���[��1: �P���ȏd���g�ݍ��킹�Ƃ��� *)
  val rrSet = [1.2, 1.5, 1.8, 3.0]
  (*fun gen1 (set,m) = map setConf (enumHP(set,m)) *)
  fun gen {set, nDup} = let 
    val m = 4 (* setConf�̈��� *)
    val zs = enumHP(set,m)
    val ys = List.concat (map (fn z => List.tabulate(nDup, fn mcid => (z,mcid))) zs)
  in
    map setConf ys
  end
 
  (* ���[��2: �S���̂݋������p�����[�^���l���� *)
  val rrSet2 = [1.2, 1.5, 1.8]
  val rrTr = [1.8, 2.0, 3.0]
  fun gen2 {setO = set1, setTr = set2, nDup} = let 
    val m  = 3 (* setConf�̈��� - 1 *)
    val xs = enumHP(set1,m)
    val zs = List.concat (map (fn x => map (fn y => x @ [y]) set2) xs) 
    val ys = List.concat (map (fn z => List.tabulate(nDup, fn mcid => (z,mcid))) zs)
  in
    map setConf ys
  end
end
