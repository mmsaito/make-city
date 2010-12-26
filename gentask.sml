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
    ,infectID     = 30
    ,nPop       = 3000
    ,tag  = String.concatWith "_" (map Real.toString [p1,p2,p3,p4])
    ,mcid = let open StringCvt in padLeft #"0" 3 (Int.fmt DEC mcid) end
    }
    | setConf _ = 
      (print "The length of each list should be 4!\n"
      ;raise Bind
      )

  (* �d���g�ݍ��킹�𖇋����� *)
  fun enumHP (nil, _) = nil
    | enumHP (set, 0) = nil
    | enumHP (set, 1) = map (fn x => [x]) set
    | enumHP (set, m) = 
    map (fn rest => hd set :: rest) (enumHP (set, m - 1)) @
    enumHP (tl set, m)

  (* �f�J���g�� *)
  fun prodSet (xs::xss) = 
    List.concat (map (fn x => map (fn ys => x :: ys) (prodSet xss)) xs)
    | prodSet nil = [nil]

  (* �����e�J�����̕������������� *)
  fun dup (n,xs) = let
    fun dd 0 x = nil
      | dd n x = x :: dd (n-1) x
  in
    List.concat (map (dd n) xs)
  end

  fun dup' (n, conf:Trivial.conf) = let
    fun set (x:Trivial.conf) mcid  =
    {betaNPark  = #betaNPark x
    ,betaNHome  = #betaNHome x
    ,betaNSuper = #betaNSuper x
    ,betaNSch   = #betaNSch x
    ,betaNCorp  = #betaNCorp x
    ,betaNTrain = #betaNTrain x
    ,infectRule = #infectRule x
    ,nPop       = #nPop x
    ,tag        = #tag x    
    ,vacEff     = #vacEff x
    ,vacTrCover = #vacTrCover x
    ,vacSchCover= #vacSchCover x
    ,mcid       = Int.toString mcid
    }: Trivial.conf
  in
    List.tabulate(n, set conf)
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

  (* ���[��2: �Ƃ̂ق����ނ���傫���A��ЂƂ̊֌W�͂ǂ���Ƃ������Ȃ� *)
  fun gen3 {setO = s1, setTr = s2, nDup} = let
    open List
    val ss = prodSet [s1, s1, s1, s2] 
    val ss'= filter (fn [hm,sp,cr,tr] => sp <= hm andalso sp <= cr) ss
  in
    concat (map (fn par => tabulate(nDup, fn id => setConf(par, id))) ss')
  end

end


