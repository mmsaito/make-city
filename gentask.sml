(* 1行/1パラメータ・セットの形式でタスクリストを生成。*)
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

  (* 重複組み合わせを枚挙する *)
  fun enumHP (nil, _) = nil
    | enumHP (set, 0) = nil
    | enumHP (set, 1) = map (fn x => [x]) set
    | enumHP (set, m) = 
    map (fn rest => hd set :: rest) (enumHP (set, m - 1)) @
    enumHP (tl set, m)

  (* デカルト積 *)
  fun prodSet (xs::xss) = 
    List.concat (map (fn x => map (fn ys => x :: ys) (prodSet xss)) xs)
    | prodSet nil = [nil]

  (* モンテカルロの分だけ複製する *)
  fun dup (n,xs) = let
    fun dd 0 x = nil
      | dd n x = x :: dd (n-1) x
  in
    List.concat (map (dd n) xs)
  end

  fun setMcid (x:Trivial.conf) mcid  =
  {betaNPark  = #betaNPark x
  ,betaNHome  = #betaNHome x
  ,betaNSuper = #betaNSuper x
  ,betaNSch   = #betaNSch x
  ,betaNCorp  = #betaNCorp x
  ,betaNTrain = #betaNTrain x
  ,infectRule = #infectRule x
  ,nPop       = #nPop x
  ,nPlaces    = #nPlaces x
  ,tag        = #tag x    
  ,vacEff     = #vacEff x
  ,vacTrCover = #vacTrCover x
  ,vacSchCover= #vacSchCover x
  ,mcid       = Int.toString mcid
  }: Trivial.conf

  fun dup' (n, conf:Trivial.conf) = List.tabulate(n, setMcid conf)

  val mapConcat = (fn f => List.concat o map f) 

  (* ルール1: 単純な重複組み合わせとする *)
  val rrSet = [1.2, 1.5, 1.8, 3.0]
  (*fun gen1 (set,m) = map setConf (enumHP(set,m)) *)
  fun gen {set, nDup} = let 
    val m = 4 (* setConfの引数 *)
    val zs = enumHP(set,m)
    val ys = List.concat (map (fn z => List.tabulate(nDup, fn mcid => (z,mcid))) zs)
  in
    map setConf ys
  end
 
  (* ルール2: 鉄道のみ許されるパラメータを考える *)
  val rrSet2 = [1.2, 1.5, 1.8]
  val rrTr = [1.8, 2.0, 3.0]
  fun gen2 {setO = set1, setTr = set2, nDup} = let 
    val m  = 3 (* setConfの引数 - 1 *)
    val xs = enumHP(set1,m)
    val zs = List.concat (map (fn x => map (fn y => x @ [y]) set2) xs) 
    val ys = List.concat (map (fn z => List.tabulate(nDup, fn mcid => (z,mcid))) zs)
  in
    map setConf ys
  end

  (* ルール2: 家のほうがむしろ大きい、会社との関係はどちらともいえない *)
  fun gen3 {setO = s1, setTr = s2, nDup} = let
    open List
    val ss = prodSet [s1, s1, s1, s2] 
    val ss'= filter (fn [hm,sp,cr,tr] => sp <= hm andalso sp <= cr) ss
  in
    concat (map (fn par => tabulate(nDup, fn id => setConf(par, id))) ss')
  end
end

(* タスクセット1
  * ワクチン接種の最初の実験
  * 20000モンテカルロ実験
*)
structure Tasks1 = struct
  open GenTask
  open Type
  val novac = {vacEff = 0.0, vacTrCover = 0.0, vacSchCover = 0.0}
  val nPlaces = 
    #[{sch = 2, corp = 4, cram = 0, super = 10, park = 2}
     ,{sch = 2, corp = 8, cram = 1, super = 10, park = 2}
     ,{sch = 2, corp = 4, cram = 0, super = 10, park = 2}
     ,{sch = 3, corp =30, cram = 1, super = 10, park = 2}
     ,{sch = 2, corp =30, cram = 0, super = 10, park = 2}
     ]

  val reproNum3 = {super = 0.3, park = 0.5, home = 1.5, corp = 4.0, school = 5.0,train = 6.0}
  val reproNum4 = {super = 0.3, park = 0.5, home = 1.5, corp = 3.0, school = 4.0,train = 6.0}

  fun genConf
    ({super, park, home, corp, school, train})
    ({vacEff:real, vacTrCover:real, vacSchCover:real})
    (infectRule:{tag:string,n:int,rule:belongSpec, isRandom:bool})
    (mcid: string option)
  = {betaNSuper = super  * Type.gamma
    ,betaNPark  = park   * Type.gamma
    ,betaNHome  = home   * Type.gamma
    ,betaNCorp  = corp   * Type.gamma
    ,betaNSch   = school * Type.gamma
    ,betaNTrain = train  * Type.gamma
    ,infectRule = infectRule
    ,vacEff     = vacEff
    ,vacTrCover = vacTrCover
    ,vacSchCover= vacSchCover
    ,nPop       = 3000
    ,nPlaces    = nPlaces
    ,tag        = 
      String.concatWith "_" 
        ( map Real.toString [super,park,home,corp,school,train]
        @ [#tag infectRule]
        @ map Real.toString [vacEff,vacTrCover,vacSchCover])
    ,mcid       = getOpt (mcid, "0")
    }: Trivial.conf

  val infRules1 = 
    [   {tag = "EMP_30_JOJ_SJK"
        ,n    = 30
        ,rule = {role   = ROL_SOME Employed
                ,livein = LIV_SOME JOJ
                ,workat = WOR_SOME [(SJK,Corp)]
                }
        ,isRandom = false
        } 
    ,  {tag = "EMP_30_JOJ_LOCAL"
        ,n    = 30
        ,rule = {role   = ROL_SOME Employed
                ,livein = LIV_SOME JOJ
                ,workat = WOR_LOCAL
                }
        ,isRandom = false
        }
     ,  {tag = "HUS_30_JOJ_LOCAL"
        ,n    = 30
        ,rule = {role   = ROL_SOME Hausfrau
                ,livein = LIV_SOME JOJ
                ,workat = WOR_LOCAL
                }
        ,isRandom = false
        } 
    ,   {tag = "EMP_30_HAC_SJK"
        ,n    = 30
        ,rule = {role   = ROL_SOME Employed
                ,livein = LIV_SOME HAC
                ,workat = WOR_SOME [(SJK,Corp)]
                }
        ,isRandom = false
        }
    ,  {tag = "CRM_30_HAC_SNJ"
        ,n    = 30
        ,rule = {role   = ROL_SOME Student
                ,livein = LIV_SOME HAC
                ,workat = WOR_SOME [(SJK,Cram)]
                }
        ,isRandom = false
        }
  ]

  val infRules2 =
    [   {tag = "EMP_60_TKY_SJK"
        ,n    = 60
        ,rule = {role   = ROL_SOME Employed
                ,livein = LIV_SOME TKY
                ,workat = WOR_SOME [(SJK,Corp)]
                }
        ,isRandom = false
        } 
    ,   {tag = "EMP_60_TAC_SJK"
        ,n    = 60
        ,rule = {role   = ROL_SOME Employed
                ,livein = LIV_SOME TAC
                ,workat = WOR_SOME [(SJK,Corp)]
                }
        ,isRandom = false
        } 
    ,   {tag = "EMP_60_JOJ_TKY"
        ,n    = 60
        ,rule = {role   = ROL_SOME Employed
                ,livein = LIV_SOME JOJ
                ,workat = WOR_SOME [(TKY,Corp)]
                }
        ,isRandom = false
        } 
    ,   {tag = "EMP_60_JOJ_TAC"
        ,n    = 60
        ,rule = {role   = ROL_SOME Employed
                ,livein = LIV_SOME JOJ
                ,workat = WOR_SOME [(TAC,Corp)]
                }
        ,isRandom = false
        } 
    ]
(*
  fun tasksForInfRules nMC reproNum vac infRules =
    mapConcat (fn iniInfect => dup' (nMC, genConf reproNum vac iniInfect))
    *)

  fun tasks1 (): Trivial.conf list = 
     mapConcat (fn vacCover =>
       mapConcat (fn vacEff =>
          [ genConf reproNum4
                    {vacEff = vacEff, vacTrCover = vacCover, vacSchCover = 0.0}
                    (List.nth(infRules1,0))
                    NONE
          , genConf reproNum4
                    {vacEff = vacEff, vacTrCover = 0.0     , vacSchCover = vacCover}
                    (List.nth(infRules1,0))
                    NONE
          ]
       ) [0.1, 0.3, 0.5]
     ) [0.5, 0.6, 0.7, 0.8, 1.0]

  fun tasks2 (): Trivial.conf list =
    List.tabulate(16384, fn mcid =>
      genConf reproNum4
              novac
              {tag  = "ARBIT_30_JOJ_ARBIT"
              ,n    = 30
              ,rule = {role   = ROL_ARBIT (* ignored *)
                      ,livein = LIV_SOME JOJ
                      ,workat = WOR_ARBIT (* ignored *)
                      }:belongSpec
              ,isRandom = true
              }
              (SOME (Alice.sI mcid))
    )
end 

(* タスクセット2
  * 設置件数のスケールの問題
  *
*)
structure Tasks2 = struct
  open GenTask
  open Type
  fun genNPlaces (s:real) = let
    fun op * (x:int,y:real):int =
      Int.max(Real.round (Real.* (Real.fromInt x,y)),1)
  in
    #[{sch = 10*s, corp =20*s, cram = 1*s, super = 10*s, park = 2*s}
     ,{sch = 10*s, corp =20*s, cram = 1*s, super = 10*s, park = 2*s}
     ,{sch = 10*s, corp =20*s, cram = 1*s, super = 10*s, park = 2*s}
     ,{sch = 10*s, corp =20*s, cram = 1*s, super = 10*s, park = 2*s}
     ,{sch = 10*s, corp =20*s, cram = 1*s, super = 10*s, park = 2*s}
     ]
  end

  fun genConf
    {rr = {super, park, home, corp, school, train}
    ,nPop: int
    ,nPlacesScale: real
    }
  = {betaNSuper = super  * Type.gamma
    ,betaNPark  = park   * Type.gamma
    ,betaNHome  = home   * Type.gamma
    ,betaNCorp  = corp   * Type.gamma
    ,betaNSch   = school * Type.gamma
    ,betaNTrain = train  * Type.gamma
    ,infectRule = List.nth(Tasks1.infRules1, 0)
    ,vacEff     = 0.0
    ,vacTrCover = 0.0
    ,vacSchCover= 0.0
    ,nPop       = nPop
    ,nPlaces    = genNPlaces (nPlacesScale)
    ,tag        = 
      String.concatWith "_" 
        ( map Real.toString [super,park,home,corp,school,train]
        @ [#tag (List.nth(Tasks1.infRules1, 0))
          ,Alice.sI (nPop)
          ,Alice.sR (nPlacesScale)]
        )
    ,mcid       = getOpt (NONE, "0")
    }: Trivial.conf
  val tasks1 = 
    map (fn s => genConf {rr = Tasks1.reproNum4, nPop = 200000, nPlacesScale = s})
      [1.0, 10.0, 100.0]
  val tasks2 = 
    map (fn s => genConf {rr = Tasks1.reproNum4, nPop = 2000, nPlacesScale = s})
      [1.0, 0.5, 0.1]
end 
