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
    ,infectID   = 30
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

  fun rep(n,x) = Vector.tabulate(n, fn _ => x)

  fun setMcid (x:Trivial.conf) mcid  =
  {betaNPark  = #betaNPark x
  ,betaNHome  = #betaNHome x
  ,betaNSuper = #betaNSuper x
  ,betaNSch   = #betaNSch x
  ,betaNCorp  = #betaNCorp x
  ,betaNTrain = #betaNTrain x
  ,betaNHosp  = #betaNHosp x
  ,infectRule = #infectRule x
  ,intervRule = #intervRule x
  ,nPop       = #nPop x
  ,nPlaces    = #nPlaces x
  ,tag        = #tag x    
  ,vacEff     = #vacEff x
  ,vacTrCover = #vacTrCover x
  ,vacSchCover= #vacSchCover x
  ,hospPop    = #hospPop x
  ,mcid       = Int.toString mcid
  ,vacResponse = NONE
  ,vacHyposensitize = NONE
  }: Trivial.conf

  fun dup' (n, conf:Trivial.conf) = List.tabulate(n, setMcid conf)

  val mapConcat = (fn f => List.concat o map f) 

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

(* �^�X�N�Z�b�g1
  * ���N�`���ڎ�̍ŏ��̎���
  * 20000�����e�J��������
*)
structure Tasks1 = struct
  open GenTask
  open Type
  val novac = {vacEff = 0.0, vacTrCover = 0.0, vacSchCover = 0.0}
  val nPlaces = 
    #[{sch = 2, corp = 4, cram = 0, super = 10, park = 2, hosp=0}
     ,{sch = 2, corp = 8, cram = 1, super = 10, park = 2, hosp=0}
     ,{sch = 2, corp = 4, cram = 0, super = 10, park = 2, hosp=0}
     ,{sch = 3, corp =30, cram = 1, super = 10, park = 2, hosp=0}
     ,{sch = 2, corp =30, cram = 0, super = 10, park = 2, hosp=0}
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
    ,betaNHosp  = 0.0
    ,infectRule = () (* infectRule *)
    ,intervRule = nil
    ,vacEff     = vacEff
    ,vacTrCover = vacTrCover
    ,vacSchCover= vacSchCover
    ,vacResponse = NONE
    ,vacHyposensitize = NONE
    ,nPop       = rep(5, 3000)
    ,hospPop    = {doc = 0, inpat = 0, outpat = 0}
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

(* �^�X�N�Z�b�g2
  * �ݒu�����̃X�P�[���̖��
  *
*)
structure Tasks2 = struct
  open GenTask
  open Type; infix 9 $;
  fun genNPlaces (s:real) = let
    fun op * (x:int,y:real):int =
      Int.max(Real.round (Real.* (Real.fromInt x,y)),1)
  in
    #[{sch = 10*s, corp =20*s, cram = 1*s, super = 10*s, park = 2*s, hosp=0}
     ,{sch = 10*s, corp =20*s, cram = 1*s, super = 10*s, park = 2*s, hosp=0}
     ,{sch = 10*s, corp =20*s, cram = 1*s, super = 10*s, park = 2*s, hosp=0}
     ,{sch = 10*s, corp =20*s, cram = 1*s, super = 10*s, park = 2*s, hosp=0}
     ,{sch = 10*s, corp =20*s, cram = 1*s, super = 10*s, park = 2*s, hosp=0}
     ]
  end

  fun genConf
    {rr = {super, park, home, corp, school, train}
    ,nPop: int vector
    ,nPlacesScale: real
    }
  = {betaNSuper = super  * Type.gamma
    ,betaNPark  = park   * Type.gamma
    ,betaNHome  = home   * Type.gamma
    ,betaNCorp  = corp   * Type.gamma
    ,betaNSch   = school * Type.gamma
    ,betaNTrain = train  * Type.gamma
    ,betaNHosp  = 0.0
    ,infectRule = () (* List.nth(Tasks1.infRules1, 0) *)
    ,intervRule = nil
    ,vacResponse = NONE
    ,vacHyposensitize = NONE
    ,vacEff     = 0.0
    ,vacTrCover = 0.0
    ,vacSchCover= 0.0
    ,hospPop    = {doc = 0, inpat = 0, outpat = 0}
    ,nPop       = nPop
    ,nPlaces    = genNPlaces (nPlacesScale)
    ,tag        = 
      String.concatWith "_" 
        ( map Real.toString [super,park,home,corp,school,train]
        @ [#tag (List.nth(Tasks1.infRules1, 0))
          ,Alice.sI (nPop $ 0)
          ,Alice.sR (nPlacesScale)]
        )
    ,mcid       = getOpt (NONE, "0")
    }: Trivial.conf
  val tasks1 = 
    map (fn s => genConf {rr = Tasks1.reproNum4, nPop = rep (5,200000), nPlacesScale = s})
      [1.0, 10.0, 100.0]
  val tasks2 = 
    map (fn s => genConf {rr = Tasks1.reproNum4, nPop = rep (5, 2000), nPlacesScale = s})
      [1.0, 0.5, 0.1]
end 

structure Tasks3 = struct
  open GenTask
  open Type; infix 9 $;
  val nPop = #[571641, 176866, 138684, 314861, 44680]
  fun mult {super, park, home, corp, school, train} (n:real) = 
    {super=super*n, park=park*n, home=home, corp=corp*n, school=school*n, train=train*n}

  val reproNum3 = {super = 0.3, park = 0.5, home = 1.5, corp = 4.0, school = 5.0, train = 6.0, hosp = 1.2}
  val reproNum4 = {super = 0.3, park = 0.5, home = 1.5, corp = 3.0, school = 4.0, train = 6.0, hosp = 1.2}
  val reproNum5 = {super = 0.3, park = 0.5, home = 1.2, corp = 1.5, school = 1.8, train = 3.0, hosp = 1.2}
  val reproNum6 = {super = 0.3, park = 0.5, home = 0.8, corp = 1.5, school = 1.8, train = 10.0, hosp = 1.2}
  val nPlaces1 = 
    #[{sch = 70, corp = 1, cram = 0, super = 10, park = 2, nHosp=0}
     ,{sch = 20, corp = 1, cram = 1, super = 10, park = 2, nHosp=0}
     ,{sch = 12, corp = 1, cram = 0, super = 10, park = 2, nHosp=0}
     ,{sch = 29, corp =20, cram = 1, super = 10, park = 2, nHosp=0}
     ,{sch =  8, corp =20, cram = 0, super = 10, park = 2, nHosp=0}
     ]

  val nPlaces2 = 
    #[{sch = 28, corp = 1, cram = 0, super = 10, park = 2, nHosp=0}
     ,{sch = 28, corp = 1, cram = 1, super = 10, park = 2, nHosp=0}
     ,{sch = 28, corp = 1, cram = 0, super = 10, park = 2, nHosp=0}
     ,{sch = 28, corp =20, cram = 1, super = 10, park = 2, nHosp=0}
     ,{sch = 28, corp =20, cram = 0, super = 10, park = 2, nHosp=0}
     ]

  val nPlaces3 = 
    #[{sch = 28, corp = 1000, cram = 0, super = 10, park = 2, nHosp=0}
     ,{sch = 28, corp = 1000, cram = 1, super = 10, park = 2, nHosp=0}
     ,{sch = 28, corp = 1000, cram = 0, super = 10, park = 2, nHosp=0}
     ,{sch = 28, corp =20000, cram = 1, super = 10, park = 2, nHosp=0}
     ,{sch = 28, corp =20000, cram = 0, super = 10, park = 2, nHosp=0}
     ]

  val nPlaces4 = 
    #[{sch = 28, corp =  100, cram = 0, super = 10, park = 2, nHosp=0}
     ,{sch = 28, corp =  100, cram = 1, super = 10, park = 2, nHosp=0}
     ,{sch = 28, corp =  100, cram = 0, super = 10, park = 2, nHosp=0}
     ,{sch = 28, corp = 2000, cram = 1, super = 10, park = 2, nHosp=0}
     ,{sch = 28, corp = 2000, cram = 0, super = 10, park = 2, nHosp=0}
     ]

  val dummyINF = 
       {tag = ""
        ,n    = 0
        ,rule = {role   = ROL_SOME Employed
                ,livein = LIV_SOME JOJ
                ,workat = WOR_SOME [(SJK,Corp)]
                }
        ,isRandom = false
        } 

  val infEMP = 
       {kind = OPT_INTERV_INF
       ,n    = 30
       ,rule = {role   = ROL_SOME Employed
               ,livein = LIV_SOME JOJ
               ,workat = WOR_SOME [(SJK,Corp)]
               }
       ,time = 0
       ,isRandom = false
       ,tag  = "EMP30J2S"
       } 
  val infSCH = 
        {kind = OPT_INTERV_INF
        ,n    = 30
        ,rule = {role   = ROL_SOME Student
                ,livein = LIV_SOME JOJ
                ,workat = WOR_ARBIT 
                }
        ,time = 0
        ,isRandom = false
        ,tag = "SCH30J2S"
        }
  val vacTAC =
        {kind = OPT_INTERV_VAC
        ,n    = 5000
        ,rule = {role   = ROL_SOME Employed
                ,livein = LIV_SOME TAC
                ,workat = WOR_SOME [(TKY,Corp),(SJK,Corp),(JOJ,Corp),(HAC,Corp)]
                }
        ,time = 1440*3
        ,isRandom = false
        ,tag = "VAC5000TAC"
        }

  val hospPop = {doc = 10, inpat = 30, outpat = 70}
  fun genConf
    {rr = {super, park, home, corp, school, train, hosp}
    ,nPlaces
    }
  = {betaNSuper = super  * Type.gamma
    ,betaNPark  = park   * Type.gamma
    ,betaNHome  = home   * Type.gamma
    ,betaNCorp  = corp   * Type.gamma
    ,betaNSch   = school * Type.gamma
    ,betaNTrain = train  * Type.gamma
    ,betaNHosp  = hosp   * Type.gamma
    ,infectRule = () 
    ,intervRule = [infEMP, infSCH, vacTAC]
    ,vacEff     = 0.0
    ,vacTrCover = 0.0
    ,vacSchCover= 0.0
    ,vacResponse = NONE
    ,vacHyposensitize = NONE
    ,hospPop    = hospPop
    ,nPop       = nPop
    ,nPlaces    = nPlaces
    ,tag        = 
      String.concatWith "_" 
        ( map Real.toString [super,park,home,corp,school,hosp,train]
        @ [#tag (List.nth(Tasks1.infRules1, 0))]  (* �����C���K�v *)
        @ Misc.listV (Vector.map Alice.sI (nPop))
        )
    ,mcid       = getOpt (NONE, "0")
    }: Trivial.conf
end

