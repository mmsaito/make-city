(* 人口と振る舞いの具体的なデザイン *)
structure Trivial = struct
(* ================================================================ *)
  open Type
  infix 7 times
  val op @@ = fn (x,y) => x y; infix 1 @@;
  val op $ = Vector.sub; infix 9 $;
  structure F = Frame;
  structure T = TextIO
  structure It = Iterator
  open X_Misc;
  open Alice; infix 9 *^;
  open EasyPrint;

  (* モデル設定パラメータ:
    実装上の注意: 内容はtenativeかつadhocである。型を立てるのは、記述上の便宜のためであって、
      設計上の理由ではない。
     - 12/16追記:
       - モデルに出てくる数値パラメータをconfに置くことにすると、作業がしにくくなる。
       - そこで、パラメータ探索の対象となるもの(これは、研究の進行とともに変化す
         る流動的なものである) だけをconfに書くことにする。
  *)
  type conf = 
    {betaNHome  : real
    ,betaNSch   : real
    ,betaNSuper : real
    ,betaNCorp  : real
    ,betaNPark  : real
    ,betaNTrain : real
    ,betaNHosp  : real
    ,infectRule : {tag:string, n:int, rule:belongSpec, isRandom:bool}
    ,intervRule : {tag:string, n:int, rule:belongSpec, isRandom:bool, time:int, kind:intervOpt} list
    ,vacResponse: (age -> real) option
    ,vacHyposensitize: (age -> real) option
    ,nPlaces    : {cram:int, sch:int, corp:int, park:int, super:int, hosp:int} vector
    ,nPop       : int vector (* ひとつの街の人口。後で配列にする *)
    ,vacEff     : real (* ワクチン効果 *)
    ,vacTrCover : real (* 接種実施率 *)
    ,vacSchCover: real (* 接種実施率 *)
    ,hospPop    : {doc:int, inpat: int, outpat: int}
    ,tag        : string
    ,mcid       : string
    }

  (* テストデータ *)
  val frau = let
    val vis = {place_k = Home , area_t = 0, id = 0, tVis = ref ~1}: place_t
  in PERSON 
    {age=23.0:age, gender = F
    ,belong = [ {place_k = Super, area_t = 0, id = 0, tVis = ref ~1}
              , {place_k = Park , area_t = 0, id = 0, tVis = ref ~1}
              , {place_k = Corp , area_t = 0, id = 0, tVis = ref ~1}
              , vis
              ]: place_t list
    ,visit  = vis
    ,role   = Hausfrau
    ,dest   = NONE: place_t option
    ,health = nil: health list
    ,sched  = nil: (time * place_t) list
    ,mkSched  = (fn _ => nil): person * time -> (time * place_t) list
    }
  end

  (* 人口分布に似た簡単な分布がある:   クラーク分布 *)
  (* ====================================================================  *)
  (* 生成ルール *)

  (* 行動テンプレート *)
  (* 次のような振る舞い
   * 会社員: 月～金は会社へ、土日は会社以外に適当に
   * 学生  : 月～金は学校へ、人によっては塾通いをしている。
   * 主婦  : 一日n回はスーパーに行く。
   *
   *)
  
  (* 会社員のスケジュール *)
  fun schedEmp (PERSON p, t:time): (time * place_t) list = let
    val rnd = getrnd()
    val {day,weekday,hour,step} = timecomp t
    fun home() = valOf (List.find (fn {place_k = Home, ...} => true | _ => false) (#belong p))
    fun corp() = valOf (List.find (fn {place_k = Corp, ...} => true | _ => false) (#belong p))
    val today = day*days'
  in
    if (step = 0) then (* 毎日午前0時にスケジュールを立てる *)
      if (1 <= weekday andalso weekday <= 5) then
          [ (today + irndIn rnd ( 6*hours',10*hours'), corp())
          , (today + irndIn rnd (18*hours',22*hours'), home())
          ]
      else
        Misc.qsortL (fn ((t,_),(t',_)) => Int.compare(t,t')) (
          (today + 18*hours', home()) ::
          ListPair.zip
            ( List.tabulate
               ( irndIn rnd (1,length (#belong p))
               , fn _ => today + irndIn rnd (10*hours',18*hours'))
            , #belong p)
        ) 
    else
      #sched p
  end

  (* 学生のスケジュール *)
  fun schedStu (PERSON p, t:time): (time * place_t) list = let
    val rnd = getrnd()
    val {day=day,weekday,hour,step} = timecomp t
    val today = day*days'
    fun home() = valOf (List.find (fn {place_k = Home, ...} => true | _ => false) (#belong p))
    fun sch()  = valOf (List.find (fn {place_k = Sch , ...} => true | _ => false) (#belong p))
    fun cram() = List.find (fn {place_k = Cram, ...} => true | _ => false) (#belong p)
  in
    if (step = 0) then (* 毎日午前0時にスケジュールを立てる *)
      if 1 <= weekday andalso weekday <= 5 then
          [ (today + irndIn rnd ( 6*hours', 8*hours'), sch() )
          , (today + irndIn rnd (16*hours',19*hours'), home())
          ] 
          @(case cram() 
            of SOME cram => 
              [ (today + irndIn rnd (19*hours', 20*hours'), cram)
              , (today + irndIn rnd (21*hours', 22*hours'), home())
              ]
             | NONE => nil)
      else
        (* 10時～18時の間、適当な時間に適当な場所をうろつく *)
        (* ダブルブッキングが生じ得るが...どうしようか *)
        Misc.qsortL (fn ((t,_),(t',_)) => Int.compare(t,t')) (
          (today + 18*hours', home()) ::
          List.tabulate
            (irndIn rnd (1,length (#belong p))
            , fn _ => 
              (today + irndIn rnd (10*hours',18*hours')
              ,List.nth(#belong p, irndIn rnd (0, length(#belong p)))
              )
            )
        )
    else
      #sched p
  end

  fun schedHaus (PERSON p, t:time): (time * place_t) list = let
    val rnd = getrnd()
    (* 注意: 厳密には、区間演算をする必要があるが、面倒なのでさぼっている *)
    val {day=day,weekday,hour,step} = timecomp t
    val today = day*days'
    fun supers() = List.filter (fn p => #place_k p = Super) (#belong p)
    val sort = Misc.qsortL (fn ((t,_,_),(t',_,_)) => Int.compare (t,t')) 
  in
    if (step = 0) then
      let 
        (* (1) 場所ごとの滞在期間を計算する *)
        (* (1a) 一日のどこかの時点でたかだか1時間の買い物をする *)
        val tInSuper  = today    + irndIn rnd (10*hours', 20*hours')
        val tOutSuper = tInSuper + irndIn rnd (10*minutes', 1*hours')
        (* 行きつけりスーパーが2個以上あってもいいかもしれぬ。 *)
        val schedS = (tInSuper, tOutSuper, rndSelL rnd (supers()))

        (* (1b) 別の場所にも行くかもしれない *)
        val others = List.filter (fn {place_k=x,...} => x <> Super andalso x <> Home) (#belong p)
        val nO = irndIn rnd (0,length others + 1)
        val schedO = List.tabulate 
          ( nO, fn _ => let val tIn  = today + irndIn rnd (10*hours', 20*hours')
                            val tOut = tIn   + irndIn rnd (0*hours' , 2*hours' )
                        in (tIn, tOut, rndSelL rnd others) end)

        val home = valOf (List.find (fn p => #place_k p = Home) (#belong p))
        (* (2) 場所毎の滞在時刻の間隔から旅程を連結する *)
        fun connect (x::x'::xs) = 
          let val (tb,te,p) = x
              val (tb',te',p') = x'
          in
             if tb + 2*hours' < tb' 
               then (tb,p)::(te,home)::connect (x'::xs) 
               else (tb,p)           ::connect (x'::xs)
          end
          | connect ((tb,te,p)::nil) = (tb,p)::(te,home)::nil (* いづれ、家に帰る *)
          | connect nil = nil (* unreachable *)
      in
        connect (sort (schedS :: schedO))
      end
    else
      #sched p
  end

  (* 人間生成ルール *)
  val ageTable = let
    fun normalise xs = let
      val s = rI (foldl (fn ((_,_,n),s) => n + s) 0 xs) in
      map (fn (a,b,c) => (rI c/s,(a,b))) xs end
  in normalise
      [(0,  5,476692)
      ,(5, 10,481382)
      ,(10,15,466593)
      ,(15,20,562968)
      ,(20,25,859742)
      ,(25,30,981230)
      ,(30,35,1121689)
      ,(35,40,1026016)
      ,(40,45,885146)
      ,(45,50,736656)
      ,(50,55,770054)
      ,(55,60,938669)
      ,(60,65,813422)
      ,(65,70,705944)
      ,(70,75,612400)
      ,(75,80,451357)
      ,(80,100,525826)
      ]
  end

  fun rulePerson id = let
    val rnd = getrnd()
    (* val age = Real.abs (30.0 + 30.0*rgauss rnd) *)
    val age = rI (irndIn rnd (rndSelLP rnd ageTable))
    val gender = rndSel rnd 0.5 (F,M)
    val (role,sched)   = 
      if (age <= 22.0)     then (Student, schedStu)
      else if (age < 60.0) then
        case gender
          of F => rndSel rnd 0.6 ((Employed,schedEmp), (Hausfrau,schedHaus))
           | M => rndSel rnd 0.8 ((Employed,schedEmp), (Hausfrau,schedHaus))
      else
        (Hausfrau,schedHaus)
  in
    {age = age, gender = gender, role = role, mkSched = sched}
  end

  (* 家族構成ルール *)
  fun ruleHome () =  let val rnd = getrnd() in
    List.tabulate (1 + iR (abs (2.0 * rgauss rnd)), fn _ => fn _ => true) end

  (* 公共空間生成ルール *)
  fun rulePlace' betaN place_t = let
    val rnd = getrnd()
  in
    {id    = place_t 
    ,nVis  = zeroNVis ()
    ,pTrns = zeroPTrns ()
    ,size  = 0 (* no longer referred *)
    ,betaN = abs (betaN + 0.1*betaN*rgauss rnd)
    }: place
  end

  fun rulePlace place_k betaN area_t id = 
    rulePlace' betaN {place_k = place_k, area_t = area_t, id = id, tVis = ref ~1}

  (* 行動範囲ルール *) 
  fun ruleVisit (areas: area vector): person -> place_t list = let
    val rnd = getrnd()
    val nCorp =
      Vector.map (fn area => Vector.length (#corp (#3 area))) areas
    val nSumCorp = Vector.foldl (op + ) 0 nCorp

    val prCorpArea = Misc.listV 
        (Vector.mapi (fn (i,nC) => (rI nC / rI nSumCorp, #3 (areas $ i))) nCorp)

    (* 会社の従業員数の分布がパレート分布になるように構成 *)
    fun selMyCorp () = let
      val corp = #corp (rndSelLP rnd prCorpArea)
      val n    = rI (Vector.length corp)
      val idxSel = paretoSel {alpha = 1.0, gamma = 100.0, n = n - 1.0, m = 0.0, beta = 2.0}
        handle s => (print "some exception arise!\n"; raise s)
      fun gard i = 
        if i < 0 then (print ("parato[" ^ sI i ^ "] "); 0)
        else if i >= iR n then (print ("parato[" ^ sI i ^"] "); 0)
        else i
    in
      corp $ (gard (idxSel rnd))
    end

    (* 外勤先はランダムに選択 *)
    fun selCorp n = 
      selMyCorp () :: 
        List.tabulate(n-1, fn _ => rndSelV rnd (#corp (rndSelLP rnd prCorpArea)))
  in  
    fn (PERSON p: person) => let
    val i0 = areaPer (PERSON p)
    val (_,_,a0) = areas $ i0
    val (_,_,a') = rndSelV rnd areas

    infixr 2 `::
    fun op `:: (SOME x,xs) = x :: xs
      | op `:: (NONE  ,xs) = xs
 
    fun crams() = List.filter (fn x => Vector.length x > 0) 
                   (Misc.listV (Vector.map (#cram o #3) areas))
    val selCram = rndSelLP rnd
      [(0.4, fn () => SOME (rndSelV rnd (rndSelL rnd (crams()))))
      ,(0.6, fn () => NONE)]

    fun selSch () = let
      val a = rndSelLP rnd [(0.8,a0), (0.2, a')]  (* 8割は地元へ、残り2割りは学区外へ *)
    in
      rndSelV rnd (#sch a)
    end
  in
    case #role p
      (* 23.2.18 外勤の実験。とりあえず、コード改変でやってみる。*)
      (* of Employed => map #id [selCorp(), rndSelV rnd (#super a0)] *)
      of Employed => map #id (rndSelV rnd (#super a0) :: selCorp 1)
       | Student  => map #id (selCram() `:: [selSch (), rndSelV rnd (#super a0)])
       | HausFrau => map #id [rndSelV rnd (#super a0), rndSelV rnd (#super a'), rndSelV rnd (#park a0) ]
  end
  end

  (* 鉄道運行ルール *)
  val time2next = [(TKY,5),(SJK,10),(JOJ,15),(TAC,20),(HAC,0)]
  val services =
    (Vector.fromList o List.concat) 
    (map (fn h => 
      List.concat (map (fn m =>
       [ {deptime = iR (rI h*hours + rI m*minutes), stations = #[TKY, SJK, JOJ, TAC, HAC]} 
       , {deptime = iR (rI h*hours + rI m*minutes), stations = #[HAC, TAC, JOJ, SJK, TKY]} 
       ]) [0, 20, 40]) 
          (* [0, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55]) *)
     ) [6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23])

  (* 学校数,会社数の表 ⇒ 2/1の考察により廃止 *)
  (* 
  fun makePopTable (conf:conf) = let
    val nAvgPop  = #nPop conf
    val nCityPop = nAvgPop * 5
    val nTownPop = nAvgPop
    val stuRate  = 0.20 (* 人口に占める学生の割合 *)
    val empRate  = 0.50 (* 人口に占める従業員の割合 *)
    val wEmpPerCorp = 100.0 (* 1社あたりの従業員数 *)
    val popTable0 =
     [{area_t = HAC, wStuPerSch = 424.0, wCorp =  5.0, nTownPop = nTownPop}
     ,{area_t = TAC, wStuPerSch = 442.0, wCorp = 10.0, nTownPop = nTownPop}
     ,{area_t = JOJ, wStuPerSch = 419.0, wCorp =  5.0, nTownPop = nTownPop}
     ,{area_t = SJK, wStuPerSch = 277.0, wCorp = 40.0, nTownPop = nTownPop}
     ,{area_t = TKY, wStuPerSch = 348.0, wCorp = 40.0, nTownPop = nTownPop}
     ]
    val wSumCorp = foldl (fn ({wCorp,...},s) => s + wCorp) 0.0 popTable0
    fun norm {area_t = n, wStuPerSch, wCorp, nTownPop} =
      {area_t = n
      ,nSch   = Real.ceil (rI nTownPop*stuRate/wStuPerSch)
      ,nCorp  = Real.ceil (rI nCityPop*empRate/wEmpPerCorp*(wCorp/wSumCorp))
      }
  in
    map norm popTable0
  end
  *)

  (* ====================================================================  *)
  (* 上記ルールによる都市の組み立て *)
  fun perHome (conf:conf) at = let
    val betaN = fn () => abs (#betaNHome conf*(1.0 + 0.1*rgauss (getrnd())))
  in
    F.makeHome at betaN (F.makePerson (#nPop conf $ at) rulePerson) ruleHome
  end

  fun place (conf:conf) at home = let 
      val nPlaces = #nPlaces conf $ at
  in 
    F.makePlace at home
        [(#cram  nPlaces, rulePlace Cram  (#betaNSch   conf))
        ,(#sch   nPlaces, rulePlace Sch   (#betaNSch   conf))
        ,(#super nPlaces, rulePlace Super (#betaNSuper conf))
        ,(#corp  nPlaces, rulePlace Corp  (#betaNCorp  conf))
        ,(#park  nPlaces, rulePlace Park  (#betaNPark  conf))
        ]
  end

  fun area (conf:conf) = 
    Vector.map 
      (fn at => 
        (fn (per,home) =>
          (at, per, place conf at home)
        ) (perHome conf at)
      ) #[HAC, TAC, JOJ, SJK, TKY]

  fun train (conf:conf) = F.makeTrains 
    {time2next = time2next, services = services, betaN = #betaNTrain conf, size = 200}

  fun infectVac (conf:conf) (city:city) = let
    val () = F.setVacEff (#vacEff conf)
    val rnd = getrnd()
    val vac: area -> area =
      F.vacWho (fn p => F.ruleVacTrain  {cover = #vacTrCover  conf, eff = #vacEff conf} p orelse 
                        F.ruleVacSchool {cover = #vacSchCover conf, eff = #vacEff conf} p)

    (* 注意: ここの抽出方法の違い(決定的/ランダム)の対応は、非常に場当たり的なものである *)
    fun infect infectRule (area:area) = let
      val nInf = #n infectRule
      val rule = #rule infectRule
      val () = print (Bool.toString (#isRandom (#infectRule conf)))
    in
      case #isRandom infectRule
        of false => F.ruleInfect nInf rule area
         | true  =>
            (case #livein rule
               of LIV_SOME area_t =>
                 if area_t = idArea area 
                   then F.distInfect rnd [(nInf,INF 0)] area
                   else area
                | LIV_ARBIT =>
                   F.distInfect rnd [(nInf div 5, INF 0)] area
                   (* マジックナンバー 5 = 都市数 *)
            )
    end
    and infect' (area:area) = 
      case Unsafe.Object.rep (Unsafe.Object.toObject (#infectRule conf))
        of Unsafe.Object.Pair =>
           foldl (fn (infectRule,area) => infect infectRule area) 
             area (Unsafe.cast (#infectRule conf))
         | _      => infect (#infectRule conf) area
  in
    {area  = Vector.map (infect' o vac) (#area city)
    ,train = #train city
    ,time  = #time city
    }
  end

  (* 病院の構成 *)
  fun mkHospital {inpat:int, doc:int, outpat:int} nHosp beta (area:area) = let
    fun loop (0, pop, popCooked, hosps) 
      = ( idArea area
        , List.concat (pop::rev popCooked)
          (* 注: 変更したものを末尾にもってくるのは、mkHospitalをもう一度読んで、
           * さらに病院を追加できるようにするため *)
        , {cram  = #cram (placeArea area)
          ,sch   = #sch (placeArea area)
          ,corp  = #corp (placeArea area)
          ,park  = #park (placeArea area)
          ,super = #super (placeArea area)
          ,home  = #home (placeArea area)
          ,hosp  = Vector.fromList hosps
          }
        ): area
      | loop (cnt, pop, popCooked, hosps) = let
      val pDoc   = List.take (List.drop (pop, inpat), doc)
      val pOutpat= List.take (List.drop (pop, inpat + doc), outpat)
      val rest   = List.drop (pop, inpat + doc + outpat)
      val hosp_t: place_t =  
        {place_k = Hosp
        ,area_t  = idArea area
        ,id      = cnt - 1
        ,tVis    = ref ~1
        }
      val hosp: place = 
        {id    = hosp_t
        ,betaN = beta
        ,nVis  = zeroNVis()
        ,size  = 0
        ,pTrns = zeroPTrns()
        }
      fun asPatient (PERSON p) = PERSON
      { age    = #age p
      , gender = #gender p
      , role   = Patient
      , belong = [hosp_t] 
      , visit  = hosp_t
      , dest   = NONE
      , health = #health p
      , mkSched = fn _ => nil 
      , sched   = nil
      } 
      val pInpat = map asPatient (List.take (pop, inpat))
      val pop = List.drop (pop, inpat)
     
      fun asDoctor (PERSON p) = PERSON 
      { age    = #age p
      , gender = #gender p
      , role   = Doctor
      , belong = hosp_t :: List.filter (fn {place_k,...} => place_k <> Corp) (#belong p)
      , visit  = #visit p
      , dest   = #dest p
      , health = #health p
      (* さぼり:SML版でシミュレーションするときは別のものにする必要あり *)
      , mkSched = #mkSched p 
      , sched   = #sched p
      }
      val pDoc = map asDoctor (List.take (pop, doc))
      val pop = List.drop (pop, doc)

      fun asOutpat (PERSON p) = PERSON
      { age    = #age p
      , gender = #gender p
      , role   = #role p
      , belong = hosp_t :: #belong p
      , visit  = #visit p
      , dest   = #dest p
      , health = #health p
      , mkSched = #mkSched p 
      , sched   = #sched p
      }
      val pOutpat = map asOutpat (List.take (pop, outpat))
      val pop = List.drop (pop, doc)
    in
      loop (cnt - 1, pop, pInpat::pDoc::pOutpat::popCooked, hosp::hosps) 
    end
  in
    loop (nHosp, popArea area, nil, nil)
  end

  fun city (conf:conf) = 
    F.evalPlace
      {area  = Vector.mapi (fn (i,area) =>
           mkHospital (#hospPop conf) (#hosp (#nPlaces conf $ i)) (#betaNHosp conf) area
         ) (F.makeVisit' (area conf) ruleVisit)
      ,train = train conf
      ,time  = 0
      }

  (* ワクチンの効果 [デフォルト] ************************************)
  (* ワクチン奏功率 *)
  fun vacResponse age =
    if      0.0 <= age andalso age < 5.0  then 0.6
    else if 5.0 <= age andalso age < 65.0 then 0.8
    else                                       0.5
  (* ワクチンによる減感作率 ------- これはダミー *)
  fun vacHyposensitize age = 
    if      0.0 <= age andalso age < 5.0  then 0.8
    else if 5.0 <= age andalso age < 65.0 then 0.4
    else                                       0.8
  (****************************************************************)

  (* ルールに従って、いろんな介入計画表をつくる *)
  fun mkIntervPlan (conf:conf) (city:city) = let
    val rnd = getrnd()
    val vacEff = 
      {vacResponse      = getOpt (#vacResponse conf, vacResponse)
      ,vacHyposensitize = getOpt (#vacHyposensitize conf, vacHyposensitize)
      }
    fun interv intervRule (area:area) = let
    in
      case #isRandom intervRule
        of false => F.ruleInterv vacEff intervRule area
         | true  => (print "Random selection is not implemented!\n"; raise Undef)
    end
    and interv' (area:area) = 
      List.concat (map (fn rule => interv rule area) (#intervRule conf))
  in
    Vector.foldl (fn (a,xs) => interv' a @ xs) nil (#area city)
  end


  (* 設定の書き出し、とても てんたてぃぶ *)
  fun writeConf (conf:conf) f = let
    val os = T.openOut f
  in
    (T.output(os, "alpha,"     ^ fG 14 Type.alpha ^ "\n")
    ;T.output(os, "gamma,"     ^ fG 14 Type.gamma ^ "\n")
    ;T.output(os, "betaNHome," ^ fG 14 (#betaNHome conf) ^ "\n")
    ;T.output(os, "betaNSch,"  ^ fG 14 (#betaNSch conf) ^ "\n")
    ;T.output(os, "betaNCorp," ^ fG 14 (#betaNCorp conf) ^ "\n")
    ;T.output(os, "betaNSuper,"^ fG 14 (#betaNSuper conf) ^ "\n")
    ;T.output(os, "betaNTrain,"^ fG 14 (#betaNTrain conf) ^ "\n")
    (* ;T.output(os, "e0_JOJ,"    ^ fI (#e0_JOJ conf) ^ "\n") *)
    ;T.closeOut os
    )
  end

    fun roleSym Employed = #"E"
      | roleSym Hausfrau = #"H"
      | roleSym Student  = #"S"
      | roleSym Patient  = #"P"
      | roleSym Doctor   = #"D"

    fun symOrd (#"S",#"S") = EQUAL
      | symOrd (#"S",_   ) = GREATER
      | symOrd (#"E",#"S") = LESS
      | symOrd (#"E",#"E") = EQUAL
      | symOrd (#"E",_   ) = GREATER
      | symOrd (#"H",#"S") = LESS
      | symOrd (#"H",#"E") = LESS
      | symOrd (#"H",#"H") = EQUAL
      | symOrd (#"H",_   ) = GREATER
      | symOrd (#"P",#"S") = LESS
      | symOrd (#"P",#"E") = LESS
      | symOrd (#"P",#"H") = LESS
      | symOrd (#"P",#"P") = EQUAL
      | symOrd (#"P",_   ) = GREATER
      | symOrd (#"D",#"S") = LESS
      | symOrd (#"D",#"E") = LESS
      | symOrd (#"D",#"H") = LESS
      | symOrd (#"D",#"P") = LESS
      | symOrd (#"D",#"D") = EQUAL

    fun infSeq (city:city) = let
      fun inArea (area:area) = 
        List.mapPartial
          (fn PERSON p => 
            case hd (#health p)
              of INF _ => SOME (roleSym (#role p)) | _ => NONE) (popArea area) 
      val seq = List.concat (Misc.listV (Vector.map inArea (#area city)))
    in
      String.implode (Misc.qsortL symOrd seq)
    end


  (* トップレベル関数 *)
  fun run1 {conf:conf, recstep, tStop, dir, tag, city, seq} = let
    val () = writeConf conf (dir^"/conf_"^ tag^".csv")
    val os = T.openOut      (dir^"/pop_" ^ tag^".csv")
    val os2= if seq then SOME (T.openOut  (dir^"/seq_" ^ tag^".csv")) else NONE
    val () = Option.app (fn os2 => 
      (T.output(os2, #mcid conf ^ " ")
      ;T.output(os2, infSeq city)
      ;T.output(os2, ".")
      )) os2

    val () = Probe.showPopTag os 5
    val nstep   = tStop div recstep
    fun step city = let
      val city = Iterator.applyN (F.advanceTime os2) recstep city
      val pop  = Probe.reducePop' city
      (* val _ = print (sI (#time city) ^ "\n") *)
    in
      city before Probe.showPop os (#time city) pop
    end
  in
    Iterator.applyN step nstep city 
      before (T.closeOut os
             ; Option.app (fn os => T.output(os, "\n")) os2
             ; Option.app T.closeOut os2
             )
  end
  
  fun run2 conf recstep tStop file (*city*) = let
    val npick = 20
    val () = writeConf conf ("conf_" ^ file ^ ".csv")
    val os = T.openOut ("trip_" ^ file ^ ".csv")
    (* tag *)
    val _ = T.output(os,"t,")
    val _ = It.applyN (fn () => T.output(os, "area, place_id, place_k,")) npick ()
    val _ = T.output(os,"\n")

    val nstep   = tStop div recstep
    fun step city = let
      val city = Iterator.applyN (F.advanceTime NONE) recstep city
      (* val pop  = Probe.reducePop' city *)
    in
      city before 
        (T.output(os, fI (#time city) ^ ",")
        ;app (fn per => T.output(os, Probe.showPlace_t (#visit (deP per)))) 
           (List.take (#2 (#area city $ JOJ), npick))
        ;T.output(os,"\n")
        )
    end
  in
    Iterator.applyN step nstep (city conf)
      before T.closeOut os
  end
end
