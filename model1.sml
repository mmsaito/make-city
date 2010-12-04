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
  open Alice;
  open EasyPrint;

  (* モデル設定パラメータ:
    実装上の注意: 内容はtenativeかつadhocである。型を立てるのは、記述上の便宜のためであって、
      設計上の理由ではない。
   *)
  type conf = 
    {betaNHome : real
    ,betaNSch  : real
    ,betaNSuper: real
    ,betaNCorp : real
    ,betaNPark : real
    ,betaNTrain: real
    ,e0_JOJ    : int
    ,nPop      : int (* ひとつの街の人口。後で配列にする *)
    ,tag       : string
    ,mcid      : string
    }
  (* 設定例 *)
  val conf0: conf =
    {betaNHome  = 1.4 * gamma
    ,betaNSch   = 1.8 * gamma
    ,betaNSuper = 1.4 * gamma
    ,betaNCorp  = 1.8 * gamma
    ,betaNTrain = 1.8 * gamma
    ,betaNPark  = 0.5 * gamma
    ,e0_JOJ     = 30
    ,nPop       = 3000
    ,tag        = "sample"
    ,mcid       = ""
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
   * ToDo: このこめんとをきちんとメンテナンスしておくように。
   * 会社員: 月～金は会社へ、土日は会社以外に適当に
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
(* でたらめぶりは、上のように改定したもののほうが望ましかろう
          ListPair.zip
            ( List.tabulate
               ( irndIn rnd (1,length (#belong p))
               , fn _ => today + irndIn rnd (10*hours',18*hours'))
            , #belong p)
*)
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
  fun rulePerson id = let
    val rnd = getrnd()
    val age    = Real.abs (30.0 + 30.0*rgauss rnd)
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
  fun rulePlace' size betaN place_t = let
    val rnd = getrnd()
  in
    {id    = place_t 
    ,nVis  = zeroNVis ()
    ,pTrns = zeroPTrns ()
    ,size  = iR o abs @@ rI size + rI size*rgauss rnd
    ,betaN = abs (betaN + 0.1*betaN*rgauss rnd)
    }: place
  end

  fun rulePlace place_k size betaN area_t id = 
    rulePlace' size betaN {place_k = place_k, area_t = area_t, id = id, tVis = ref ~1}

  (* 行動範囲ルール *) 
  fun ruleVisit (areas: area vector)(PERSON p: person): place_t list = let
    val rnd = getrnd()
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
  in
    case #role p 
      of Employed => map #id [rndSelV rnd (#corp a') , rndSelV rnd (#super a0)]
       | Student  => map #id (selCram() `:: [rndSelV rnd (#sch a0), rndSelV rnd (#super a0)])
       | HausFrau => map #id [rndSelV rnd (#super a0), rndSelV rnd (#park a0) ]
  end

  (* 鉄道運行ルール *)
  val time2next = [(TKY,5),(SJK,10),(JOJ,15),(TAC,20),(HAC,0)]
  val services =
    (Vector.fromList o List.concat) 
    (map (fn h => 
      List.concat (map (fn m =>
       [ {deptime = 60*h + m, stations = #[TKY, SJK, JOJ, TAC, HAC]} 
       , {deptime = 60*h + m, stations = #[HAC, TAC, JOJ, SJK, TKY]} 
       ]) [0, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55])
     ) [6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23])

  (* ====================================================================  *)
  (* 上記ルールによる都市の組み立て *)
  fun perHome (conf:conf) at = let
    val betaN = fn () => abs (#betaNHome conf*(1.0 + 0.1*rgauss (getrnd())))
  in
    F.makeHome at betaN (F.makePerson (#nPop conf) rulePerson) ruleHome
  end

  fun place (conf:conf) at home = let
    infixr 2 `::
    fun op `:: ((true,x),xs)  = x :: xs
      | op `:: ((false,_),xs) = xs
  in
    F.makePlace at home (
        (at = SJK orelse at = TAC, 
         ( 1, rulePlace Cram 100 (#betaNSch   conf)) )`::
        [(10, rulePlace Sch  100 (#betaNSch   conf))
        ,(10, rulePlace Super 20 (#betaNSuper conf))
        ,(19, rulePlace Corp  15 (#betaNCorp  conf))
        ,( 2, rulePlace Park 100 (#betaNPark  conf))
        ]
      ) 
  end

  fun infect (conf:conf)(area:area) = let
    val rnd = getrnd()
  in
    if (#1 area = JOJ) 
      then F.distInfect rnd area [(#e0_JOJ conf,EXP 0)]
      else area
  end

  fun area (conf:conf) = 
    Vector.map (fn at => 
      (fn (per,home) => 
        infect conf (at, per, place conf at home)
      ) (perHome conf at)) 
        #[HAC, TAC, JOJ, SJK, TKY]
  fun train (conf:conf) = F.makeTrains 
    {time2next = time2next, services = services, betaN = #betaNTrain conf, size = 200}

  fun city (conf:conf) = 
    F.evalPlace
      {area = F.makeVisit' (area conf) ruleVisit
      ,train = train conf
      ,time = 0
      }
  
  (* 印字機 *)
  fun showRole Employed = "Employed" 
    | showRole Hausfrau = "Hausfrau" 
    | showRole Student  = "Student" 

  fun showHealth SUS        = "SUS," ^ "0" (* dummy *)
    | showHealth (EXP time) = "EXP," ^ sI time
    | showHealth (INF time) = "INF," ^ sI time
    | showHealth (VAC time) = "VAC," ^ sI time   
    | showHealth (REC time) = "REC," ^ sI time

  fun showPlace_k' Cram  = "Cram"
    | showPlace_k' Sch   = "Sch"
    | showPlace_k' Corp  = "Corp"
    | showPlace_k' Home  = "Home"
    | showPlace_k' Super = "Super"
    | showPlace_k' Park  = "Park"
    | showPlace_k' Train = "Train"

  fun showPlace_t' ({area_t, place_k, id, ...}: place_t) =
    String.concat [sI area_t,",", showPlace_k' place_k,",", sI id]

  fun writeCity (city:city) (f:string) = let
    val os = TextIO.openOut f
    fun w s = TextIO.output(os, s)
    fun w' s = (w s; w "\n")
    fun w_ s = (w s; w ",")

    val w_place_t = w' o showPlace_t' 

    fun w_person (PERSON {role, belong, visit, dest, health, ...}) = 
      (w' (showRole role) 
      ;w' "belong:"; w (sI o length @@ belong); w' ",length"
      ;app w_place_t belong
      ;w' "health:"
      ;w' (showHealth (hd health))
      )

    fun w_place (pl: place) = 
      (w_place_t (#id pl)
      ;w' "betaN:"
      ;w' (sR (#betaN pl))
      )

    fun w_place_group({cram,sch,corp,park,super,home}:places) =
      (w' (showPlace_k' Cram); w (sI o Vector.length @@ cram); w' ",length"
      ;Vector.app w_place cram

      ;w' (showPlace_k' Sch); w (sI o Vector.length @@ sch) ; w' ",length"
      ;Vector.app w_place sch

      ;w' (showPlace_k' Corp); w (sI o Vector.length @@ corp); w' ",length" 
      ;Vector.app w_place corp

      ;w' (showPlace_k' Park); w (sI o Vector.length @@ park); w' ",length" 
      ;Vector.app w_place park

      ;w' (showPlace_k' Super); w (sI o Vector.length @@ super); w' ",length" 
      ;Vector.app w_place super

      ;w' (showPlace_k' Home); w (sI o Vector.length @@ home); w' ",length" 
      ;Vector.app w_place home
      )

    fun w_train({id: place_t, nVis: nVis, size: size, betaN: real, pTrns:pTrns
               ,iSked: int, sked: {time:time, area_t:area_t} vector}:mobil) =
      (w_place_t id
      ;w' "betaN:"
      ;w' (sR betaN)
      ;w (sI (Vector.length sked)); w' (",length")
      ;w' "sked:"
      ;Vector.app (fn {time,area_t} => (w (sI time); w ","; w' (sI area_t))) sked
      )

    fun w_area(area:area) =
     (w' (sI (#1 area) ^ ", id" ) (* id *)
     ;w' "person:"; w' (sI (length (#2 area)))
     ;List.app w_person (#2 area)
     ;w' "place-group:"; w (sI 6); w' ",length" (* type places *)
     ;w_place_group (#3 area)
     )
    fun w_time(time:time) = w' (sI time)
  in
    (w' "area:"; w (sI (Vector.length (#area city))); w' ",length"
    ;Vector.app w_area (#area city)
    ;w' "train:"; w' (sI (Vector.length (#train city)))
    ;Vector.app w_train (#train city)
    ;w' "time:"
    ;w(sI (#time city)^"\n")
    ;TextIO.closeOut os
    )
  end


  (* 3000人 、1日で、45[sec] 
     150万人、1日で、6.25[時間]    (実スケール)
     15万人 、1日で、37.5[分]      (1/10スケール)
     15万人 、1週間で、4.375[時間] (1/10スケール)
     15万人 、6か月で、4.6875[日]  (1/10スケール)
   * *)

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
    ;T.output(os, "e0_JOJ,"    ^ fI (#e0_JOJ conf) ^ "\n")
    ;T.closeOut os
    )
  end

  (* トップレベル関数 = C/Fortranでコマンドラインにあたる関数たち *)
  (* 実装上の注意:
     動作を決める引数はいまのところ conf型としているが、じきにパラメータ数が
     多くなって手に負えなくなるので、label{X}{不完全なパラメータ}からconf型を完成される
     ルーチンを書いて、ref{X}を引数にとるようにせよ。
   *)
  fun run1 {conf, recstep, tStop, dir, tag, city} = let
    val () = writeConf conf (dir^"/conf_"^ tag^".csv")
    val os = T.openOut      (dir^"/pop_" ^ tag^".csv")
    val () = Probe.showPopTag os 5
    val nstep   = tStop div recstep
    fun step city = let
      val city = Iterator.applyN F.advanceTime recstep city
      val pop  = Probe.reducePop' city
    in
      city before Probe.showPop os (#time city) pop
    end
  in
    Iterator.applyN step nstep city 
      before T.closeOut os
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
      val city = Iterator.applyN F.advanceTime recstep city
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
