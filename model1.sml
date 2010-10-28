structure Trivial = struct
(* ================================================================ *)
(* 具体的な構成を決める手続き *)
  open Type
  infix 7 times
  val op <> = fn (x,y) => x y; infix 1 <>;
  val op $ = Vector.sub; infix 9 $;
  structure F = Frame;
  structure T = TextIO
  structure It = Iterator
  open X_Misc;
  open Alice;
  open EasyPrint;
  val rnd = Random.rand (0,1);
  fun rndselV (v: 'a vector) = 
    v $ (Int.mod (Random.randInt rnd, Vector.length v))

  (* 感染に関わる定数 *)
  val betaNHome  = 1.4 * gamma
  val betaNSch   = 1.8 * gamma
  val betaNSuper = 1.4 * gamma
  val betaNCorp  = 1.8 * gamma
  val e0_JOJ     = 30

  (* 人口分布に似た簡単な分布がある:   クラーク分布 *)
  (* ====================================================================  *)
  (* 生成ルール *)

  (* 行動テンプレート *)
  (* 次のような振る舞い
   * 会社員: 月～金は会社へ、土日は会社以外に適当に
   * 主婦  : 一日n回はスーパーに行く。
   *   p[1/step] * Δt^{-1}[steps/day] = n[/day] =>  p = nΔt
   *
   * シミュレーションの1ステップΔtと時間間隔のステップ数表示
   *   - Δtは[day]の単位で書かれているとする 
   *     <=> Δt[days/step]            <=> Δt^{-1}[steps/day]
   *     <=> Δt^{-1}/24[steps/hour]   <=> 24Δt [hour/steps]
   *     <=> Δt^{-1}/24/60[steps/min] <=> 24*60*Δt [min/steps]
   *        per n unit = 1.0*unit / n
   *        unit = {day = 1, hour = 24, min = 24*60}
   * 回帰処理: 「毎日帰宅する」
   *   ... これをリアリスティックに書くのはむつかしい。第0近似として、
   *     - 午後8時 - 午後10時: 確率 p でHomeを目的値にセットする。
   *       ただし、p = 24Δt/2 = 12Δt
   *     - 午後11時 -        : 確率1でHomeを目的値にセットする。
   *
   * e-folding time を T とすると、tだけ経過したときの残留率は、
       t = T: 36%, t = 2T: 13%, t = 3T: 5%, {t = 4T: 2%}, t = 5T: 0.7%
   * 出勤時刻をNステップいないで一様にするには、
       pk = 1/(N - k).  (k=0,...,N-1)
   *)
  (* 会社員のスケジュール *)
  fun schedEmp (PERSON p, t:time): (real * place_t) list = let
    val Employed = #role p
    val {day,weekday,hour,step} = timecomp t
    fun home() = valOf (List.find (fn {place_k = Home, ...} => true | _ => false) (#belong p))
    fun corp() = valOf (List.find (fn {place_k = Corp, ...} => true | _ => false) (#belong p))
  in
    if (1 <= weekday andalso weekday <= 5) then
      if (6.0 <= hour andalso hour < 10.0) then
        let (* val pr = 1.0 times per 4.0 hours *)
            val pr = 1.0/(4.0*hours - (rI step - 6.0*hours))
        in [(pr, corp()), (1.0 - pr, #visit p)] end
      else if (hour < 18.0) then
        [(1.0, #visit p)]
      else if (18.0 <= hour andalso hour <= 22.0) then
        let val pr = 1.0/(4.0*hours - (rI step - 18.0*hours))
        in [(pr, home()), (1.0 - pr, #visit p)] end
      else
        [(1.0, home())]
    else
      if (hour < 18.0) then
        let 
          val n  = Real.fromInt (length (#belong p))
          val pr = (1.0 times per 1.0 days) / n
        in 
          (1.0 - n*pr, #visit p) :: map (fn place => (pr, place)) (#belong p)
        end
      else 
        [(1.0, home())]
  end
   
  (* 学生のスケジュール *)
  fun schedStu (PERSON p, t:time): (real * place_t) list = let
    val Student = #role p
    val {day,weekday,hour,step} = timecomp t
    fun home() = valOf (List.find (fn {place_k = Home, ...} => true | _ => false) (#belong p))
    fun sch()  = valOf (List.find (fn {place_k = Sch, ...} => true | _ => false) (#belong p))
  in
    if (1 <= weekday andalso weekday <= 5) then
      if (6.0 <= hour andalso hour <= 8.0) then
        let val pr = 1.0/(2.0*hours - (rI step - 6.0*hours))
        in [(pr, sch()), (1.0 - pr, #visit p)] end
      else if (hour < 15.0) then
        [(1.0, #visit p)]
      else if (16.0 <= hour andalso hour <= 19.0) then
        let val pr = 1.0/(3.0*hours - (rI step - 16.0*hours))
        in [(pr, home()), (1.0 - pr, #visit p)] end
      else
        [(1.0, home())]
    else
      if (hour < 18.0) then
        let 
          val n  = Real.fromInt (length (#belong p))
          val pr = (1.0 times per 1.0 days) / n
        in 
          (1.0 - n*pr, #visit p) :: map (fn place => (pr, place)) (#belong p)
        end
      else 
        [(1.0, home())]
  end

  (* 主婦のスケジュール *)
  fun schedHaus (PERSON p, t:time): (real * place_t) list = let
    val Hausfrau = #role p
    val {day,weekday,hour,step} = timecomp t
    fun home() = valOf (List.find (fn {place_k = Home, ...} => true | _ => false) (#belong p))
  in
    if (6.0 <= hour andalso hour <= 8.0) then
      [(1.0, home())]
    else if (hour < 18.0) then
      (* 行ける場所に平均1回ずつ行く *)
      let
        val n  = Real.fromInt (length (#belong p))
        val pr = 1.0 times per 10.0 hours 
      in 
       (1.0 - n*pr, #visit p) :: map (fn place => (pr, place)) (#belong p)
      end
    else
      [(1.0, home())]
  end

  (* 人間生成ルール *)
  fun rulePerson id = let
    val age    = Real.abs (30.0 + 30.0*rgauss rnd)
    val gender = rndsel rnd 0.5 (F,M)
    val (role,sched)   = 
      if (age <= 22.0)     then (Student, schedStu)
      else if (age < 60.0) then
        case gender
          of F => rndsel rnd 0.6 ((Employed,schedEmp), (Hausfrau,schedHaus))
           | M => rndsel rnd 0.8 ((Employed,schedEmp), (Hausfrau,schedHaus))
      else
        (Hausfrau,schedHaus)
  in
    {age = age, gender = gender, role = role, sched = sched}
  end

  (* 家族構成ルール *)
  fun ruleHome () = 
    List.tabulate (1 + iR (abs (2.0 * rgauss rnd)), fn _ => fn _ => true)

  (* 公共空間生成ルール *)
  fun rulePlace' size betaN place_t =
    {id    = place_t 
    ,nVis  = zeroNVis ()
    ,pTrns = zeroPTrns ()
    ,size  = iR o abs <> rI size + rI size*rgauss rnd
    ,betaN = abs (betaN + betaN*rgauss rnd)
    }: place

  fun rulePlace place_k size betaN area_t id = 
    rulePlace' size betaN {place_k = place_k, area_t = area_t, id = id}

  (* 行動範囲ルール *) 
  fun ruleVisit (areas: area vector)(PERSON p: person): place_t list = let
    val i0 = areaPer (PERSON p)
    val (_,_,a0) = areas $ i0
    val (_,_,a') = rndselV areas
  in
    case #role p 
      of Employed => map #id [rndselV (#corp a') , rndselV (#super a0)]
       | Student  => map #id [rndselV (#sch a0)  , rndselV (#super a0)]
       | HausFrau => map #id [rndselV (#super a0)]
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
  fun per ()     = F.makePerson 3000 rulePerson
  fun perHome at = F.makeHome at betaNHome (per ()) ruleHome
  fun place   at home = 
    F.makePlace at home
      [(10,rulePlace Sch  100  betaNSch)
      ,(10,rulePlace Super 20  betaNSuper)
      ,(19,rulePlace Corp  15  betaNCorp)
      ]
  fun infect (area:area) = 
    if (#1 area = JOJ) 
      then F.distInfect rnd area [(e0_JOJ,EXP 0)]
      else area
  val area = 
    Vector.map (fn at => 
      (fn (per,home) => 
        infect (at,per,place at home)
      ) (perHome at)) 
        #[HAC, TAC, JOJ, SJK, TKY]
  val train = F.makeTrains 
    {time2next = time2next, services = services, betaN = 0.5, size = 200}

  val city  = F.evalPlace
    {area = F.makeVisit' area ruleVisit
    ,train = train
    ,time = 0
    }
  (* 3000人 、1日で、45[sec] 
     150万人、1日で、6.25[時間]    (実スケール)
     15万人 、1日で、37.5[分]      (1/10スケール)
     15万人 、1週間で、4.375[時間] (1/10スケール)
     15万人 、6か月で、4.6875[日]  (1/10スケール)
   * *)
  (* 設定の書き出し、とても てんたてぃぶ *)
  fun writeConf f = let
    val os = T.openOut f
  in
    (T.output(os, "alpha," ^ fG 14 Type.alpha ^ "\n")
    ;T.output(os, "gamma," ^ fG 14 Type.gamma ^ "\n")
    ;T.output(os, "betaNHome," ^ fG 14 betaNHome ^ "\n")
    ;T.output(os, "betaNSch," ^ fG 14 betaNSch ^ "\n")
    ;T.output(os, "betaNCorp," ^ fG 14 betaNCorp ^ "\n")
    ;T.output(os, "betaNSuper," ^ fG 14 betaNSuper ^ "\n")
    ;T.output(os, "e0_JOJ," ^ fI e0_JOJ ^ "\n")
    ;T.closeOut os
    )
  end

  fun run1 recstep tStop file city = let
    val () = writeConf ("conf_" ^ file ^ ".csv")
    val os = T.openOut ("pop_" ^ file ^ ".csv")
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

  
  fun run2 recstep tStop file (*city*) = let
    val npick = 20
    val () = writeConf ("conf_" ^ file ^ ".csv")
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
    Iterator.applyN step nstep city 
      before T.closeOut os
  end
end

(* 可視化について 
* 初期のデバッグには、個人がどう動いたかが重要なので、単純に (時刻,都市) の
  プロットをつくるのがよいだろう。はじめはぬきうち検査で。ただし、「電車」
  にいるときは、電車の現在ではなく、「電車」という仮想都市にいることにする
  のが良いだろう。
*)


