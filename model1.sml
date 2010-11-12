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
  val rnd = Random.rand (0,1);
  fun rndselV (v: 'a vector) = 
    v $ (Int.mod (Random.randInt rnd, Vector.length v))

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
    ,e0_JOJ    : int
    ,nPop      : int (* ひとつの街の人口。後で配列にする *)
    }
  (* 設定例 *)
  val conf0: conf =
    {betaNHome  = 1.4 * gamma
    ,betaNSch   = 1.8 * gamma
    ,betaNSuper = 1.4 * gamma
    ,betaNCorp  = 1.8 * gamma
    ,betaNPark  = 0.5 * gamma
    ,e0_JOJ     = 30
    ,nPop       = 3000
    }

  (* テストデータ *)
  val frau = let
    val vis = {place_k = Home , area_t = 0, id = 0, tVis = ref ~1}: place_t
  in PERSON 
    {age=23.0:age, gender = F
    ,belong = [ {place_k = Super, area_t = 0, id = 0, tVis = ref ~1}
              , {place_k = Park , area_t = 0, id = 0, tVis = ref ~1}
              , vis
              ]: place_t list
    ,visit  = vis
    ,role   = Hausfrau
    ,dest   = NONE: place_t option
    ,health = nil: health list
    ,genSched  = fn (_:person*time) => nil: (real*place_t) list
    }
  end

  (* 人口分布に似た簡単な分布がある:   クラーク分布 *)
  (* ====================================================================  *)
  (* 生成ルール *)

  (* 行動テンプレート *)
  (* 次のような振る舞い
   * 会社員: 月〜金は会社へ、土日は会社以外に適当に
   * 主婦  : 一日n回はスーパーに行く。
   *
   * ステップ数 nsteps => 物理的時間
   *   分に  : nsteps * minutes
   *   時間に: nsteps * hours
   *   日に  : nsteps * days
   *
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
        let val pr = 1.0/(4.0*hours - (rI step - 6.0*hours)) 
        in  [(pr, corp()), (1.0 - pr, #visit p)] end
      else if (hour < 18.0) then
        [(1.0, #visit p)]
      else if (18.0 <= hour andalso hour <= 22.0) then
        let val pr = 1.0/(4.0*hours - (rI step - 18.0*hours))
        in [(pr, home()), (1.0 - pr, #visit p)] end
      else
        [(1.0, home())]
    else
      if (hour < 18.0) then
        let val n  = Real.fromInt (length (#belong p))
            val pr = (1.0 times per 1.0 days) / n
        in  (1.0 - n*pr, #visit p) :: map (fn place => (pr, place)) (#belong p) end
      else 
        [(1.0, home())]
  end
   
  (* 学生のスケジュール *)
  (* マルコフ的な記述だと管理がとても面倒。 
   *   日のはじめに、行動を決定する方式の方が自然書けるはず。
   *   書きなおしてみよ。
   * *)
  
  fun schedStu (PERSON p, t:time): (real * place_t) list = let
    val Student = #role p
    val {day,weekday,hour,step} = timecomp t
    fun home() = valOf (List.find (fn {place_k = Home, ...} => true | _ => false) (#belong p))
    fun sch()  = valOf (List.find (fn {place_k = Sch , ...} => true | _ => false) (#belong p))
    fun cram() = List.find (fn {place_k = Cram, ...} => true | _ => false) (#belong p)
  in
    if (1 <= weekday andalso weekday <= 5) then
      if (6.0 <= hour andalso hour <= 8.0) then
        let val pr = 1.0/(8.0*hours - rI step)
        in [(pr, sch()), (1.0 - pr, #visit p)] end
      else if (hour < 15.0) then
        [(1.0, #visit p)]
      else if (16.0 <= hour andalso hour <= 19.0) then
        let val pr = 1.0/(19.0*hours - rI step)
        in [(pr, home()), (1.0 - pr, #visit p)] end
      else if (20.0 <= hour andalso hour < 22.0) then
        case cram()
          of SOME cram => [(1.0, cram) ]
           | _         => [(1.0, home())]
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

  fun schedStu2 (PERSON p, t:time): (real * place_t) list = let
    val Student = #role p
    val {day,weekday,hour,step} = timecomp t
    fun home() = valOf (List.find (fn {place_k = Home, ...} => true | _ => false) (#belong p))
    fun sch()  = valOf (List.find (fn {place_k = Sch , ...} => true | _ => false) (#belong p))
    fun cram() = List.find (fn {place_k = Cram, ...} => true | _ => false) (#belong p)
  in
    if (1 <= weekday andalso weekday <= 5) then
      if (6.0 <= hour andalso hour <= 8.0) then
        let val pr = 1.0/(8.0*hours - rI step)
        in [(pr, sch()), (1.0 - pr, #visit p)] end
      else if (hour < 15.0) then
        [(1.0, #visit p)]
      else if (16.0 <= hour andalso hour <= 19.0) then
        let val pr = 1.0/(19.0*hours - rI step)
        in [(pr, home()), (1.0 - pr, #visit p)] end
      else if (20.0 <= hour andalso hour < 22.0) then
        case cram()
          of SOME cram => [(1.0, cram) ]
           | _         => [(1.0, home())]
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
    val {day,weekday,hour,step} = timecomp t
    val Hausfrau = #role p
    local
      val nH = ref 0
      val nS = ref 0
      val nO = ref 0
      fun k ({place_k,...}:place_t) = place_k
      fun inc Home  = nH := !nH + 1
        | inc Super = nS := !nS + 1
        | inc _     = nO := !nO + 1
      val () = app (inc o k) (#belong p)
    in
      val nH = rI (!nH)
      val nS = rI (!nS)
      val nO = rI (!nO)
    end

    (* スーパーを最後に訪問した日付 *)
    fun isSuper ({place_k,...}: place_t) = place_k = Super
    val dayVisitedSuper = 
      foldl Int.max ~1 
        o map (fn {tVis,...} => !tVis div steps_per_day)
        o List.filter isSuper 
        @@ (#belong p)
  in
    if (6.0 <= hour andalso hour <= 8.0) then
      map (fn dst => if (#place_k dst = Home) then (1.0/nH,dst) else (0.0,dst)) (#belong p)
    else if (8.0 <= hour andalso hour < 18.0) then
      let
        val tVis = !(#tVis (#visit p))
        val prTo = 
          case #place_k (#visit p) (* namely, src *)
            of Home  =>
              let
                (* まず、カテゴリ別の遷移確率を計算する。
                 * カテゴリとして家庭、スーパー、その他の3つを考える *)
                val pHS = if day > dayVisitedSuper then 1.0/(18.0*hours - rI step) else 0.0
                val pHO = 1.0/5.0/hours
                val pHH = 1.0 - pHO - pHS
              in
                (* つぎに、同一カテゴリのノード間で等分する *)
                fn (dst:place_t) => case #place_k dst
                  of Home  => pHH/nH
                   | Super => pHS/nS
                   | _     => pHO/nO
              end
             | Super => 
              let 
                val pSX = 1.0/(30.0*minutes - rI (t - tVis))
                val pSO = 0.1*pSX (* Super1 -> Super2 も含む *)
                val pSH = 0.9*pSX
                val pSS = 1.0 - pSO - pSH
                val nO' = nO + nS - 1.0
              in
                fn (dst:place_t) => case #place_k dst
                  of Home  => pSH/nH
                   | Super => 
                       (* 同一カテゴリ間の遷移の場合は、自己遷移とそれ以外を区別する必要がある。
                        * さらに、「別のスーパー」は便宜的に「その他の場所」として扱っている。*)
                       if (#id (#visit p) = #id dst) 
                         then pSS
                         else pSO/nO'
                   | _     => pSO/nO'
              end
             | _     => 
              let
                val pOH = 1.0/(3.0*hours - rI (t - tVis))
                val pOO'= 1.0/2.0/hours
                val pOS = if day > dayVisitedSuper then 1.0/(18.0*hours - rI step) else 0.0
                val pOO = 1.0 - pOH - pOS - pOO'
                val nO' = nO - 1.0
              in
                fn (dst:place_t) => case #place_k dst
                  of Home  => pOH/nH
                   | Super => pOS/nS
                   | _     =>
                       if (#id (#visit p) = #id dst) 
                         then pOO
                         else pOO'/nO'
              end
      in
        map (fn dst => (prTo dst, dst)) (#belong p)
      end
    else
      map (fn dst => if (#place_k dst = Home) then (1.0/nH,dst) else (0.0,dst)) (#belong p)
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
    {age = age, gender = gender, role = role, genSched = sched}
  end

  (* 家族構成ルール *)
  fun ruleHome () = 
    List.tabulate (1 + iR (abs (2.0 * rgauss rnd)), fn _ => fn _ => true)

  (* 公共空間生成ルール *)
  fun rulePlace' size betaN place_t =
    {id    = place_t 
    ,nVis  = zeroNVis ()
    ,pTrns = zeroPTrns ()
    ,size  = iR o abs @@ rI size + rI size*rgauss rnd
    ,betaN = abs (betaN + betaN*rgauss rnd)
    }: place

  fun rulePlace place_k size betaN area_t id = 
    rulePlace' size betaN {place_k = place_k, area_t = area_t, id = id, tVis = ref ~1}

  (* 行動範囲ルール *) 
  fun ruleVisit (areas: area vector)(PERSON p: person): place_t list = let
    val i0 = areaPer (PERSON p)
    val (_,_,a0) = areas $ i0
    val (_,_,a') = rndselV areas
    (* アイデア:
     *   行き先の個数も、バラツカセタラ? *)
  in
    case #role p 
      of Employed => map #id [rndselV (#corp a') , rndselV (#super a0)]
       | Student  => map #id [rndselV (#sch a0)  , rndselV (#super a0)]
       | HausFrau => map #id [rndselV (#super a0), rndselV (#park a0) ]
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
  fun perHome (conf:conf) at = 
    F.makeHome at (#betaNHome conf) (F.makePerson (#nPop conf) rulePerson) ruleHome

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

  fun infect (conf:conf)(area:area) = 
    if (#1 area = JOJ) 
      then F.distInfect rnd area [(#e0_JOJ conf,EXP 0)]
      else area

  fun area (conf:conf) = 
    Vector.map (fn at => 
      (fn (per,home) => 
        infect conf (at, per, place conf at home)
      ) (perHome conf at)) 
        #[HAC, TAC, JOJ, SJK, TKY]
  val train = F.makeTrains 
    {time2next = time2next, services = services, betaN = 0.5, size = 200}

  fun city (conf:conf) = 
    F.evalPlace
      {area = F.makeVisit' (area conf) ruleVisit
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
  fun writeConf (conf:conf) f = let
    val os = T.openOut f
  in
    (T.output(os, "alpha,"     ^ fG 14 Type.alpha ^ "\n")
    ;T.output(os, "gamma,"     ^ fG 14 Type.gamma ^ "\n")
    ;T.output(os, "betaNHome," ^ fG 14 (#betaNHome conf) ^ "\n")
    ;T.output(os, "betaNSch,"  ^ fG 14 (#betaNSch conf) ^ "\n")
    ;T.output(os, "betaNCorp," ^ fG 14 (#betaNCorp conf) ^ "\n")
    ;T.output(os, "betaNSuper,"^ fG 14 (#betaNSuper conf) ^ "\n")
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
  fun run1 conf recstep tStop file city = let
    val () = writeConf conf ("conf_" ^ file ^ ".csv")
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

(* 可視化について 
* 初期のデバッグには、個人がどう動いたかが重要なので、単純に (時刻,都市) の
  プロットをつくるのがよいだろう。はじめはぬきうち検査で。ただし、「電車」
  にいるときは、電車の現在ではなく、「電車」という仮想都市にいることにする
  のが良いだろう。
*)


