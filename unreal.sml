(*
(1) 性別・年齢別人口分布に従って「裸の人間」たちを生成する
(2) (1)のメンバに社会的役割 (Employed | Hausfrau | Student)を与える
----
(3) 空間I = (家庭 | 学校 | 地域社会)
    空間II = 「第四空間」 = (スーパー | 電車 | ...)
 を生成する
----
(4) 時刻0において、(2)のメンバに空間Iを割り当てる
----
(5) 各社会的的役割と空間を参照するスクリプト
に従って、(4)の各メンバを動かす

 (1),(2),(4)がポピュレーションに関すること、(3)が環境、
(5)がシミュレーションに対応する。エージェントの要素は、

「人間」 = 裸の人間 * 社会的役割 * 所属空間 (⊆空間I) * 現在空間 (⊆空間I∪空間II)

---
可視化
  GIS = Geographi Information System を調べるとよい。
    商用ソフト。APIのセットがあったりする。曼荼羅。

戻ってくる表現:
---------------
x_t ∈ {Home, Corp, ...}
p(x_t=Home|x_{t-1}=Corp,t) などを要素とする確率表を与える
*)
structure Type = struct
  fun op <> (x,y) = x y; infix 1 <>;
  val op $ = Vector.sub; infix 9 $;
  exception Undef;
  fun undef () = raise Undef;
  type rnd = Random.rand;
  
  (* ============================================================== *)
  (* 差しあたって定数とするもの *)
  val alpha = 1.0/3.5
  val gamma = 1.0/3.0

  (* 時計関係 *)
  type time = int (* in steps *)
  val dt = 1.0/1440.0 (* [day] = 1 [min] *)
  val steps_per_day = Real.round (1.0/dt)

  (* 時間の単位 (in steps) *)
  val days    = 1.0/dt
  val hours   = 1.0/24.0/dt
  val minutes = 1.0/24.0/60.0/dt

  fun timecomp (t:time) = let
    val step    = t mod steps_per_day
    val day     = t div steps_per_day
  in{ day     = day
    , weekday = day mod 7  (* Sun:0 & Sat:6 *)
    , hour    = 24.0*dt*Real.fromInt step
    , step    = step
    }
  end
 
  fun per n uni = 1.0/n/uni
  infix 7 times
  fun op times(x,y) = x * y: real

  (* ============================================================== *)
  (* 人間 *)
  type age  = real
  datatype gender  = F | M
  datatype role    = Employed | Hausfrau | Student
  datatype health
    = SUS         (* 免疫なし *)
    | EXP of time (* 時刻 _で感染した *)
    | INF of time (* 時刻 _で発症した *)
    | VAC of time (* 時刻 _にワクチンを接種した *)
    | REC of time (* 時刻 _に回復(免疫獲得)した *)

  (* 空間 *)
  type id   = int
  type size = int
  (* datatype area = HAC | TAC | JOJ | SJK | TKY *)
  (* 数として扱いたいかもしれないので *)
  type area_t = int
    val HAC = 0:area_t
    val TAC = 1:area_t
    val JOJ = 2:area_t
    val SJK = 3:area_t
    val TKY = 4:area_t

  datatype place_k = Sch | Corp | Home | Super | Park | Train
  type place_t = {place_k: place_k, area_t: area_t, id: id}
  type nVis  = {s: size ref, e: size ref, i: size ref, r: size ref, v: size ref}
    fun zeroNVis (): nVis = {s = ref 0, e = ref 0, i = ref 0, r = ref 0, v = ref 0}
    fun clearNVis ({s,e,i,r,v}:nVis) = (s := 0; e := 0; i := 0; r := 0; v := 0)
  type pTrns = {s2e: real, e2i: real, i2r: real}
    fun zeroPTrns () = {s2e = 0.0, e2i = 0.0, i2r = 0.0}
  type place = {id: place_t, nVis: nVis, size: size, betaN: real, pTrns:pTrns}
  type mobil = {id: place_t, nVis: nVis, size: size, betaN: real, pTrns:pTrns
               ,iSked: int, sked: {time:time, area_t:area_t} vector}
  (* 感染効率にβではなくβNを使う理由
     - 今回はSEIRと違い、小集団ごとの人口Nは時々刻々と変化するので、
       スケールフリーなβNを使う必要がある。
     - なお、βN ～ O(1)である。
   * 言語の特性からくる妥協
     OOP風に云えば、placeを継承してmobilを定義したいところだが、SMLは
     SML#とちがって、「すくなくともこのフィールドを持つレコード型」が
     描けないので、 コンポジションとしてplaceを含む方定義にしている。

     と思ったが、継承もコンポジションも必要ないことがわかったので、
     単にフィールド追加にする。
   *)
  datatype person = PERSON of
    { age   : age
    , gender: gender
    , role  : role
    , belong: place_t list
    , visit : place_t
    , dest  : place_t option
    , health: health list
    , sched: person * time -> (real * place_t) list
    }
  fun deP (PERSON p) = p
  (* 中間構造体 *)
    (* type per1 = {age: age, gender: gender} *)
    type per2 = {age: age, gender: gender, role: role, sched: person * time -> (real * place_t) list}
    (* type per3 = {age: age, gender: gender, role: role, belong: place_t list} *)

  type places = 
    { sch  : place vector
    , corp : place vector
    , park : place vector
    , super: place vector
    , home : place vector
    }
    (* 中間構造体 *)
    type places1 =
      { sch  : place vector
      , corp : place vector
      , park : place vector
      , super: place vector
      }
 (* ToDo: どこかで #id area = index of vector を保証しないといけない *)
  type area = id * person list * places
  type city =
    {area : area vector  
    ,train: mobil vector
    ,time : time
    }
  (* アクセサ *)
  fun projHome x = 
    valOf (List.find (fn (p:place_t) => #place_k p = Home) x)
  fun areaPer (PERSON {belong, ...}:person) = #area_t (projHome belong)
  fun localPer (PERSON p:person) = let
    val i = areaPer (PERSON p)
  in
    List.filter (fn plc => i = #area_t plc) (#belong p)
  end
  (* place_t で指定される場所を全都市から探す *)
  fun placeAreas (areas: area vector) ({area_t,place_k,id}: place_t) =
    case place_k 
      of Home  => (#home  o #3) (areas $ area_t) $ id
       | Corp  => (#corp  o #3) (areas $ area_t) $ id
       | Sch   => (#sch   o #3) (areas $ area_t) $ id
       | Park  => (#park  o #3) (areas $ area_t) $ id
       | Super => (#super o #3) (areas $ area_t) $ id
       | Train => raise undef () (* unreachable *)
end

structure Frame = struct
  open Type; infix 1 <>; infix 1 $;
  open X_Misc;
  open EasyPrint; infix 1 <<
  (* ============================================================== *)
  fun cnt () =
    (fn n => fn () => !n before n := !n + 1 ) (ref 0)

  (* 1. 人口生成 *)
  fun makePerson (n:int)(rule: id -> per2): per2 list = List.tabulate(n, rule)

  (* 2. 家族構成 *)
  fun makeHome (area_t: area_t)
               (betaN : real)
               (persons:per2 list)
               (rule: unit -> (per2 -> bool) list)
               :person list * place vector = 
  let
    fun grouping (ps:per2 list): per2 list list = let
      fun op <+> (x,(SOME u,v)) = (u::x,v)
        | op <+> (x,(NONE,  v)) = (x, v)
      infix 1 <+>
      fun group1 (ps: per2 list) = 
        case foldl (fn (cond,(mem,pop)) => mem <+> findpop cond pop) (nil,ps) (rule())
          of (nil, y::ys) => ([y], ys) (* １人は取れることを保証 *)
           | (xs , ys)    => (xs , ys)
      fun loop pps nil = pps
        | loop pps (ps:per2 list)  =
        (fn (qs,ps) => loop (qs::pps) ps) (group1 ps)
    in
      loop nil ps
    end
    val idx = cnt ()
    fun home (ps:per2 list): (person list) * place = let
      val hm: place = 
        {id    = {area_t = area_t, place_k = Home, id = idx ()}
        ,nVis  = zeroNVis()
        ,pTrns = zeroPTrns()
        ,size  = length ps
        ,betaN = betaN
        }
      val ps: person list = 
        map (fn p => PERSON
          {age    = #age p
          ,gender = #gender p
          ,role   = #role p
          ,visit  = (#id hm)
          ,belong = (#id hm) :: nil
          ,dest   = NONE
          ,health = SUS :: nil
          ,sched = #sched p
          }
        ) ps 
    in
      (ps,hm)
    end
    val unzip = (fn (per,pl) => (List.concat per,Vector.fromList pl)) o ListPair.unzip
  in
    unzip (map home (grouping persons))
  end

  (* 3. 公共空間生成 *)
  fun makePlace (area_t:area_t) 
                (home:place vector)
                (rules: (size * (area_t -> id -> place)) list)
                :places =
  let
    val places = 
      List.concat (map (fn (n,rule) => List.tabulate(n, rule area_t)) rules)
    fun is x (p:place) = #place_k (#id p) = x
    fun filter c = Vector.fromList o (List.filter c)
  in
    {sch   = filter (is Sch  ) places
    ,corp  = filter (is Corp ) places
    ,super = filter (is Super) places
    ,park  = filter (is Park ) places
    ,home  = home
    }
  end

  (* 4. 行動範囲 *)
  fun makeVisit 
    {area  : area vector
    ,belong: area vector -> person -> place_t list}
    :area vector = 
  let
    fun ext (PERSON x:person) = PERSON
      {age = #age x
      ,gender = #gender x
      ,role   = #role x
      ,belong = belong area (PERSON x) @ #belong x
      ,visit  = #visit x
      ,dest   = #dest x
      ,health = #health x
      ,sched = #sched x
      }
    fun asg (id, pop, places) =
      (id, map ext pop, places)
  in
    Vector.map asg area
  end

  fun makeVisit' area belong = makeVisit {area=area,belong=belong}


  (* 5. 鉄道 *)
  (* ∀A∀B.A駅(町)～B駅(町)の所要時間表を次の駅までの所要時間からつくる *)
  (* ... なんかえらい苦労した ... *)
  fun genReqtime (tm2s: (area_t * time) list) = let
    infix 9 $; val op $ = Vector.sub
    val n = 1 + foldl (fn (a,s) => Int.max(s, #1 a)) 0 tm2s
    fun rdu (a, nil) = a
      | rdu (a, b) = 
        a @ rdu (ListPair.map (fn ((i,_,t),(_,j,t')) => (i,j, t + t')) (a, b), tl b)
    val lst0= ListPair.map (fn ((i,t),(j,t')) => (i,j, t)) (tm2s, tl tm2s)
    val lst = rdu (lst0, tl lst0)
    val arr = Vector.tabulate(n, fn i => Vector.tabulate(n,fn j => ref 0))
    val ()  = app (fn (i,j,t) => (arr$ i $ j := t; arr $j $ i := t)) lst 
  in
    Vector.map (Vector.map (op !)) arr
  end

  fun makeTrains
    {time2next: (area_t * time) list
    ,services: {deptime:time, stations: area_t vector} vector
    ,betaN: real
    ,size: size
    }: mobil vector =
  let
    val idx = cnt ()
    val req = genReqtime time2next
    infix 9 $; val op $ = Vector.sub
  in
    Vector.map (fn {deptime, stations} => 
        {id    = {place_k = Train, area_t = 0, id = idx ()}
        ,nVis  = zeroNVis()
        ,pTrns = zeroPTrns()
        ,size  = size
        ,betaN  = betaN
        ,iSked = 0
        ,sked  = Vector.map (fn st => 
          {area_t = st, time = deptime + req $ (stations $ 0) $ st}) stations
        }
    ) services
  end

  (* 6. 感染者のばらまき *)
  (* 【注意】これははっきりいって良い定式化が思い当たらない。ので、とりあえずテスト計算
   * がはじめられるように、アドほっくにつくる。
   *)
  (* distInfect:
   *  area 内で
   *  重複しないようにランダムに指定個を選び出し、指定された健康状態にする *)
  fun distInfect rnd (area:area) (rules: (size * health) list): area = let
    val per = #2 area
    val (modified, rest) = 
      foldl (fn ((n,stat),(modified, per)) => 
        let val (target,per) = uniqRndSample rnd per n in  
          (map 
            (fn (PERSON p:person) => PERSON
              { age    = #age p
              , gender = #gender p
              , role   = #role p
              , belong = #belong p
              , visit  = #visit p
              , dest   = #dest p
              , health = 
                  if (hd (#health p) = stat) 
                    then #health p
                    else stat :: tl (#health p)
              , sched = #sched p
              }:person
          ) target :: modified
          ,per)
        end
      ) (nil: person list list,per: person list) rules
  in
    (#1 area
    ,List.concat (rev (rest :: modified))
    ,#3 area)
  end
  
  (* ============================================================== *)
  (* シミュレーションエンジン *)
  val rnd = Random.rand(0,1)

  val succTr = ref 0
  val failTr = ref 0

  fun catchTrain' (train:mobil vector, now:time, src:area_t, dst:area_t): mobil option = let
    (* steps数単位での(日内)時刻,
     * 平日/土日で異なるダイアを使うときはここを変える必要がある *)
    val {step,...} = timecomp now 
  in
    Vector.find (fn tr => 
      Vector.exists (fn {time,area_t} => time =  step andalso area_t = src) (#sked tr) andalso 
      Vector.exists (fn {time,area_t} => time >= step andalso area_t = dst) (#sked tr)
    ) train
  end

  fun catchTrain z = 
    case catchTrain' z
      of SOME x => SOME x before succTr := !succTr + 1
       | NONE   => NONE   before failTr := !failTr + 1

  (* いま居る場所の感染効率から確率過程により健康状態を遷移させる *)
  fun doTransit ({area=areas,train,time}:city) (PERSON p: person): health list = let
    val pTrns =
      case #visit p 
        of {place_k = Train, id, area_t} => #pTrns (train $ id)
         | place_t                       => #pTrns (placeAreas areas place_t)
    val cur::hist = #health p
  in
    (* 注: 履歴のとり方に注意。リスト連結は、効率が悪いが、簡潔に書くにはこう。
     * rndsel rnd (#s2e pTrns) (EXP time, cur) :: cur :: hist ではない!!
     * という間違ったことをすると、10倍遅くなる。*)
    case cur
      of SUS   => rndsel rnd (#s2e pTrns) ([EXP time, cur],[cur]) @ hist
       | EXP _ => rndsel rnd (#e2i pTrns) ([INF time, cur],[cur]) @ hist
       | INF _ => rndsel rnd (#i2r pTrns) ([REC time, cur],[cur]) @ hist
       | _     => cur :: hist
  end

  (* 人間の振る舞いのこのプログラムが「お仕着せる」部分。核である *)

  (* その前に、[(pr,x) ...] から確率prでxを取るルーチンを書け *)

  fun evalPerson (city:city) (PERSON p: person) = let
    val {area=areas,train,time} = city
    fun movetrns {visit, dest} = PERSON
      { age    = #age p, gender = #gender p, role = #role p, belong = #belong p
      , sched  = #sched p
      , visit  = visit
      , dest   = dest     
      , health = doTransit city (PERSON p)
      }
    fun trns() = movetrns {visit = #visit p, dest = #dest p}
    val myArea = #area_t (#visit p)
  in
    case #dest p
      of NONE => 
        let
          (* ランダムに行き先を選び、町内であればそこへ、でなければ「駅」に向かう *)
          (* val dest = rndSelL rnd (#belong p) *)
          val dest = rndSelLP rnd (#sched p (PERSON p, time))
        in
          if (#area_t dest = myArea)
            then movetrns {visit = dest, dest = NONE}
            else movetrns {visit = #visit p, dest = SOME dest} end
       | SOME dest =>
          (* 乗れる電車があれば乗る。そして、目的地でおりる *)
          if (#place_k (#visit p) = Train) then 
            if (#area_t (#id (train $ #id (#visit p))) = #area_t dest) then
              movetrns {visit = dest, dest = NONE}
            else
              trns () 
          else (case catchTrain (train, time, myArea, #area_t dest)
            of SOME tr => movetrns {visit = #id tr, dest = SOME dest}
             | NONE    => trns ()
          )
  end

  fun evalTrain (city:city) (tr:mobil) = let
    val {time = time', area_t = area_t'} = #sked tr $ (#iSked tr)
    val steps_per_day = Real.round (1.0/dt)
    val steps   = (#time city) mod steps_per_day 
  in
    if (steps = time') then
      {id    = {place_k = Train, id = #id (#id tr), area_t = area_t'}
      ,nVis  = #nVis tr
      ,pTrns = #pTrns tr
      ,size  = #size tr
      ,betaN  = #betaN tr
      ,iSked = (#iSked tr + 1) mod (Vector.length (#sked tr))
      ,sked  = #sked tr
      }
    else
      tr
  end

  (* SEIRモデルに基づく、場所毎の各健康状態にいる人口の推移の予測 *)
  fun estTransitP (p:place): place = let
    (* val n = Real.fromInt (#size p)   (* これはバグってハニー!! *) *)
    val s = Real.fromInt (!(#s(#nVis p)))
    val e = Real.fromInt (!(#e(#nVis p)))
    val i = Real.fromInt (!(#i(#nVis p)))
    val r = Real.fromInt (!(#r(#nVis p)))
    val v = Real.fromInt (!(#v(#nVis p)))
    val n = s + e + i + r + v
    val beta = if (n > 0.0) then #betaN p / n else 0.0
  in
    {id = #id p, nVis = #nVis p, size = #size p, betaN = #betaN p
    ,pTrns =
      {s2e = beta * i * dt  (* 意味がわかりやすい式は、 betaN * (i/n) * dt *)
      ,e2i = alpha * dt (* 発症は自発的だから *e はおかしい *)
      ,i2r = gamma * dt
      }
    }
  end
  fun estTransitM (p:mobil): mobil = let
    val s = Real.fromInt (!(#s(#nVis p)))
    val e = Real.fromInt (!(#e(#nVis p)))
    val i = Real.fromInt (!(#i(#nVis p)))
    val r = Real.fromInt (!(#r(#nVis p)))
    val v = Real.fromInt (!(#v(#nVis p)))
    val n = s + e + i + r + v
    val beta = if (n > 0.0) then  #betaN p / n else 0.0
  in
    {id = #id p, nVis = #nVis p, size = #size p, betaN = #betaN p
    ,iSked = #iSked p, sked = #sked p
    ,pTrns =
      {s2e = beta * i * dt 
      ,e2i = alpha * dt
      ,i2r = gamma * dt
      }
    }
  end

  fun estTransit ({area,train,time}:city): city = 
     {area = Vector.map (fn (id,persons,places) => 
          (id
          ,persons
          , {sch   = Vector.map estTransitP (#sch   places)
            ,corp  = Vector.map estTransitP (#corp  places)
            ,park  = Vector.map estTransitP (#park  places)
            ,super = Vector.map estTransitP (#super places)
            ,home  = Vector.map estTransitP (#home  places)
            }
          )
       ) area
     ,train = Vector.map estTransitM train
     ,time = time
     }

  (* 個人の移動の結果を、場所毎の人口に反映させる *)
  fun evalPlace (city:city) = let
    val {area=areas,train,time} = city
    val vapp = Vector.app

    fun addVis (PERSON p:person) = let
      val nVis =
        case #visit p 
          of {place_k = Train, id, area_t} => #nVis (train $ id)
           | others => #nVis (placeAreas areas others)
    in
      case hd (#health p)
        of SUS   => #s nVis := !(#s nVis) + 1
         | VAC _ => #v nVis := !(#v nVis) + 1
         | EXP _ => #e nVis := !(#e nVis) + 1
         | INF _ => #i nVis := !(#i nVis) + 1
         | REC _ => #r nVis := !(#r nVis) + 1
    end
       
    fun cntPersons ((id, persons, places):area) = 
      ( app (fn f => vapp (fn x => clearNVis (#nVis x)) (f places)) 
          [#sch, #corp, #park, #super, #home]
      ; app addVis persons
      )
  in
    (vapp cntPersons areas
    ;estTransit city)
  end

  fun advanceTime (city:city): city = 
    evalPlace
      {area  = Vector.map 
        (fn (id,persons,places) => 
          (id, map (evalPerson city) persons, places)) (#area city)
      ,train = Vector.map (evalTrain city) (#train city)
      ,time  = #time city + 1
      }
  fun mtime f = let
    val t0 = Time.now()
  in
    f () before 
      (print (Time.toString (Time.- (Time.now(), t0)))
      ;print "\n")
  end
end

structure Probe = struct
  open Type
  open EasyPrint
  structure T = TextIO
  infix 9 $
  infix 1 <>
  fun isStat (h:health)(PERSON p:person) = 
    case (h,hd (#health p)) 
      of (SUS  ,SUS  ) => true
       | (EXP _,EXP _) => true
       | (INF _,INF _) => true
       | (REC _,REC _) => true
       | (VAC _,VAC _) => true
       | _             => false
  fun reducePop ((_,ps,_):area) = 
    {s = length o List.filter (isStat SUS    ) <> ps
    ,e = length o List.filter (isStat (EXP 0)) <> ps
    ,i = length o List.filter (isStat (INF 0)) <> ps
    ,r = length o List.filter (isStat (REC 0)) <> ps
    }
  fun reducePop' (city:city) = 
    Vector.map reducePop (#area city)

  fun showPopTag os n = 
    (T.output(os,"t,")
    ;app (fn i => app (fn x => T.output(os, x ^ fI i ^ ",")) ["s","e","i","r"])
      (List.tabulate(n,fn i => i+1))
    ;T.output(os,"\n"))
  fun showPop os (t:time) (pop:{s:int,e:int,i:int,r:int} vector) = 
    (T.output(os,fI t ^ ",")
    ;Vector.app (fn {s,e,i,r} => app (fn x => T.output(os, fI x ^ ",")) [s,e,i,r]) pop
    ;T.output(os,"\n"))

  fun getPerson (city:city) (area_t:area_t) (idx:id) = 
    List.nth(#2 (#area city $ area_t), idx)
(*
  fun showPlace_k Sch   = "Sch"
    | showPlace_k Corp  = "Corp"
    | showPlace_k Home  = "Home"
    | showPlace_k Super = "Super"
    | showPlace_k Park  = "Park"
    | showPlace_k Train = "Train"
 *)
  fun showPlace_k Sch   = "1"
    | showPlace_k Corp  = "2"
    | showPlace_k Home  = "3"
    | showPlace_k Super = "4"
    | showPlace_k Park  = "5"
    | showPlace_k Train = "6"

  fun showPlace_t ({area_t,id,place_k}:place_t) = 
    fI area_t ^ "," ^ fI id ^ "," ^ showPlace_k place_k ^ ","
end

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


