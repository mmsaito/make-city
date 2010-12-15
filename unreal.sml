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
  fun op @@ (x,y) = x y; infix 1 @@;
  val op $ = Vector.sub; infix 9 $;
  exception Undef;
  fun undef () = raise Undef;
  
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
  
  val days'   = Alice.iR days
  val hours'  = Alice.iR hours
  val minutes'= Alice.iR minutes

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

  datatype place_k = Cram | Sch | Corp | Home | Super | Park | Train

  (* 場所を特定するタプル *)
  (* 【実装上の注意】
     参照型のtVisはpersonのフィールドに現れるときのみ意味を持つ。
     これは、「あるひとがその場所に移動した時刻」を記述するためのトリック。
     in-place replaceを実現するために参照型になっている。
  *)
  type place_t = {place_k: place_k, area_t: area_t, id: id, tVis:time ref}
  (* なので、personレコードを完成するときにクローニングする必要がある *)
  fun clone_place_t {place_k: place_k, area_t: area_t, id: id, tVis:time ref} = 
    {place_k = place_k, area_t = area_t, id = id, tVis = ref (!tVis)}

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
    { age    : age
    , gender : gender
    , role   : role
    , belong : place_t list
    , visit  : place_t
    , dest   : place_t option
    , health : health list
    , mkSched: person * time -> (time * place_t) list
    , sched  : (time * place_t) list
    } 
    fun deP (PERSON p) = p
    (* 中間構造体 *)
    type per2 = 
      { age     : age
       , gender : gender
       , role   : role
       , mkSched: person * time -> (time * place_t) list
       }

  type places = 
    { cram : place vector
    , sch  : place vector
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
    (*
  fun localPer (PERSON p:person) = let
    val i = areaPer (PERSON p)
  in
    List.filter (fn plc => i = #area_t plc) (#belong p)
  end
  *)
  (* place_t で指定される場所を全都市から探す *)
  fun placeAreas (areas: area vector) ({area_t,place_k,id,...}: place_t) =
    case place_k 
      of Home  => (#home  o #3) (areas $ area_t) $ id
       | Corp  => (#corp  o #3) (areas $ area_t) $ id
       | Sch   => (#sch   o #3) (areas $ area_t) $ id
       | Cram  => (#cram  o #3) (areas $ area_t) $ id
       | Park  => (#park  o #3) (areas $ area_t) $ id
       | Super => (#super o #3) (areas $ area_t) $ id
       | Train => raise undef () (* unreachable *)

  (* 乱数のシードの設定 *)
  (* ここで設定するのは汚いが、延々と引数で引き回すのもあれなので、ここでやる *)
  local
    val rndref = ref NONE: Random.rand option ref
  in
    fun getrnd () =
      case (!rndref)
        of SOME r => r
         | NONE   => (print "model1.sml:20: random number is not initialized!  Using default!"
                     ;rndref := SOME (Random.rand (0,1))
                     ;valOf(!rndref)
                     )
    fun inirnd n = rndref := SOME (Random.rand(0,n))
  end
end

structure Frame = struct
  open Type; infix 1 @@; infix 9 $;
  open X_Misc;
  open EasyPrint; infix 1 <<
  (* ============================================================== *)
  fun cnt () =
    (fn n => fn () => !n before n := !n + 1 ) (ref 0)

  (* 1. 人口生成 *)
  fun makePerson (n:int)(rule: id -> per2): per2 list = List.tabulate(n, rule)

  (* 2. 家族構成 *)
  fun makeHome (area_t: area_t)
               (betaN : unit -> real) (* ランダム化を許すため *)
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
        {id    = {area_t = area_t, place_k = Home, id = idx (), tVis = ref ~1}: place_t
        ,nVis  = zeroNVis()
        ,pTrns = zeroPTrns()
        ,size  = length ps
        ,betaN = betaN ()
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
          ,mkSched = #mkSched p
          ,sched  = nil
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
  (* area_t: 場所を設置する街
   * home  : 先に作った家庭の集まり。
   * rules : 生成規則の集まり。各生成規則 (n,gen) に対して、
   *   n個の場所を genから生成する。
   *)
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
    {cram  = filter (is Cram ) places
    ,sch   = filter (is Sch  ) places
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
      ,belong = map clone_place_t (belong area (PERSON x) @ #belong x)
      ,visit  = #visit x
      ,dest   = #dest x
      ,health = #health x
      ,mkSched = #mkSched x
      ,sched   = #sched x
      }
    fun asg (id, pop, places): area =
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
        {id    = {place_k = Train, area_t = 0, id = idx (), tVis = ref ~1}
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
              , mkSched = #mkSched p
              , sched    = #sched p
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
  (*
  *)

  fun catchTrain (train:mobil vector, now:time, src:area_t, dst:area_t): mobil option = let
    (* steps数単位での(日内)時刻,
     * 平日/土日で異なるダイアを使うときはここを変える必要がある *)
    val {step,...} = timecomp now 
  in
    Vector.find (fn tr => 
      (* Vector.exists (fn {time,area_t} => time =  step andalso area_t = src) (#sked tr) andalso  *)
      let
        val {time,area_t} = #sked tr $ (#iSked tr)
      in
        time >= step andalso area_t = src
      end andalso
      Vector.exists (fn {time,area_t} => time >= step andalso area_t = dst) (#sked tr)
    ) train
  end
(*
  val succTr = ref 0
  val failTr = ref 0
  fun catchTrain z = 
    case catchTrain' z
      of SOME x => SOME x before succTr := !succTr + 1
       | NONE   => NONE   before failTr := !failTr + 1
*)
  (* いま居る場所の感染効率から確率過程により健康状態を遷移させる *)
  fun updateHealth ({area=areas,train,time}:city) (PERSON p: person): health list = let
    val rnd = getrnd()
    val pTrns =
      case #visit p 
        of {place_k = Train, id, ...} => #pTrns (train $ id)
         | place_t                    => #pTrns (placeAreas areas place_t)
    val cur::hist = #health p
  in
    (* 注: 履歴のとり方に注意。リスト連結は、効率が悪いが、簡潔に書くにはこう。
     * rndSel rnd (#s2e pTrns) (EXP time, cur) :: cur :: hist ではない!!
     * という間違ったことをすると、10倍遅くなる。*)
    case cur
      of SUS   => rndSel rnd (#s2e pTrns) ([EXP time, cur],[cur]) @ hist
       | EXP _ => rndSel rnd (#e2i pTrns) ([INF time, cur],[cur]) @ hist
       | INF _ => rndSel rnd (#i2r pTrns) ([REC time, cur],[cur]) @ hist
       | _     => cur :: hist
  end

  (* 人間の振る舞いのこのプログラムが「お仕着せる」部分。核である *)

  (* その前に、[(pr,x) ...] から確率prでxを取るルーチンを書け *)

  fun evalPerson (city:city) (PERSON p: person) = let
    val {area=areas,train,time} = city
    fun update {visit, dest, sched} = PERSON
      { age    = #age p, gender = #gender p, role = #role p, belong = #belong p
      , mkSched  = #mkSched p
      , sched  = sched
      , visit  = visit
      , dest   = dest     
      , health = updateHealth city (PERSON p)
      } 
      before
        (if #place_k visit <> #place_k (#visit p) 
             orelse #id visit <> #id (#visit p) then 
          #tVis visit := time else ())
    val myArea = #area_t (#visit p)
    fun consumeSched () = 
      case #mkSched p (PERSON p, time)
        of (time',dst') :: sched => 
          if (time >= time') 
            then (dst'    , sched)
            else (#visit p, (time',dst')::sched)
         | nil => (#visit p, nil)
    fun newSched() = #mkSched p (PERSON p, time)
  in
    case #dest p
      of NONE => 
        let
          val (dest,sched') = consumeSched ()
        in
          if (#area_t dest = myArea)
            then update {visit = dest    , dest = NONE     , sched = sched'}
            else update {visit = #visit p, dest = SOME dest, sched = sched'} end
       | SOME dest =>
          (* 乗れる電車があれば乗る。そして、目的地でおりる *)
          if (#place_k (#visit p) = Train) then 
            if (#area_t (#id (train $ #id (#visit p))) = #area_t dest) 
              then update {visit = dest    , dest = NONE   , sched = newSched()}
              else update {visit = #visit p, dest = #dest p, sched = newSched()}
          else (case catchTrain (train, time, myArea, #area_t dest)
            of SOME tr => update {visit = #id tr  , dest = SOME dest, sched = newSched()}
             | NONE    => update {visit = #visit p, dest = #dest p  , sched = newSched()}
          )
  end

  fun evalTrain (time_now:time) (tr:mobil) = let
    val {time = time', area_t = area_t'} = #sked tr $ (#iSked tr)
    val steps_per_day = Real.round (1.0/dt)
    val steps = time_now mod steps_per_day 
  in
    (* 発車時間がくるまでは、停車駅にずっといて、発車時間がきたら、瞬間的に次の駅へ移動 *)
    if (steps = time') then
      {id    = {place_k = Train, id = #id (#id tr), area_t = area_t', tVis = #tVis (#id tr)}
      ,nVis  = #nVis tr
      ,pTrns = #pTrns tr
      ,size  = #size tr
      ,betaN = #betaN tr
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
          , {cram  = Vector.map estTransitP (#cram  places)
            ,sch   = Vector.map estTransitP (#sch   places)
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
          of {place_k = Train, id, area_t, ...} => #nVis (train $ id)
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

  fun advanceTime (city:city): city = let
    val new_area = 
      Vector.map 
        (fn (id,persons,places) => 
          (id, map (evalPerson city) persons, places))  (#area city)
    val new_train = Vector.map (evalTrain (#time city)) (#train city)
    (* ----< bondary of time> ------------------------------------- *)
    val new_time = #time city + 1
  in
    evalPlace {area = new_area, train = new_train, time = new_time}
  end

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
  infix 1 @@
  fun isStat (h:health)(PERSON p:person) = 
    case (h,hd (#health p)) 
      of (SUS  ,SUS  ) => true
       | (EXP _,EXP _) => true
       | (INF _,INF _) => true
       | (REC _,REC _) => true
       | (VAC _,VAC _) => true
       | _             => false
  fun reducePop ((_,ps,_):area) = 
    {s = length o List.filter (isStat SUS    ) @@ ps
    ,e = length o List.filter (isStat (EXP 0)) @@ ps
    ,i = length o List.filter (isStat (INF 0)) @@ ps
    ,r = length o List.filter (isStat (REC 0)) @@ ps
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

  fun showPlace_k Cram  = "0"
    | showPlace_k Sch   = "1"
    | showPlace_k Corp  = "2"
    | showPlace_k Home  = "3"
    | showPlace_k Super = "4"
    | showPlace_k Park  = "5"
    | showPlace_k Train = "6"

  fun showPlace_t ({area_t,id,place_k,...}:place_t) = 
    fI area_t ^ "," ^ fI id ^ "," ^ showPlace_k place_k ^ ","
end
