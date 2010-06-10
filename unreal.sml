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
  fun op $ (x,y) = x y; infix 1 $;
  val op \ = Vector.sub; infix 9 \;
  exception Undef;
  fun undef () = raise Undef;
  type rnd = Random.rand;
  
  (* ============================================================== *)
  (* 差しあたって定数とするもの *)
  val betaHome = 1.0
 
  (* ============================================================== *)
  (* 人間 *)
  type age  = real
  type time = int
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
  datatype place_t
    = Sch   of id * area_t
    | Corp  of id * area_t 
    | Home  of id * area_t
    | Train of id * area_t
    | Super of id * area_t
    | Park  of id * area_t
  type place = {id: place_t, nVis: size ref, size: size, beta: real}
  type mobil = 
    {id: place_t, nVis: size ref, size: size, beta: real
    ,iSked: int, sked: (time * area_t) vector}
  type person = 
    { age   : age
    , gender: gender
    , role  : role
    , belong: place_t list
    , visit : place_t
    , dest  : place_t option
    , health: health list
    }
  (* 中間構造体 *)
    type per1 = {age: age, gender: gender}
    type per2 = {age: age, gender: gender, role: role}
    type per3 = {age: age, gender: gender, role: role, belong: place_t list}

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
    valOf (List.find (fn Home _ => true | _ => false) x)
  fun areaPlace (Home (_,x)) = x
    | areaPlace (Corp (_,x)) = x
    | areaPlace (Sch  (_,x)) = x
    | areaPlace (Park (_,x)) = x
    | areaPlace (Super(_,x)) = x
    | areaPlace (Train(_,x)) = x
  fun areaPer ({belong, ...}:person) = areaPlace (projHome belong)
  fun localPer (p:person) = let
    val i = areaPer p
  in
    List.filter (fn plc => i = areaPlace plc) (#belong p)
  end
  fun placeCity (areas: area vector) (p: place_t) = 
    case p
      of Home (i,x) => (#home  o #3) (areas \ x) \ i
       | Corp (i,x) => (#corp  o #3) (areas \ x) \ i
       | Sch  (i,x) => (#sch   o #3) (areas \ x) \ i
       | Park (i,x) => (#park  o #3) (areas \ x) \ i
       | Super(i,x) => (#super o #3) (areas \ x) \ i
       | Train(i,x) => raise undef () (* unreachable *)
end

structure Frame = struct
  open Type; infix 1 $; infix 1 \;
  open X_Misc;
  open EasyPrint; infix 1 <<
  (* ============================================================== *)
  (* 1. [モックアップ]性別年齢分布 *)
  fun cnt () = let
    val n = ref 0
  in
    fn () => !n before n := !n + 1 
  end

  (* 3. 人口+世帯生成 *)
  fun genPop
    {area_t: area_t
    ,nPop  : size
    ,person: id -> {age:age, gender:gender}
    ,role  : {age:age, gender:gender} -> role
    ,fmRule: unit -> ({age:age, gender:gender, role: role} -> bool) list
    }: person list * place vector =
  let
    fun ext1 (p: per1,r): per2 = {age = #age p, gender = #gender p, role = r}
    fun ext2 (p: per2,b): person = 
      {age    = #age p
      ,gender = #gender p
      ,role   = #role p
      ,visit  = b
      ,belong = b :: nil
      ,dest   = NONE
      ,health = SUS :: nil
      }

    val pop = List.tabulate (nPop, (fn p => ext1 (p,role p)) o person)

    fun cup pop = let
      val idHome = cnt ()
      fun cup1 pop = let
        fun op <+> (x,(u,v)) = 
          case u 
            of SOME u => (u::x, v)
             | NONE   => (x,v)
        infix 1 <+>
        val rule = fmRule () (* 非決定的に家族構成を決める *)
      in
        case foldl (fn (c,(m,pop)) => m <+> findpop c pop) (nil,pop) (fmRule())
          of (nil, y::ys) => ([y], ys) (* １人は取れることを保証 *)
           | (xs , ys)    => (xs , ys)
      end
      fun loop pop = let
        val (mem,pop') = cup1 pop
        val n = length mem
        val _ = print (fI n)
        val hm = Home (idHome (), area_t)
        val hmObj = {id = hm, nVis = ref n, size = n, beta = betaHome}
        val mem = map (fn m => ext2 (m,hm)) mem 
      in
        if (not (null pop')) then
          (mem, hmObj) :: loop pop'
        else
          nil
      end
    in
      (fn (x,y) => (List.concat x, Vector.fromList y)) o ListPair.unzip $ (loop pop)
    end
  in
    cup pop
  end

  (* 3. コミュニティ形成 *)
  fun genArea
    {area_t: area_t
    ,nSch  : size , sch  : area_t -> id -> place
    ,nCorp : size , corp : area_t -> id -> place
    ,nSuper: size , super: area_t -> id -> place
    ,nPark : size , park : area_t -> id -> place
    ,popHome: person list * place vector
    }: area =
    (area_t
    , #1 popHome
    , {sch   = Vector.tabulate (nSch  , sch area_t)
      ,corp  = Vector.tabulate (nCorp , corp area_t)
      ,super = Vector.tabulate (nSuper, super area_t)
      ,park  = Vector.tabulate (nPark , park area_t)
      ,home  = #2 popHome
      }
    )

  fun makeTrains 
    {reqtime: time vector
    ,services: {deptime:time, from: area_t, to: area_t} vector
    ,beta: real
    ,size: size
    }: mobil vector = 
  let
    val idx = cnt ()
    infix 9 \
  in
    Vector.map (fn {deptime, from, to} => 
        {id    = Train (idx (), 0)
        ,nVis  = ref 0
        ,size  = size
        ,beta  = beta
        ,iSked = 0
        ,sked  = 
          if (to > from) then 
            (fn s => Vector.tabulate(to - from, 
              (fn i => (deptime + !s, from + i) before s := !s + reqtime \ (from + i)))
            )(ref 0)
          else
            (fn s => Vector.tabulate(from - to, 
              (fn i => (deptime + !s, from - i) before s := !s + reqtime \ (from - i - 1)))
            )(ref 0)
        }
    ) services
  end

  fun conArea 
    {area  : area vector
    ,belong: area vector -> person -> place_t list
    }: city =
  let
    fun ext (x:person) =
      {age = #age x
      ,gender = #gender x
      ,role   = #role x
      ,belong = belong area x @ #belong x
      ,visit  = #visit x
      ,dest   = #dest x
      ,health = #health x
      }
    fun asg (id, pop, places) =
      (id, map ext pop, places)
  in
    {area  = Vector.map asg area
    ,train = #[]
    ,time = 0
    }
  end

  val rnd = Random.rand(0,1)

  fun updatePerson (areas:area vector)(p:person){visit, dest, health} = let
    val from' = placeCity areas (#visit p)
    val to'   = placeCity areas visit
  in
    ( #nVis from' := !(#nVis from') - 1
    ; #nVis to'   := !(#nVis to')   + 1
    ;{ age    = #age p
     , gender = #gender p
     , role   = #role p
     , belong = #belong p
     , visit  = visit
     , dest   = dest
     , health = health
     }
    )
  end

  fun evalPerson ({area=areas,train,time}:city) (p: person) = let
    val myArea = areaPer p
  in
    case #dest p
      of NONE => 
        let
          val dest = rndSelL rnd (#belong p)
        in
          if (areaPlace dest = myArea) then
            updatePerson areas p {visit = dest, dest = NONE, health = #health p}
          else            
            updatePerson areas p {visit = #visit p, dest = SOME dest, health = #health p}
        end
      | SOME dest => 
        ( print "bording train is not implemented!"
        ; updatePerson areas p {visit = dest, dest = NONE, health = #health p}
        )
  end

  fun evalTrain ({time,...}:city) (tr:mobil) = let
    val (time',area_t') = #sked tr \ (#iSked tr)
    val Train (id, _) = #id tr
  in
    if (time = time') then
      {id    = Train (id,area_t')
      ,nVis  = #nVis tr
      ,size  = #size tr
      ,beta  = #beta tr
      ,iSked = (#iSked tr + 1) mod (Vector.length (#sked tr))
      ,sked  = #sked tr
      }
    else
      tr
  end

  fun evalArea city ((id,pop,places): area): area = 
    (id
    ,map (evalPerson city) pop
    ,places
    )

  fun eval1 (city:city) = 
    {area  = Vector.map (evalArea city) (#area city)
    ,train = Vector.map (evalTrain city) (#train city)
    ,time  = #time city + 1
    }
end

structure Trivial = struct
(* ================================================================ *)
(* 具体的な構成を決める手続き *)
  open Type 
  val op $ = fn (x,y) => x y; infix 1 $;
  val op \ = Vector.sub; infix 9 \;
  structure F = Frame;
  open X_Misc;
  open Alice;
  val rnd = Random.rand (0,1);
  fun rndselV (v: 'a vector) = 
    v \ (Int.mod (Random.randInt rnd, Vector.length v))

  (* 人口分布に似た簡単な分布がある
   *   クラーク分布 *)
  fun person (rnd:Random.rand) (id:id) =
    {age    = Real.abs (30.0 + 30.0*rgauss rnd)
    ,gender = if Random.randInt rnd > 0 then M else F
    }

  fun role (rnd:rnd) {age, gender} = 
    if (age <= 22.0) then Student
    else if (age < 60.0) then
      case gender
        of F => rndsel rnd 0.6 (Employed, Hausfrau)
         | M => rndsel rnd 0.8 (Employed, Hausfrau)
    else
      Hausfrau

  fun place typ size beta a id  =
    {id = typ (id, a)
    ,nVis = ref 0
    ,size = iR o abs $ rI size + rI size*rgauss rnd
    ,beta = abs (beta + beta*rgauss rnd)
    }

  fun fmRule () = 
    List.tabulate (1 + iR (abs (2.0 * rgauss rnd)), fn _ => fn _ => true)

  fun belong (areas: area vector)(p: person): place_t list = let
    val i0 = areaPer p
    val (_,_,a0) = areas \ i0
    val (_,_,a') = rndselV areas
  in
    case #role p 
      of Employed => map #id [rndselV (#corp a') , rndselV (#super a0)]
       | Student  => map #id [rndselV (#sch a0)  , rndselV (#super a0)]
       | HausFrau => map #id [rndselV (#super a0)]
  end

  fun genArea area_t = 
    F.genArea 
      {area_t = area_t
      ,nSch   = 3, sch   = place Sch  100 0.1
      ,nCorp  = 5, corp  = place Corp  80 0.1
      ,nSuper = 1, super = place Super 50 0.05
      ,nPark  = 1, park  = place Park  50 0.01
      ,popHome = 
        F.genPop 
          {area_t = area_t
          ,nPop   = 30
          ,person = person rnd
          ,role   = role rnd
          ,fmRule = fmRule
          }
      }
  fun conArea areas = F.conArea {area = areas, belong = belong}

  fun genCity () =
    conArea (Vector.map genArea #[HAC, TAC, JOJ, SJK, TKY])
end
