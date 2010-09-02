(*
(1) ���ʁE�N��ʐl�����z�ɏ]���āu���̐l�ԁv�����𐶐�����
(2) (1)�̃����o�ɎЉ�I���� (Employed | Hausfrau | Student)��^����
----
(3) ���I = (�ƒ� | �w�Z | �n��Љ�)
    ���II = �u��l��ԁv = (�X�[�p�[ | �d�� | ...)
 �𐶐�����
----
(4) ����0�ɂ����āA(2)�̃����o�ɋ��I�����蓖�Ă�
----
(5) �e�Љ�I�I�����Ƌ�Ԃ��Q�Ƃ���X�N���v�g
�ɏ]���āA(4)�̊e�����o�𓮂���

 (1),(2),(4)���|�s�����[�V�����Ɋւ��邱�ƁA(3)�����A
(5)���V�~�����[�V�����ɑΉ�����B�G�[�W�F���g�̗v�f�́A

�u�l�ԁv = ���̐l�� * �Љ�I���� * ������� (�����I) * ���݋�� (�����I�����II)

---
����
  GIS = Geographi Information System �𒲂ׂ�Ƃ悢�B
    ���p�\�t�g�BAPI�̃Z�b�g���������肷��B��䶗��B

�߂��Ă���\��:
---------------
x_t �� {Home, Corp, ...}
p(x_t=Home|x_{t-1}=Corp,t) �Ȃǂ�v�f�Ƃ���m���\��^����
*)

structure Type = struct
  fun op $ (x,y) = x y; infix 1 $;
  val op \ = Vector.sub; infix 9 \;
  exception Undef;
  fun undef () = raise Undef;
  type rnd = Random.rand;
  
  (* ============================================================== *)
  (* �����������Ē萔�Ƃ������ *)
  val betaHome = 1.0
 
  (* ============================================================== *)
  (* �l�� *)
  type age  = real
  type time = int
  datatype gender  = F | M
  datatype role    = Employed | Hausfrau | Student
  datatype health
    = SUS         (* �Ɖu�Ȃ� *)
    | EXP of time (* ���� _�Ŋ������� *)
    | INF of time (* ���� _�Ŕ��ǂ��� *)
    | VAC of time (* ���� _�Ƀ��N�`����ڎ킵�� *)
    | REC of time (* ���� _�ɉ�(�Ɖu�l��)���� *)

  (* ��� *)
  type id   = int
  type size = int
  (* datatype area = HAC | TAC | JOJ | SJK | TKY *)
  (* ���Ƃ��Ĉ���������������Ȃ��̂� *)
  type area_t = int
    val HAC = 0:area_t
    val TAC = 1:area_t
    val JOJ = 2:area_t
    val SJK = 3:area_t
    val TKY = 4:area_t

  datatype place_k = Sch | Corp | Home | Train | Super | Park
(*
  type place_k = int * place_k'
  val [sch, corp, home, train, super, park]: place_k list = 
    ListPair.zip (List.tabulate(6, fn i => i), 
      [Sch, Corp, Home, Train, Super, Park])
*)

  type place_t = {place_k: place_k, area_t: area_t, id: id}

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
  (* ���ԍ\���� *)
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
    (* ���ԍ\���� *)
    type places1 =
      { sch  : place vector
      , corp : place vector
      , park : place vector
      , super: place vector
      }
 (* ToDo: �ǂ����� #id area = index of vector ��ۏ؂��Ȃ��Ƃ����Ȃ� *)
  type area = id * person list * places
  type city =
    {area : area vector  
    ,train: mobil vector
    ,time : time
    }
  (* �A�N�Z�T *)
  fun projHome x = 
    valOf (List.find (fn (p:place_t) => #place_k p = Home) x)
  fun areaPer ({belong, ...}:person) = #area_t (projHome belong)
  fun localPer (p:person) = let
    val i = areaPer p
  in
    List.filter (fn plc => i = #area_t plc) (#belong p)
  end
  fun placeAreas (areas: area vector) ({area_t,place_k,id}: place_t) =
    case place_k 
      of Home  => (#home  o #3) (areas \ area_t) \ id
       | Corp  => (#corp  o #3) (areas \ area_t) \ id
       | Sch   => (#sch   o #3) (areas \ area_t) \ id
       | Park  => (#park  o #3) (areas \ area_t) \ id
       | Super => (#super o #3) (areas \ area_t) \ id
       | Train => raise undef () (* unreachable *)
end

structure Frame = struct
  open Type; infix 1 $; infix 1 \;
  open X_Misc;
  open EasyPrint; infix 1 <<
  (* ============================================================== *)
  (* 1. [���b�N�A�b�v]���ʔN��z *)
  fun cnt () = let
    val n = ref 0
  in
    fn () => !n before n := !n + 1 
  end

  (* 3. �l��+���ѐ��� *)
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
        val rule = fmRule () (* �񌈒�I�ɉƑ��\�������߂� *)
      in
        case foldl (fn (c,(m,pop)) => m <+> findpop c pop) (nil,pop) (fmRule())
          of (nil, y::ys) => ([y], ys) (* �P�l�͎��邱�Ƃ�ۏ� *)
           | (xs , ys)    => (xs , ys)
      end
      fun loop pop = let
        val (mem,pop') = cup1 pop
        val n = length mem
        val _ = print (fI n)
        val hm = {area_t = area_t, place_k = Home, id = idHome ()}
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

  (* 3. �R�~���j�e�B�`�� *)
  fun genArea
    {area_t: area_t
    ,nSch  : size , sch  : place_t -> place
    ,nCorp : size , corp : place_t -> place
    ,nSuper: size , super: place_t -> place
    ,nPark : size , park : place_t -> place
    ,popHome: person list * place vector
    }: area =
    (area_t
    , #1 popHome
    , {sch   = Vector.tabulate (nSch  , fn id => sch  {place_k = Home, area_t = area_t, id = id})
      ,corp  = Vector.tabulate (nCorp , fn id => corp {place_k = Home, area_t = area_t, id = id})
      ,super = Vector.tabulate (nSuper, fn id => super{place_k = Home, area_t = area_t, id = id})
      ,park  = Vector.tabulate (nPark , fn id => park {place_k = Home, area_t = area_t, id = id})
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
        {id    = {place_k = Train, area_t = 0, id = idx ()}
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

  fun setAction 
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
    val from' = placeAreas areas (#visit p)
    val to'   = placeAreas areas visit
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
          if (#area_t dest = myArea) then
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
  in
    if (time = time') then
      {id    = {place_k = Train, id = #id (#id tr), area_t = area_t'}
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
(* ��̓I�ȍ\�������߂�葱�� *)
  open Type 
  val op $ = fn (x,y) => x y; infix 1 $;
  val op \ = Vector.sub; infix 9 \;
  structure F = Frame;
  open X_Misc;
  open Alice;
  val rnd = Random.rand (0,1);
  fun rndselV (v: 'a vector) = 
    v \ (Int.mod (Random.randInt rnd, Vector.length v))

  (* �l�����z�Ɏ����ȒP�ȕ��z������
   *   �N���[�N���z *)
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

  fun place size beta place_t  =
    {id = place_t 
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
      ,nSch   = 3, sch   = place 100 0.1
      ,nCorp  = 5, corp  = place  80 0.1
      ,nSuper = 1, super = place  50 0.05
      ,nPark  = 1, park  = place  50 0.01
      ,popHome = 
        F.genPop 
          {area_t = area_t
          ,nPop   = 30
          ,person = person rnd
          ,role   = role rnd
          ,fmRule = fmRule
          }
      }
  fun setAction areas = F.setAction {area = areas, belong = belong}

  fun genCity () =
    setAction (Vector.map genArea #[HAC, TAC, JOJ, SJK, TKY])
end
