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
  fun op <> (x,y) = x y; infix 1 <>;
  val op $ = Vector.sub; infix 9 $;
  exception Undef;
  fun undef () = raise Undef;
  type rnd = Random.rand;
  
  (* ============================================================== *)
  (* �����������Ē萔�Ƃ������ *)
  val betaNHome = 1.0
  val alpha = 1.0/3.5
  val gamma = 1.0/3.0
  val dt = 1.0/1440.0 (* [day] = 1 [min] *)
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
  (* ���������Ƀ��ł͂Ȃ���N���g�����R
     - �����SEIR�ƈႢ�A���W�c���Ƃ̐l��N�͎��X���X�ƕω�����̂ŁA
       �X�P�[���t���[�ȃ�N���g���K�v������B
     - �Ȃ��A��N �` O(1)�ł���B
   * ����̓������炭��Ë�
     OOP���ɉ]���΁Aplace���p������mobil���`�������Ƃ��낾���ASML��
     SML#�Ƃ������āA�u�����Ȃ��Ƃ����̃t�B�[���h�������R�[�h�^�v��
     �`���Ȃ��̂ŁA �R���|�W�V�����Ƃ���place���܂ޕ���`�ɂ��Ă���B

     �Ǝv�������A�p�����R���|�W�V�������K�v�Ȃ����Ƃ��킩�����̂ŁA
     �P�Ƀt�B�[���h�ǉ��ɂ���B
   *)
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
  (* place_t �Ŏw�肳���ꏊ��S�s�s����T�� *)
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

  (* 1. �l������ *)
  fun makePerson (n:int)(rule: id -> per2): per2 list = List.tabulate(n, rule)

  (* 2. �Ƒ��\�� *)
  fun makeHome (area_t: area_t)
               (persons:per2 list)
               (rule: unit -> ({age:age, gender:gender, role: role} -> bool) list)
               :person list * place vector = 
  let
    fun grouping (ps:per2 list): per2 list list = let
      fun op <+> (x,(SOME u,v)) = (u::x,v)
        | op <+> (x,(NONE,  v)) = (x, v)
      infix 1 <+>
      fun group1 ps = 
        case foldl (fn (cond,(mem,pop)) => mem <+> findpop cond pop) (nil,ps) (rule())
          of (nil, y::ys) => ([y], ys) (* �P�l�͎��邱�Ƃ�ۏ� *)
           | (xs , ys)    => (xs , ys)
      fun loop pps nil = pps
        | loop pps ps  =
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
        ,betaN = betaNHome}
      val ps: person list = 
        map (fn p =>
          {age    = #age p
          ,gender = #gender p
          ,role   = #role p
          ,visit  = (#id hm)
          ,belong = (#id hm) :: nil
          ,dest   = NONE
          ,health = SUS :: nil}) ps 
    in
      (ps,hm)
    end
    val unzip = (fn (per,pl) => (List.concat per,Vector.fromList pl)) o ListPair.unzip
  in
    unzip (map home (grouping persons))
  end

  (* 3. ������Ԑ��� *)
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

  (* 4. �s���͈� *)
  fun makeVisit 
    {area  : area vector
    ,belong: area vector -> person -> place_t list}
    :area vector = 
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
    Vector.map asg area
  end

  fun makeVisit' area belong = makeVisit {area=area,belong=belong}


  (* 5. �S�� *)
  (* ��A��B.A�w(��)�`B�w(��)�̏��v���ԕ\�����̉w�܂ł̏��v���Ԃ������ *)
  (* ... �Ȃ񂩂��炢��J���� ... *)
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

  (* 6. �����҂̂΂�܂� *)
  (* �y���Ӂz����͂͂����肢���ėǂ��莮�����v��������Ȃ��B�̂ŁA�Ƃ肠�����e�X�g�v�Z
   * ���͂��߂���悤�ɁA�A�h�ق����ɂ���B
   *)
  (* distInfect:
   *  area ����
   *  �d�����Ȃ��悤�Ƀ����_���Ɏw���I�яo���A�w�肳�ꂽ���N��Ԃɂ��� *)
  fun distInfect rnd (area:area) (rules: (size * health) list): area = let
    val per = #2 area
    val (modified, rest) = 
      foldl (fn ((n,stat),(modified, per)) => 
        let val (target,per) = uniqRndSample rnd per n in  
          (map 
            (fn (p:person) =>
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
  (* �V�~�����[�V�����G���W�� *)
  val rnd = Random.rand(0,1)

  fun catchTrain (train:mobil vector, now:time, src:area_t, dst:area_t): mobil option =
    Vector.find (fn tr => 
      Vector.exists (fn {time,area_t} => time =  now andalso area_t = src) (#sked tr) andalso 
      Vector.exists (fn {time,area_t} => time >= now andalso area_t = dst) (#sked tr)
    ) train

  (* ���܋���ꏊ�̊�����������m���ߒ��ɂ�茒�N��Ԃ�J�ڂ����� *)
  fun doTransit ({area=areas,train,time}:city) (p: person): health list = let
    val pTrns =
      case #visit p 
        of {place_k = Train, id, area_t} => #pTrns (train $ id)
         | place_t                       => #pTrns (placeAreas areas place_t)
    val cur::hist = #health p
  in
    case cur
      of SUS   => rndsel rnd (#s2e pTrns) (EXP time, cur) :: hist
       | EXP _ => rndsel rnd (#e2i pTrns) (INF time, cur) :: hist
       | INF _ => rndsel rnd (#i2r pTrns) (REC time, cur) :: hist
       | _     => cur :: hist
  end

  (* �l�Ԃ̐U�镑���̂��̃v���O�������u���d������v�����B�j�ł��� *)
  fun evalPerson (city:city) (p: person) = let
    val {area=areas,train,time} = city
    fun movetrns (p:person) {visit, dest} =
      { age    = #age p, gender = #gender p, role = #role p, belong = #belong p
      , visit  = visit
      , dest   = dest     
      , health = doTransit city p
      }
    fun trns p = movetrns p {visit = #visit p, dest = #dest p}
    val myArea = areaPer p
  in
    case #dest p
      of NONE => let
          (* �����_���ɍs�����I�сA�����ł���΂����ցA�łȂ���΁u�w�v�Ɍ����� *)
          val dest = rndSelL rnd (#belong p) in
          if (#area_t dest = myArea)
            then movetrns p {visit = dest, dest = NONE}
            else movetrns p {visit = #visit p, dest = SOME dest} end
       | SOME dest =>
          (* ����d�Ԃ�����Ώ��B�����āA�ړI�n�ł���� *)
          if (#place_k (#visit p) = Train) then 
            if (#area_t (#id (train $ #id (#visit p))) = #area_t dest) then
              movetrns p {visit = dest, dest = NONE}
            else
              trns p
          else (case catchTrain (train, time, myArea, #area_t dest)
            of SOME tr => movetrns p {visit = #id tr, dest = SOME dest}
             | NONE    => trns p
          )
  end

  fun evalTrain ({time,...}:city) (tr:mobil) = let
    val {time = time', area_t = area_t'} = #sked tr $ (#iSked tr)
  in
    if (time = time') then
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

  (* SEIR���f���Ɋ�Â��A�ꏊ���̊e���N��Ԃɂ���l���̐��ڂ̗\�� *)
  fun estTransitP (p:place): place = let
    val n = Real.fromInt (#size p)
    val s = Real.fromInt (!(#s(#nVis p)))
    val e = Real.fromInt (!(#e(#nVis p)))
    val i = Real.fromInt (!(#i(#nVis p)))
  in
    {id = #id p, nVis = #nVis p, size = #size p, betaN = #betaN p
    ,pTrns =
      {s2e = #betaN p / n * s * i * dt 
      ,e2i = alpha * e * dt
      ,i2r = gamma * i * dt
      }
    }
  end
  fun estTransitM (p:mobil): mobil = let
    val n = Real.fromInt (#size p)
    val s = Real.fromInt (!(#s(#nVis p)))
    val e = Real.fromInt (!(#e(#nVis p)))
    val i = Real.fromInt (!(#i(#nVis p)))
  in
    {id = #id p, nVis = #nVis p, size = #size p, betaN = #betaN p
    ,iSked = #iSked p, sked = #sked p
    ,pTrns =
      {s2e = #betaN p / n * s * i * dt 
      ,e2i = alpha * e * dt
      ,i2r = gamma * i * dt
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

  (* �l�̈ړ��̌��ʂ��A�ꏊ���̐l���ɔ��f������ *)
  fun evalPlace (city:city) = let
    val {area=areas,train,time} = city
    val vapp = Vector.app

    fun addVis (p:person) = let
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
end

structure Probe = struct
  open Type
  open EasyPrint
  structure T = TextIO
  infix 9 $
  infix 1 <>
  fun isStat (h:health)(p:person) = 
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

end

structure Trivial = struct
(* ================================================================ *)
(* ��̓I�ȍ\�������߂�葱�� *)
  open Type 
  val op <> = fn (x,y) => x y; infix 1 <>;
  val op $ = Vector.sub; infix 9 $;
  structure F = Frame;
  structure T = TextIO
  open X_Misc;
  open Alice;
  val rnd = Random.rand (0,1);
  fun rndselV (v: 'a vector) = 
    v $ (Int.mod (Random.randInt rnd, Vector.length v))

  (* �l�����z�Ɏ����ȒP�ȕ��z������:   �N���[�N���z *)
  (* ====================================================================  *)
  (* �������[�� *)

  (* �l�Ԑ������[�� *)
  fun rulePerson id = let
    val age    = Real.abs (30.0 + 30.0*rgauss rnd)
    val gender = rndsel rnd 0.5 (F,M)
    val role   = 
      if (age <= 22.0)     then Student
      else if (age < 60.0) then
        case gender
          of F => rndsel rnd 0.6 (Employed, Hausfrau)
           | M => rndsel rnd 0.8 (Employed, Hausfrau)
      else
        Hausfrau
  in
    {age = age, gender = gender, role = role}
  end

  (* �Ƒ��\�����[�� *)
  fun ruleHome () = 
    List.tabulate (1 + iR (abs (2.0 * rgauss rnd)), fn _ => fn _ => true)

  (* ������Ԑ������[�� *)
  fun rulePlace' size betaN place_t =
    {id    = place_t 
    ,nVis  = zeroNVis ()
    ,pTrns = zeroPTrns ()
    ,size  = iR o abs <> rI size + rI size*rgauss rnd
    ,betaN = abs (betaN + betaN*rgauss rnd)
    }: place

  fun rulePlace place_k size area_t id = 
    rulePlace' size 0.1 {place_k = place_k, area_t = area_t, id = id}

  (* �s���͈̓��[�� *) 
  fun ruleVisit (areas: area vector)(p: person): place_t list = let
    val i0 = areaPer p
    val (_,_,a0) = areas $ i0
    val (_,_,a') = rndselV areas
  in
    case #role p 
      of Employed => map #id [rndselV (#corp a') , rndselV (#super a0)]
       | Student  => map #id [rndselV (#sch a0)  , rndselV (#super a0)]
       | HausFrau => map #id [rndselV (#super a0)]
  end

  (* �S���^�s���[�� *)
  val time2next = [(TKY,5),(SJK,10),(JOJ,15),(TAC,20),(HAC,0)]
  val services =
    (Vector.fromList o List.concat) 
    (map (fn h => 
       [ {deptime = 60*h, stations = #[TKY, SJK, JOJ, TAC, HAC]} 
       , {deptime = 60*h, stations = #[HAC, TAC, JOJ, SJK, TKY]} 
       ]
     ) [6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23])

  (* ====================================================================  *)
  (* ��L���[���ɂ��s�s�̑g�ݗ��� *)
  fun per ()     = F.makePerson 3000 rulePerson
  fun perHome at = F.makeHome at (per ()) ruleHome
  fun place   at home = 
    F.makePlace at home
      [(10,rulePlace Sch 100),(10,rulePlace Super 20),(19,rulePlace Corp 15)]
  fun infect (area:area) = 
    if (#1 area = JOJ) 
      then F.distInfect rnd area [(15,EXP 0)]
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

  fun run1 recstep tStop file city = let
    val os = T.openOut file
    val () = Probe.showPopTag os 5
    val recstep = recstep * iR (1.0/dt)
    val tStop   = tStop   * iR (1.0/dt)
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
end

(* �����ɂ��� 
* �����̃f�o�b�O�ɂ́A�l���ǂ������������d�v�Ȃ̂ŁA�P���� (����,�s�s) ��
  �v���b�g������̂��悢���낤�B�͂��߂͂ʂ����������ŁB�������A�u�d�ԁv
  �ɂ���Ƃ��́A�d�Ԃ̌��݂ł͂Ȃ��A�u�d�ԁv�Ƃ������z�s�s�ɂ��邱�Ƃɂ���
  �̂��ǂ����낤�B
*)
