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
  fun op @@ (x,y) = x y; infix 1 @@;
  val op $ = Vector.sub; infix 9 $;
  exception Undef;
  fun undef () = raise Undef;
  
  (* ============================================================== *)
  (* �����������Ē萔�Ƃ������ *)
  val alpha = 1.0/3.5
  val gamma = 1.0/3.0

  (* ���v�֌W *)
  type time = int (* in steps *)
  val dt = 1.0/1440.0 (* [day] = 1 [min] *)
  val steps_per_day = Real.round (1.0/dt)

  (* ���Ԃ̒P�� (in steps) *)
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
  (* �l�� *)
  type age  = real
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

  datatype place_k = Cram | Sch | Corp | Home | Super | Park | Train

  (* �ꏊ����肷��^�v�� *)
  (* �y������̒��Ӂz
     �Q�ƌ^��tVis��person�̃t�B�[���h�Ɍ����Ƃ��݈̂Ӗ������B
     ����́A�u����ЂƂ����̏ꏊ�Ɉړ����������v���L�q���邽�߂̃g���b�N�B
     in-place replace���������邽�߂ɎQ�ƌ^�ɂȂ��Ă���B
  *)
  type place_t = {place_k: place_k, area_t: area_t, id: id, tVis:time ref}
  (* �Ȃ̂ŁAperson���R�[�h����������Ƃ��ɃN���[�j���O����K�v������ *)
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
    (* ���ԍ\���� *)
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
  fun areaPer (PERSON {belong, ...}:person) = #area_t (projHome belong)
    (*
  fun localPer (PERSON p:person) = let
    val i = areaPer (PERSON p)
  in
    List.filter (fn plc => i = #area_t plc) (#belong p)
  end
  *)
  (* place_t �Ŏw�肳���ꏊ��S�s�s����T�� *)
  fun placeAreas (areas: area vector) ({area_t,place_k,id,...}: place_t) =
    case place_k 
      of Home  => (#home  o #3) (areas $ area_t) $ id
       | Corp  => (#corp  o #3) (areas $ area_t) $ id
       | Sch   => (#sch   o #3) (areas $ area_t) $ id
       | Cram  => (#cram  o #3) (areas $ area_t) $ id
       | Park  => (#park  o #3) (areas $ area_t) $ id
       | Super => (#super o #3) (areas $ area_t) $ id
       | Train => raise undef () (* unreachable *)

  (* �����̃V�[�h�̐ݒ� *)
  (* �����Őݒ肷��͉̂������A���X�ƈ����ň����񂷂̂�����Ȃ̂ŁA�����ł�� *)
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

  (* 1. �l������ *)
  fun makePerson (n:int)(rule: id -> per2): per2 list = List.tabulate(n, rule)

  (* 2. �Ƒ��\�� *)
  fun makeHome (area_t: area_t)
               (betaN : unit -> real) (* �����_�������������� *)
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
          of (nil, y::ys) => ([y], ys) (* �P�l�͎��邱�Ƃ�ۏ� *)
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

  (* 3. ������Ԑ��� *)
  (* area_t: �ꏊ��ݒu����X
   * home  : ��ɍ�����ƒ�̏W�܂�B
   * rules : �����K���̏W�܂�B�e�����K�� (n,gen) �ɑ΂��āA
   *   n�̏ꏊ�� gen���琶������B
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

  (* 4. �s���͈� *)
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
  (* �V�~�����[�V�����G���W�� *)
  (*
  *)

  fun catchTrain (train:mobil vector, now:time, src:area_t, dst:area_t): mobil option = let
    (* steps���P�ʂł�(����)����,
     * ����/�y���ňقȂ�_�C�A���g���Ƃ��͂�����ς���K�v������ *)
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
  (* ���܋���ꏊ�̊�����������m���ߒ��ɂ�茒�N��Ԃ�J�ڂ����� *)
  fun updateHealth ({area=areas,train,time}:city) (PERSON p: person): health list = let
    val rnd = getrnd()
    val pTrns =
      case #visit p 
        of {place_k = Train, id, ...} => #pTrns (train $ id)
         | place_t                    => #pTrns (placeAreas areas place_t)
    val cur::hist = #health p
  in
    (* ��: �����̂Ƃ���ɒ��ӁB���X�g�A���́A�������������A�Ȍ��ɏ����ɂ͂����B
     * rndSel rnd (#s2e pTrns) (EXP time, cur) :: cur :: hist �ł͂Ȃ�!!
     * �Ƃ����Ԉ�������Ƃ�����ƁA10�{�x���Ȃ�B*)
    case cur
      of SUS   => rndSel rnd (#s2e pTrns) ([EXP time, cur],[cur]) @ hist
       | EXP _ => rndSel rnd (#e2i pTrns) ([INF time, cur],[cur]) @ hist
       | INF _ => rndSel rnd (#i2r pTrns) ([REC time, cur],[cur]) @ hist
       | _     => cur :: hist
  end

  (* �l�Ԃ̐U�镑���̂��̃v���O�������u���d������v�����B�j�ł��� *)

  (* ���̑O�ɁA[(pr,x) ...] ����m��pr��x����郋�[�`�������� *)

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
          (* ����d�Ԃ�����Ώ��B�����āA�ړI�n�ł���� *)
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
    (* ���Ԏ��Ԃ�����܂ł́A��ԉw�ɂ����Ƃ��āA���Ԏ��Ԃ�������A�u�ԓI�Ɏ��̉w�ֈړ� *)
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

  (* SEIR���f���Ɋ�Â��A�ꏊ���̊e���N��Ԃɂ���l���̐��ڂ̗\�� *)
  fun estTransitP (p:place): place = let
    (* val n = Real.fromInt (#size p)   (* ����̓o�O���ăn�j�[!! *) *)
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
      {s2e = beta * i * dt  (* �Ӗ����킩��₷�����́A betaN * (i/n) * dt *)
      ,e2i = alpha * dt (* ���ǂ͎����I������ *e �͂������� *)
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

  (* �l�̈ړ��̌��ʂ��A�ꏊ���̐l���ɔ��f������ *)
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
