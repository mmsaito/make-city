(*
 * �p���f�~�b�N�E�G�[�W�F���g�E�V�~�����[�^ "Unreal"
 * -------------------------------------------------
 * �ȉ��̃R�����g�͂܂�����obsoleted�Ȃ̂ŁA��ŏ����������ƁB
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
  datatype role    = Employed | Hausfrau | Student | Patient | Doctor
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
  (* �Ƃ��Ă��������A�z��̓Y�����ɂ������̂� *)
  type area_t = int
    val HAC = 0:area_t
    val TAC = 1:area_t
    val JOJ = 2:area_t
    val SJK = 3:area_t
    val TKY = 4:area_t

  datatype place_k = Cram | Sch | Corp | Home | Super | Park | Train | Hosp

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
       �X�P�[���t���[�ȃ�N �` O(1) ���g���B
   * �݌v��̃~�X: mobil��place��composition���ׂ��������B�����邽�߂ɁA
   * �������ŁAcopy & paste���������Ă��܂����B
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
    , hosp : place vector
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
    (* �^�v���ł͂Ȃ��A���R�[�h�ɂ��ׂ������� *)
    val idArea    = #1: area -> id
    val popArea   = #2: area -> person list
    val placeArea = #3: area -> places
  type city =
    {area : area vector  
    ,train: mobil vector
    ,time : time
    }
  (* �A�N�Z�T *)
  fun projHome x = 
    valOf (List.find (fn (p:place_t) => #place_k p = Home) x)
  fun areaPer (PERSON {belong, ...}:person) = #area_t (projHome belong)
  
  (* place_t �Ŏw�肳���ꏊ��S�s�s����T�� *)
  fun placeAreas (areas: area vector) ({area_t,place_k,id,...}: place_t) =
    case place_k 
      of Home  => (#home  o #3) (areas $ area_t) $ id
       | Corp  => (#corp  o #3) (areas $ area_t) $ id
       | Sch   => (#sch   o #3) (areas $ area_t) $ id
       | Cram  => (#cram  o #3) (areas $ area_t) $ id
       | Park  => (#park  o #3) (areas $ area_t) $ id
       | Super => (#super o #3) (areas $ area_t) $ id
       | Hosp  => (#hosp  o #3) (areas $ area_t) $ id
       | Train => raise undef () (* unreachable *)

  (* �����̃V�[�h�̐ݒ� *)
  (* �����Őݒ肷��͉̂������A���X�ƈ����ň����񂷂̂�����Ȃ̂ŁA�����ł�� *)
  local
    val rndref = ref NONE: Random.rand option ref
  in
    fun getrnd () =
      case (!rndref)
        of SOME r => r
         | NONE   => (print "unreal.sml:190: random number is not initialized!  Using default!\n"
                     ;rndref := SOME (Random.rand (0,1))
                     ;valOf(!rndref)
                     )
    fun inirnd n = rndref := SOME (Random.rand(0,n))
  end

  (* ���N�`�������̐ݒ� *)
  local
    val vacEff__ = ref NONE: real option ref
  in
    fun getVacEff() = valOf (!vacEff__)
    fun setVacEff x = vacEff__ := SOME x
  end

  (* ����ΏۃN�G���[ *)
  datatype roleOpt   = ROL_ARBIT | ROL_SOME of role
  datatype liveinOpt = LIV_ARBIT | LIV_SOME of area_t  (* for livein *)
  datatype workatOpt = WOR_ARBIT | WOR_LOCAL | WOR_SOME of (area_t * place_k) list
  type belongSpec = {role: roleOpt, livein: liveinOpt, workat: workatOpt}
  datatype intervOpt = OPT_INTERV_INF | OPT_INTERV_VAC

  (* ����v�惊�X�g *)
  datatype interv 
    = INTERV_INF of {time:int, area_t:int, person: int}
    | INTERV_VAC of {time:int, area_t:int, person: int
                    ,response: real
                    ,hyposensitize: real}
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
    ,hosp  = #[]
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

  (* 6. ���� *)  
  fun matchBelongSpec ({role, livein, workat}: belongSpec) (PERSON p) = let
    val isHome = fn ({place_k = Home, ...}:place_t) => true | _ => false
    val hometown = #area_t (valOf (List.find isHome (#belong p)))
      handle _ => (print "warning: homeless agent. forcing their hometown to be 0-th one.\n"
                  ;0)
    fun eqRole (ROL_ARBIT, _) = true
      | eqRole (ROL_SOME x, y) = x = y
    fun eqLive (LIV_ARBIT, _) = true
      | eqLive (LIV_SOME x, y) = x = y
    fun eqWork (WOR_LOCAL , ys) = List.all (fn ({area_t,...}:place_t) => area_t = hometown) ys
      | eqWork (WOR_ARBIT , _ ) = true
      | eqWork (WOR_SOME xs,ys) = (* ToDo: ��ʂ̏W�����Z�������Ȃ��Ƃ����Ȃ� *)
      List.all (fn (area_t':area_t,place_k':place_k) => 
        List.exists (fn ({area_t,place_k,...}: place_t) =>
          area_t = area_t' andalso place_k = place_k') ys) xs
  in
    eqRole (role, #role p) andalso 
    eqLive (livein, hometown) andalso 
    eqWork (workat, #belong p)
  end

  (* ���� xx ���������l�Ԃ̏����𓾂� *)
  fun whereComeFromTo (city:city) (target:place_t)  = let
    val comefrom = Array.array(Vector.length (#area city), 0)
    fun eqPlace (x:place_t) (y: place_t) = 
      #place_k x = #place_k y andalso
      #area_t x = #area_t y andalso
      #id x = #id y
    fun countUp (PERSON p) = 
      if List.exists (eqPlace target) (#belong p) then
        let
          val hometown = 
            (#area_t o valOf o List.find (fn {place_k,...} => place_k = Home)) (#belong p)
        in
          Array.update(comefrom, hometown, Array.sub(comefrom,hometown) + 1)
        end
      else
        ()
  in
    (Vector.app (fn area => app countUp (#2 area)) (#area city)
    ;Array.vector comefrom)
  end

  (* 7. �����҂̂΂�܂� *)
  (* (1) distInfect:
   *  area ����
   *  �d�����Ȃ��悤�Ƀ����_���Ɏw���I�яo���A�w�肳�ꂽ���N��Ԃɂ��� *)
  fun distInfect rnd (rules: (size * health) list) (area:area): area = let
    val per = #2 area
    val (modified, rest) = 
      foldl (fn ((n,stat),(modified, per)) => 
        let val (target,per) = uniqRndSample rnd per n 
          handle e => 
           (print ("distInfect: #per = " ^ sI (length per)
             ^ "," ^ "n = " ^ sI n ^"\n")
           ;raise e)
        in  
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
              , sched   = #sched p
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

  (* �����A����͌�������̑��l���𐶂݁A�V�~�����[�V�������ʂ̉��߂�����ɂȂ���
   * �̂ŁA�ȉ��̌���_�I�Ȃ��̂ɐ؂�ւ��邱�Ƃɂ����B*)
 
  (* (2) ruleInfect : 
   *   area���ŁA�����𖞂����l�� * ����_�I�� * ���������� *)
  fun ruleInfect (n:int) (spec:belongSpec) (area:area) = let 
    val pop = #2 area 
    fun setExposed (PERSON p:person) =
      PERSON { age    = #age p
             , gender = #gender p
             , role   = #role p
             , belong = #belong p
             , visit  = #visit p
             , dest   = #dest p
             , health = [EXP 0]
             , mkSched= #mkSched p
             , sched  = #sched p }
    val n' = ref n
    fun setExposedIf p = 
      if (!n' > 0 andalso matchBelongSpec spec p) 
        then setExposed p before n' := !n' - 1
        else p
  in
    (#1 area, map setExposedIf (#2 area),#3 area): area
  end


  (* ������󂯂�l�̃��X�g������ *)
  fun ruleInterv 
    {vacResponse: age -> real
    ,vacHyposensitize: age -> real}
    {tag:string, n:int, rule:belongSpec, isRandom:bool, time:int, kind:intervOpt} (area:area) = let

    fun add (id, PERSON p:person) =
      case kind 
        of OPT_INTERV_INF => INTERV_INF {time=time, area_t = idArea area, person = id}
         | OPT_INTERV_VAC => 
             INTERV_VAC {time          = time
                        ,area_t        = idArea area
                        ,person        = id
                        ,response      = vacResponse (#age p)
                        ,hyposensitize = vacHyposensitize (#age p)
                        }
    val n' = ref n
    fun matchAdd (id, p, xs:interv list) = 
      if (!n' > 0 andalso matchBelongSpec rule p) 
        then add (id,p) :: xs  before n' := !n' - 1
        else xs
  in
    Misc.foldliL matchAdd nil (popArea area)
  end

  (* 8. ���N�`���ڎ� *)
  fun vacWho (rule:person -> bool)(area:area): area = let
    val per = #2 area
    fun set (PERSON p:person) =
      PERSON { age    = #age p
             , gender = #gender p
             , role   = #role p
             , belong = #belong p
             , visit  = #visit p
             , dest   = #dest p
             , health = [VAC 0]
             , mkSched= #mkSched p
             , sched  = #sched p }
    fun set_if p = if rule p then set p else p
  in
    (#1 area, map set_if (#2 area), #3 area)
  end

  (* �d�Ԃɏ��l�ɐڎ� *)
  fun ruleVacTrain {cover:real, eff:real} (PERSON p) = let
    val rnd    = getrnd()
    val pr = cover * eff
    val isHome = fn ({place_k = Home, ...}:place_t) => true | _ => false
    val hometown = #area_t (valOf (List.find isHome (#belong p)))
  in
      List.exists (fn ({area_t,...}:place_t) => area_t <> hometown) (#belong p)
      andalso rndSel rnd pr (true,false)
  end

  (* �w���ɐڎ� *)
  fun ruleVacSchool {cover:real, eff:real} (PERSON p) = 
    #role p = Student andalso rndSel (getrnd()) (cover*eff) (true,false)

  (* ============================================================== *)
  (* �V�~�����[�V�����G���W�� *)
  fun catchTrain (train:mobil vector, now:time, src:area_t, dst:area_t): mobil option = let
    val {step,...} = timecomp now 
  in
    Vector.find (fn tr => 
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
  fun updateHealth
    (osSeq:TextIO.outstream option) 
    ({area=areas,train,time}:city) (PERSON p: person): health list = let
    val rnd    = getrnd()
    val  pTrns =
      case #visit p 
        of {place_k = Train, id, ...} => #pTrns (train $ id)
         | place_t                    => #pTrns (placeAreas areas place_t)
    val cur::hist = #health p
    fun printRole () =
      case osSeq 
        of NONE => ()
         | SOME os =>
             (case (#role p)
               of Employed => TextIO.output(os,"E")
                | Hausfrau => TextIO.output(os,"H")
                | Student  => TextIO.output(os,"S")
                | Patient  => TextIO.output(os,"P")
                | Doctor   => TextIO.output(os,"D")
             )
    fun redu_s2e() = 
      if List.exists (fn VAC _ => true | _ => false) (#health p) then
        0.0
      else
        #s2e pTrns 
    val newStat =  
      case cur
        of SUS   => rndSel rnd (redu_s2e()) ([EXP time, cur],[cur]) @ hist
         | EXP _ => rndSel rnd (#e2i pTrns) ([INF time, cur],[cur]) @ hist
         | INF _ => rndSel rnd (#i2r pTrns) ([REC time, cur],[cur]) @ hist
         | _     => cur :: hist
  in
    (if EXP time = hd newStat then printRole() else ()
    ;newStat)
  end

  (* �l�Ԃ̐U�镑���̂��̃v���O�������u���d������v�����B�j�ł��� *)
  fun evalPerson (osSeq:TextIO.outstream option) (city:city) (PERSON p: person) = let
    val {area=areas,train,time} = city
    fun update {visit, dest, sched} = PERSON
      { age    = #age p, gender = #gender p, role = #role p, belong = #belong p
      , mkSched= #mkSched p
      , sched  = sched
      , visit  = visit
      , dest   = dest     
      , health = updateHealth osSeq city (PERSON p)
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
            ,hosp  = Vector.map estTransitP (#hosp  places)
            }
          )
       ) area
     ,train = Vector.map estTransitM train
     ,time = time
     }

  (* �l�̈ړ��̌��ʂ��A�ꏊ���̐l���ɔ��f������ *)
  fun evalPlace (city:city) = let
    fun addVis (PERSON p:person) = let
      val nVis =
        case #visit p 
          of {place_k = Train, id, area_t, ...} => #nVis (#train city $ id)
           | others => #nVis (placeAreas (#area city) others)
    in
      case hd (#health p)
        of SUS   => #s nVis := !(#s nVis) + 1
         | VAC _ => #v nVis := !(#v nVis) + 1
         | EXP _ => #e nVis := !(#e nVis) + 1
         | INF _ => #i nVis := !(#i nVis) + 1
         | REC _ => #r nVis := !(#r nVis) + 1
    end
       
    fun clearPersons ((id, persons, places):area) = 
      app (fn f => Vector.app (fn x => clearNVis (#nVis x)) (f places)) 
        [#sch, #corp, #park, #super, #home]

    fun countPersons ((id, persons, places):area) = 
      app addVis persons
  in
    (Vector.app clearPersons (#area city)
    ;Vector.app countPersons (#area city)
    ;estTransit city)
  end

  (* �V�~�����[�V�����̃g�b�v���x���֐� *)
  fun advanceTime (os2:TextIO.outstream option) (city:city): city = let
    val new_area = 
      Vector.map 
        (fn (id,persons,places) => 
          (id, map (evalPerson os2 city) persons, places))  (#area city)
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

  fun staticPlacePop kind (city:city) f = let
    val selKind =
      case kind
        of Cram  => #cram  | Sch  => #sch  | Corp => #corp | Home => #home 
         | Super => #super | Park => #park | Train => (fn _ => #[])
         | Hosp  => #hosp
    val boxes = 
      Vector.map (fn area =>
          Array.array(Vector.length (selKind (placeArea area)), 0)
        ) (#area city)
    val <- = fn ((a,i),d) => Array.update(a,i,d); infix 1 <-
    val `$ = fn (a,i)     => (a,i); infix 9 `$
    val $` = fn (a,i)     => Array.sub(a,i); infix 9 $`
    (* �]�ƈ����𐔂�������ad hoc�Ȃ��Ƃ����� *)
    fun findL c xs = (fn SOME x => [x] | _ => []) (List.find c xs)
    fun return_after_save () = let
      val sizes = Vector.map Array.vector boxes
      val sortSizes = Misc.qsortV (fn (a,b) => Int.compare(b,a)) (sizes $ 4)
      val os = TextIO.openOut f
    in
      (Vector.appi (fn (i,n) => TextIO.output(os, sI (i+1)^","^sI n^"\n")) sortSizes
      ;TextIO.closeOut os
      ;sizes
      )
    end
  in
   (Vector.app (fn area => 
      List.app (fn PERSON person => 
        List.app (fn {area_t, id, ...} =>
          boxes $ area_t `$ id <- boxes $ area_t $` id + 1
        ) (findL (fn {place_k,...} => place_k = kind) (#belong person))
      ) (popArea area)
    ) (#area city)
   ; return_after_save()
   )
  end

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
    | showPlace_k Hosp  = "7"

  fun showPlace_t ({area_t,id,place_k,...}:place_t) = 
    fI area_t ^ "," ^ fI id ^ "," ^ showPlace_k place_k ^ ","

  (* �󎚋@ *)
  fun showRole Employed = "Employed" 
    | showRole Hausfrau = "Hausfrau" 
    | showRole Student  = "Student" 
    | showRole Patient  = "Patient"
    | showRole Doctor   = "Doctor"

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
    | showPlace_k' Hosp  = "Hosp"

  fun showPlace_t' ({area_t, place_k, id, ...}: place_t) =
    String.concat [sI area_t,",", showPlace_k' place_k,",", sI id]

  (* �V�~�����[�V������Ԃ������o���BFortran�œǂ݂₷���`���ɂ��Ă��� 
   *   - ���������������o�����Ƃ����z�肵�Ă��Ȃ��̂ŁA���S�łȂ�����
   *     ����Ȃ��B*)
  fun writeCity (city:city) (f:string) = let
    val os = TextIO.openOut f
    fun w s = TextIO.output(os, s)
    fun w' s = (w s; w "\n")
    fun w_ s = (w s; w ",")

    val w_place_t = w' o showPlace_t' 

    fun w_person (PERSON {age, role, belong, visit, dest, health, ...}) = 
      (w_ (showRole role); w' (sR age)
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

    fun w_place_group({cram,sch,corp,park,super,home,hosp}:places) =
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

      ;w' (showPlace_k' Hosp); w (sI o Vector.length @@ hosp); w' ",length" 
      ;Vector.app w_place hosp
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
     ;w' "place-group:"; w (sI 7); w' ",length" (* type places *)
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

  fun writeIntervPlan (xs: interv list) f = let
    val os = T.openOut f
    fun wrt (INTERV_VAC {time:int, area_t: int, person: int, response: real, hyposensitize: real}) =
      T.output(os, "VAC"^","^sI time^","^sI area_t^","^sI person
                 ^","^sR response^","^sR hyposensitize^"\n")
      | wrt (INTERV_INF {time:int, area_t: int, person: int}) =
      T.output(os, "INF"^","^sI time^","^sI area_t^","^sI person^"\n")
  in
    (T.output(os, sI(length xs)^"\n"); app wrt xs; T.closeOut os)
  end

end
