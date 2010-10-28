structure Trivial = struct
(* ================================================================ *)
(* ��̓I�ȍ\�������߂�葱�� *)
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

  (* �����Ɋւ��萔 *)
  val betaNHome  = 1.4 * gamma
  val betaNSch   = 1.8 * gamma
  val betaNSuper = 1.4 * gamma
  val betaNCorp  = 1.8 * gamma
  val e0_JOJ     = 30

  (* �l�����z�Ɏ����ȒP�ȕ��z������:   �N���[�N���z *)
  (* ====================================================================  *)
  (* �������[�� *)

  (* �s���e���v���[�g *)
  (* ���̂悤�ȐU�镑��
   * ��Ј�: ���`���͉�ЂցA�y���͉�ЈȊO�ɓK����
   * ��w  : ���n��̓X�[�p�[�ɍs���B
   *   p[1/step] * ��t^{-1}[steps/day] = n[/day] =>  p = n��t
   *
   * �V�~�����[�V������1�X�e�b�v��t�Ǝ��ԊԊu�̃X�e�b�v���\��
   *   - ��t��[day]�̒P�ʂŏ�����Ă���Ƃ��� 
   *     <=> ��t[days/step]            <=> ��t^{-1}[steps/day]
   *     <=> ��t^{-1}/24[steps/hour]   <=> 24��t [hour/steps]
   *     <=> ��t^{-1}/24/60[steps/min] <=> 24*60*��t [min/steps]
   *        per n unit = 1.0*unit / n
   *        unit = {day = 1, hour = 24, min = 24*60}
   * ��A����: �u�����A���v
   *   ... ��������A���X�e�B�b�N�ɏ����̂͂ނ������B��0�ߎ��Ƃ��āA
   *     - �ߌ�8�� - �ߌ�10��: �m�� p ��Home��ړI�l�ɃZ�b�g����B
   *       �������Ap = 24��t/2 = 12��t
   *     - �ߌ�11�� -        : �m��1��Home��ړI�l�ɃZ�b�g����B
   *
   * e-folding time �� T �Ƃ���ƁAt�����o�߂����Ƃ��̎c�����́A
       t = T: 36%, t = 2T: 13%, t = 3T: 5%, {t = 4T: 2%}, t = 5T: 0.7%
   * �o�Ύ�����N�X�e�b�v���Ȃ��ň�l�ɂ���ɂ́A
       pk = 1/(N - k).  (k=0,...,N-1)
   *)
  (* ��Ј��̃X�P�W���[�� *)
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
   
  (* �w���̃X�P�W���[�� *)
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

  (* ��w�̃X�P�W���[�� *)
  fun schedHaus (PERSON p, t:time): (real * place_t) list = let
    val Hausfrau = #role p
    val {day,weekday,hour,step} = timecomp t
    fun home() = valOf (List.find (fn {place_k = Home, ...} => true | _ => false) (#belong p))
  in
    if (6.0 <= hour andalso hour <= 8.0) then
      [(1.0, home())]
    else if (hour < 18.0) then
      (* �s����ꏊ�ɕ���1�񂸂s�� *)
      let
        val n  = Real.fromInt (length (#belong p))
        val pr = 1.0 times per 10.0 hours 
      in 
       (1.0 - n*pr, #visit p) :: map (fn place => (pr, place)) (#belong p)
      end
    else
      [(1.0, home())]
  end

  (* �l�Ԑ������[�� *)
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

  fun rulePlace place_k size betaN area_t id = 
    rulePlace' size betaN {place_k = place_k, area_t = area_t, id = id}

  (* �s���͈̓��[�� *) 
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

  (* �S���^�s���[�� *)
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
  (* ��L���[���ɂ��s�s�̑g�ݗ��� *)
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
  (* 3000�l �A1���ŁA45[sec] 
     150���l�A1���ŁA6.25[����]    (���X�P�[��)
     15���l �A1���ŁA37.5[��]      (1/10�X�P�[��)
     15���l �A1�T�ԂŁA4.375[����] (1/10�X�P�[��)
     15���l �A6�����ŁA4.6875[��]  (1/10�X�P�[��)
   * *)
  (* �ݒ�̏����o���A�ƂĂ� �Ă񂽂Ă��� *)
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

(* �����ɂ��� 
* �����̃f�o�b�O�ɂ́A�l���ǂ������������d�v�Ȃ̂ŁA�P���� (����,�s�s) ��
  �v���b�g������̂��悢���낤�B�͂��߂͂ʂ����������ŁB�������A�u�d�ԁv
  �ɂ���Ƃ��́A�d�Ԃ̌��݂ł͂Ȃ��A�u�d�ԁv�Ƃ������z�s�s�ɂ��邱�Ƃɂ���
  �̂��ǂ����낤�B
*)


