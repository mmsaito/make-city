(* �l���ƐU�镑���̋�̓I�ȃf�U�C�� *)
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

  (* ���f���ݒ�p�����[�^:
    ������̒���: ���e��tenative����adhoc�ł���B�^�𗧂Ă�̂́A�L�q��̕֋X�̂��߂ł����āA
      �݌v��̗��R�ł͂Ȃ��B
     - 12/16�ǋL:
       - ���f���ɏo�Ă��鐔�l�p�����[�^��conf�ɒu�����Ƃɂ���ƁA��Ƃ����ɂ����Ȃ�B
       - �����ŁA�p�����[�^�T���̑ΏۂƂȂ����(����́A�����̐i�s�ƂƂ��ɕω���
         �闬���I�Ȃ��̂ł���) ������conf�ɏ������Ƃɂ���B
  *)
  type conf = 
    {betaNHome  : real
    ,betaNSch   : real
    ,betaNSuper : real
    ,betaNCorp  : real
    ,betaNPark  : real
    ,betaNTrain : real
    ,infectRule : {tag:string, n:int, rule:belongSpec, isRandom:bool}
    ,nPlaces    : {cram:int, sch:int, corp:int, park:int, super:int} vector
    ,nPop       : int vector (* �ЂƂ̊X�̐l���B��Ŕz��ɂ��� *)
    ,vacEff     : real (* ���N�`������ *)
    ,vacTrCover : real (* �ڎ���{�� *)
    ,vacSchCover: real (* �ڎ���{�� *)
    ,tag        : string
    ,mcid       : string
    }

  (* �e�X�g�f�[�^ *)
  val frau = let
    val vis = {place_k = Home , area_t = 0, id = 0, tVis = ref ~1}: place_t
  in PERSON 
    {age=23.0:age, gender = F
    ,belong = [ {place_k = Super, area_t = 0, id = 0, tVis = ref ~1}
              , {place_k = Park , area_t = 0, id = 0, tVis = ref ~1}
              , {place_k = Corp , area_t = 0, id = 0, tVis = ref ~1}
              , vis
              ]: place_t list
    ,visit  = vis
    ,role   = Hausfrau
    ,dest   = NONE: place_t option
    ,health = nil: health list
    ,sched  = nil: (time * place_t) list
    ,mkSched  = (fn _ => nil): person * time -> (time * place_t) list
    }
  end

  (* �l�����z�Ɏ����ȒP�ȕ��z������:   �N���[�N���z *)
  (* ====================================================================  *)
  (* �������[�� *)

  (* �s���e���v���[�g *)
  (* ���̂悤�ȐU�镑��
   * ��Ј�: ���`���͉�ЂցA�y���͉�ЈȊO�ɓK����
   * �w��  : ���`���͊w�Z�ցA�l�ɂ���Ă͏m�ʂ������Ă���B
   * ��w  : ���n��̓X�[�p�[�ɍs���B
   *
   *)
  
  (* ��Ј��̃X�P�W���[�� *)
  fun schedEmp (PERSON p, t:time): (time * place_t) list = let
    val rnd = getrnd()
    val {day,weekday,hour,step} = timecomp t
    fun home() = valOf (List.find (fn {place_k = Home, ...} => true | _ => false) (#belong p))
    fun corp() = valOf (List.find (fn {place_k = Corp, ...} => true | _ => false) (#belong p))
    val today = day*days'
  in
    if (step = 0) then (* �����ߑO0���ɃX�P�W���[���𗧂Ă� *)
      if (1 <= weekday andalso weekday <= 5) then
          [ (today + irndIn rnd ( 6*hours',10*hours'), corp())
          , (today + irndIn rnd (18*hours',22*hours'), home())
          ]
      else
        Misc.qsortL (fn ((t,_),(t',_)) => Int.compare(t,t')) (
          (today + 18*hours', home()) ::
          ListPair.zip
            ( List.tabulate
               ( irndIn rnd (1,length (#belong p))
               , fn _ => today + irndIn rnd (10*hours',18*hours'))
            , #belong p)
        ) 
    else
      #sched p
  end

  (* �w���̃X�P�W���[�� *)
  fun schedStu (PERSON p, t:time): (time * place_t) list = let
    val rnd = getrnd()
    val {day=day,weekday,hour,step} = timecomp t
    val today = day*days'
    fun home() = valOf (List.find (fn {place_k = Home, ...} => true | _ => false) (#belong p))
    fun sch()  = valOf (List.find (fn {place_k = Sch , ...} => true | _ => false) (#belong p))
    fun cram() = List.find (fn {place_k = Cram, ...} => true | _ => false) (#belong p)
  in
    if (step = 0) then (* �����ߑO0���ɃX�P�W���[���𗧂Ă� *)
      if 1 <= weekday andalso weekday <= 5 then
          [ (today + irndIn rnd ( 6*hours', 8*hours'), sch() )
          , (today + irndIn rnd (16*hours',19*hours'), home())
          ] 
          @(case cram() 
            of SOME cram => 
              [ (today + irndIn rnd (19*hours', 20*hours'), cram)
              , (today + irndIn rnd (21*hours', 22*hours'), home())
              ]
             | NONE => nil)
      else
        (* 10���`18���̊ԁA�K���Ȏ��ԂɓK���ȏꏊ������� *)
        (* �_�u���u�b�L���O���������邪...�ǂ����悤�� *)
        Misc.qsortL (fn ((t,_),(t',_)) => Int.compare(t,t')) (
          (today + 18*hours', home()) ::
          List.tabulate
            (irndIn rnd (1,length (#belong p))
            , fn _ => 
              (today + irndIn rnd (10*hours',18*hours')
              ,List.nth(#belong p, irndIn rnd (0, length(#belong p)))
              )
            )
        )
    else
      #sched p
  end

  fun schedHaus (PERSON p, t:time): (time * place_t) list = let
    val rnd = getrnd()
    (* ����: �����ɂ́A��ԉ��Z������K�v�����邪�A�ʓ|�Ȃ̂ł��ڂ��Ă��� *)
    val {day=day,weekday,hour,step} = timecomp t
    val today = day*days'
    fun supers() = List.filter (fn p => #place_k p = Super) (#belong p)
    val sort = Misc.qsortL (fn ((t,_,_),(t',_,_)) => Int.compare (t,t')) 
  in
    if (step = 0) then
      let 
        (* (1) �ꏊ���Ƃ̑؍݊��Ԃ��v�Z���� *)
        (* (1a) ����̂ǂ����̎��_�ł�������1���Ԃ̔����������� *)
        val tInSuper  = today    + irndIn rnd (10*hours', 20*hours')
        val tOutSuper = tInSuper + irndIn rnd (10*minutes', 1*hours')
        (* �s������X�[�p�[��2�ȏ゠���Ă�������������ʁB *)
        val schedS = (tInSuper, tOutSuper, rndSelL rnd (supers()))

        (* (1b) �ʂ̏ꏊ�ɂ��s����������Ȃ� *)
        val others = List.filter (fn {place_k=x,...} => x <> Super andalso x <> Home) (#belong p)
        val nO = irndIn rnd (0,length others + 1)
        val schedO = List.tabulate 
          ( nO, fn _ => let val tIn  = today + irndIn rnd (10*hours', 20*hours')
                            val tOut = tIn   + irndIn rnd (0*hours' , 2*hours' )
                        in (tIn, tOut, rndSelL rnd others) end)

        val home = valOf (List.find (fn p => #place_k p = Home) (#belong p))
        (* (2) �ꏊ���̑؍ݎ����̊Ԋu���痷����A������ *)
        fun connect (x::x'::xs) = 
          let val (tb,te,p) = x
              val (tb',te',p') = x'
          in
             if tb + 2*hours' < tb' 
               then (tb,p)::(te,home)::connect (x'::xs) 
               else (tb,p)           ::connect (x'::xs)
          end
          | connect ((tb,te,p)::nil) = (tb,p)::(te,home)::nil (* ���Â�A�ƂɋA�� *)
          | connect nil = nil (* unreachable *)
      in
        connect (sort (schedS :: schedO))
      end
    else
      #sched p
  end

  (* �l�Ԑ������[�� *)
  fun rulePerson id = let
    val rnd = getrnd()
    val age    = Real.abs (30.0 + 30.0*rgauss rnd)
    val gender = rndSel rnd 0.5 (F,M)
    val (role,sched)   = 
      if (age <= 22.0)     then (Student, schedStu)
      else if (age < 60.0) then
        case gender
          of F => rndSel rnd 0.6 ((Employed,schedEmp), (Hausfrau,schedHaus))
           | M => rndSel rnd 0.8 ((Employed,schedEmp), (Hausfrau,schedHaus))
      else
        (Hausfrau,schedHaus)
  in
    {age = age, gender = gender, role = role, mkSched = sched}
  end

  (* �Ƒ��\�����[�� *)
  fun ruleHome () =  let val rnd = getrnd() in
    List.tabulate (1 + iR (abs (2.0 * rgauss rnd)), fn _ => fn _ => true) end

  (* ������Ԑ������[�� *)
  fun rulePlace' betaN place_t = let
    val rnd = getrnd()
  in
    {id    = place_t 
    ,nVis  = zeroNVis ()
    ,pTrns = zeroPTrns ()
    ,size  = 0 (* no longer referred *)
    ,betaN = abs (betaN + 0.1*betaN*rgauss rnd)
    }: place
  end

  fun rulePlace place_k betaN area_t id = 
    rulePlace' betaN {place_k = place_k, area_t = area_t, id = id, tVis = ref ~1}

  (* �s���͈̓��[�� *) 
  fun ruleVisit (areas: area vector): person -> place_t list = let
    val rnd = getrnd()
    val nCorp =
      Vector.map (fn area => Vector.length (#corp (#3 area))) areas
    val nSumCorp = Vector.foldl (op + ) 0 nCorp
    fun selCorp n = let
      val pr_area = Misc.listV 
        (Vector.mapi (fn (i,nC) => (rI nC / rI nSumCorp, #3 (areas $ i))) nCorp)
    in
      List.tabulate(n, fn _ => rndSelV rnd (#corp (rndSelLP rnd pr_area)))
    end
  in  
    fn (PERSON p: person) => let
    val i0 = areaPer (PERSON p)
    val (_,_,a0) = areas $ i0
    val (_,_,a') = rndSelV rnd areas

    infixr 2 `::
    fun op `:: (SOME x,xs) = x :: xs
      | op `:: (NONE  ,xs) = xs
 
    fun crams() = List.filter (fn x => Vector.length x > 0) 
                   (Misc.listV (Vector.map (#cram o #3) areas))
    val selCram = rndSelLP rnd
      [(0.4, fn () => SOME (rndSelV rnd (rndSelL rnd (crams()))))
      ,(0.6, fn () => NONE)]

    fun selSch () = let
      val a = rndSelLP rnd [(0.8,a0), (0.2, a')]  (* 8���͒n���ցA�c��2����͊w��O�� *)
    in
      rndSelV rnd (#sch a)
    end
  in
    case #role p
      (* 23.2.18 �O�΂̎����B�Ƃ肠�����A�R�[�h���ςł���Ă݂�B*)
      (* of Employed => map #id [selCorp(), rndSelV rnd (#super a0)] *)
      of Employed => map #id (rndSelV rnd (#super a0) :: selCorp 5)
       | Student  => map #id (selCram() `:: [selSch (), rndSelV rnd (#super a0)])
       | HausFrau => map #id [rndSelV rnd (#super a0), rndSelV rnd (#super a'), rndSelV rnd (#park a0) ]
  end
  end

  (* �S���^�s���[�� *)
  val time2next = [(TKY,5),(SJK,10),(JOJ,15),(TAC,20),(HAC,0)]
  val services =
    (Vector.fromList o List.concat) 
    (map (fn h => 
      List.concat (map (fn m =>
       [ {deptime = iR (rI h*hours + rI m*minutes), stations = #[TKY, SJK, JOJ, TAC, HAC]} 
       , {deptime = iR (rI h*hours + rI m*minutes), stations = #[HAC, TAC, JOJ, SJK, TKY]} 
       ]) [0, 20, 40]) 
          (* [0, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55]) *)
     ) [6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23])

  (* �w�Z��,��А��̕\ �� 2/1�̍l�@�ɂ��p�~ *)
  (* 
  fun makePopTable (conf:conf) = let
    val nAvgPop  = #nPop conf
    val nCityPop = nAvgPop * 5
    val nTownPop = nAvgPop
    val stuRate  = 0.20 (* �l���ɐ�߂�w���̊��� *)
    val empRate  = 0.50 (* �l���ɐ�߂�]�ƈ��̊��� *)
    val wEmpPerCorp = 100.0 (* 1�Ђ�����̏]�ƈ��� *)
    val popTable0 =
     [{area_t = HAC, wStuPerSch = 424.0, wCorp =  5.0, nTownPop = nTownPop}
     ,{area_t = TAC, wStuPerSch = 442.0, wCorp = 10.0, nTownPop = nTownPop}
     ,{area_t = JOJ, wStuPerSch = 419.0, wCorp =  5.0, nTownPop = nTownPop}
     ,{area_t = SJK, wStuPerSch = 277.0, wCorp = 40.0, nTownPop = nTownPop}
     ,{area_t = TKY, wStuPerSch = 348.0, wCorp = 40.0, nTownPop = nTownPop}
     ]
    val wSumCorp = foldl (fn ({wCorp,...},s) => s + wCorp) 0.0 popTable0
    fun norm {area_t = n, wStuPerSch, wCorp, nTownPop} =
      {area_t = n
      ,nSch   = Real.ceil (rI nTownPop*stuRate/wStuPerSch)
      ,nCorp  = Real.ceil (rI nCityPop*empRate/wEmpPerCorp*(wCorp/wSumCorp))
      }
  in
    map norm popTable0
  end
  *)

  (* ====================================================================  *)
  (* ��L���[���ɂ��s�s�̑g�ݗ��� *)
  fun perHome (conf:conf) at = let
    val betaN = fn () => abs (#betaNHome conf*(1.0 + 0.1*rgauss (getrnd())))
  in
    F.makeHome at betaN (F.makePerson (#nPop conf $ at) rulePerson) ruleHome
  end

  fun place (conf:conf) at home = let 
      val nPlaces = #nPlaces conf $ at
  in 
    F.makePlace at home
        [(#cram  nPlaces, rulePlace Cram  (#betaNSch   conf))
        ,(#sch   nPlaces, rulePlace Sch   (#betaNSch   conf))
        ,(#super nPlaces, rulePlace Super (#betaNSuper conf))
        ,(#corp  nPlaces, rulePlace Corp  (#betaNCorp  conf))
        ,(#park  nPlaces, rulePlace Park  (#betaNPark  conf))
        ]
  end

  fun area (conf:conf) = 
    Vector.map 
      (fn at => 
        (fn (per,home) =>
          (at, per, place conf at home)
        ) (perHome conf at)
      ) #[HAC, TAC, JOJ, SJK, TKY]

  fun train (conf:conf) = F.makeTrains 
    {time2next = time2next, services = services, betaN = #betaNTrain conf, size = 200}

  fun infectVac (conf:conf) (city:city) = let
    val () = F.setVacEff (#vacEff conf)
    val rnd = getrnd()
    val vac: area -> area =
      F.vacWho (fn p => F.ruleVacTrain  {cover = #vacTrCover  conf, eff = #vacEff conf} p orelse 
                        F.ruleVacSchool {cover = #vacSchCover conf, eff = #vacEff conf} p)
    val nInf = #n (#infectRule conf)
    val rule = #rule (#infectRule conf)
    (* ����: �����̒��o���@�̈Ⴂ(����I/�����_��)�̑Ή��́A���ɏꓖ����I�Ȃ��̂ł��� *)
    fun infect (area:area) = 
      case #isRandom (#infectRule conf)
        of false => F.ruleInfect nInf rule area
         | true  =>
            (case #livein rule
               of LIV_SOME area_t =>
                 if area_t = idArea area 
                   then F.distInfect rnd [(nInf,INF 0)] area
                   else area
                | LIV_ARBIT =>
                   F.distInfect rnd [(nInf div 5, INF 0)] area
                   (* �}�W�b�N�i���o�[ 5 = �s�s�� *)
            )
  in
    {area  = Vector.map (infect o vac) (#area city)
    ,train = #train city
    ,time  = #time city
    }
  end

  fun city (conf:conf) = 
    F.evalPlace
      {area  = F.makeVisit' (area conf) ruleVisit
      ,train = train conf
      ,time  = 0
      }

  (* 3000�l �A1���ŁA45[sec] 
     150���l�A1���ŁA6.25[����]    (���X�P�[��)
     15���l �A1���ŁA37.5[��]      (1/10�X�P�[��)
     15���l �A1�T�ԂŁA4.375[����] (1/10�X�P�[��)
     15���l �A6�����ŁA4.6875[��]  (1/10�X�P�[��)
   * *)

  (* �ݒ�̏����o���A�ƂĂ� �Ă񂽂Ă��� *)
  fun writeConf (conf:conf) f = let
    val os = T.openOut f
  in
    (T.output(os, "alpha,"     ^ fG 14 Type.alpha ^ "\n")
    ;T.output(os, "gamma,"     ^ fG 14 Type.gamma ^ "\n")
    ;T.output(os, "betaNHome," ^ fG 14 (#betaNHome conf) ^ "\n")
    ;T.output(os, "betaNSch,"  ^ fG 14 (#betaNSch conf) ^ "\n")
    ;T.output(os, "betaNCorp," ^ fG 14 (#betaNCorp conf) ^ "\n")
    ;T.output(os, "betaNSuper,"^ fG 14 (#betaNSuper conf) ^ "\n")
    ;T.output(os, "betaNTrain,"^ fG 14 (#betaNTrain conf) ^ "\n")
    (* ;T.output(os, "e0_JOJ,"    ^ fI (#e0_JOJ conf) ^ "\n") *)
    ;T.closeOut os
    )
  end

    fun roleSym Employed = #"E"
      | roleSym Hausfrau = #"H"
      | roleSym Student  = #"S"

    fun symOrd (#"S",#"S") = EQUAL
      | symOrd (#"S",_   ) = GREATER
      | symOrd (#"E",#"S") = LESS
      | symOrd (#"E",#"E") = EQUAL
      | symOrd (#"E",_   ) = GREATER
      | symOrd (#"H",#"H") = EQUAL
      | symOrd (#"H",_   ) = LESS

    fun infSeq (city:city) = let
      fun inArea (area:area) = 
        List.mapPartial
          (fn PERSON p => 
            case hd (#health p)
              of INF _ => SOME (roleSym (#role p)) | _ => NONE) (popArea area) 
      val seq = List.concat (Misc.listV (Vector.map inArea (#area city)))
    in
      String.implode (Misc.qsortL symOrd seq)
    end


  (* �g�b�v���x���֐� *)
  fun run1 {conf:conf, recstep, tStop, dir, tag, city, seq} = let
    val () = writeConf conf (dir^"/conf_"^ tag^".csv")
    val os = T.openOut      (dir^"/pop_" ^ tag^".csv")
    val os2= if seq then SOME (T.openOut  (dir^"/seq_" ^ tag^".csv")) else NONE
    val () = Option.app (fn os2 => 
      (T.output(os2, #mcid conf ^ " ")
      ;T.output(os2, infSeq city)
      ;T.output(os2, ".")
      )) os2

    val () = Probe.showPopTag os 5
    val nstep   = tStop div recstep
    fun step city = let
      val city = Iterator.applyN (F.advanceTime os2) recstep city
      val pop  = Probe.reducePop' city
      (* val _ = print (sI (#time city) ^ "\n") *)
    in
      city before Probe.showPop os (#time city) pop
    end
  in
    Iterator.applyN step nstep city 
      before (T.closeOut os
             ; Option.app (fn os => T.output(os, "\n")) os2
             ; Option.app T.closeOut os2
             )
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
      val city = Iterator.applyN (F.advanceTime NONE) recstep city
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
