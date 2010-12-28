structure JCL1 = struct
  (*
  fun echoOff () = Control.Print.out := {flush = fn _ => (), say = fn _ => ()} ;
  fun echoOn () = Control.Print.out := {flush = Control.Print.flush, say = Control.Print.say} ;
  *)

  structure M = MPI.Marshall;
  structure U = MPI.Unmarshall;

  open Type
    ; infix 1 @@
    ; infix 9 $

  val novac = {vacEff = 0.0, vacTrCover = 0.0, vacSchCover = 0.0}

  fun conf
    ({super, park, home, corp, school, train})
    ({vacEff:real, vacTrCover:real, vacSchCover:real})
    (infectRule:{tag:string,n:int,rule:belongSpec})
  = {betaNSuper = super  * Type.gamma
    ,betaNPark  = park   * Type.gamma
    ,betaNHome  = home   * Type.gamma
    ,betaNCorp  = corp   * Type.gamma
    ,betaNSch   = school * Type.gamma
    ,betaNTrain = train  * Type.gamma
    ,infectRule = infectRule
    ,vacEff     = vacEff
    ,vacTrCover = vacTrCover
    ,vacSchCover= vacSchCover
    ,nPop       = 3000
    ,tag        = 
      String.concatWith "_" 
        ( map Real.toString [super,park,home,corp,school,train]
        @ [#tag infectRule]
        @ map Real.toString [vacEff,vacTrCover,vacSchCover])
    ,mcid       = "0"
    }: Trivial.conf

  fun tasks_form n eff vac
   = GenTask.dup' (n, conf eff vac
        {tag = "EMP_30_JOJ_SJK"
        ,n    = 30
        ,rule = {role   = ROL_SOME Employed
                ,livein = LIV_SOME JOJ
                ,workat = WOR_SOME [(SJK,Corp)]
                }
        }) 
   @ GenTask.dup' (n, conf eff vac
        {tag = "EMP_30_JOJ_LOCAL"
        ,n    = 30
        ,rule = {role   = ROL_SOME Employed
                ,livein = LIV_SOME JOJ
                ,workat = WOR_LOCAL
                }
        })
   @ GenTask.dup' (n, conf eff vac
        {tag = "HUS_30_JOJ_LOCAL"
        ,n    = 30
        ,rule = {role   = ROL_SOME Hausfrau
                ,livein = LIV_SOME JOJ
                ,workat = WOR_LOCAL
                }
        }) 
   @ GenTask.dup' (n, conf eff vac
        {tag = "EMP_30_HAC_SJK"
        ,n    = 30
        ,rule = {role   = ROL_SOME Employed
                ,livein = LIV_SOME HAC
                ,workat = WOR_SOME [(SJK,Corp)]
                }
        }) 
   @ GenTask.dup' (n, conf eff vac
        {tag = "CRM_30_HAC_SNJ"
        ,n    = 30
        ,rule = {role   = ROL_SOME Student
                ,livein = LIV_SOME HAC
                ,workat = WOR_SOME [(SJK,Cram)]
                }
        }) 

  fun tasks_form2 n eff vac
   = GenTask.dup' (n, conf eff vac
        {tag = "EMP_60_TKY_SJK"
        ,n    = 60
        ,rule = {role   = ROL_SOME Employed
                ,livein = LIV_SOME TKY
                ,workat = WOR_SOME [(SJK,Corp)]
                }
        }) 
   @ GenTask.dup' (n, conf eff vac
        {tag = "EMP_60_TAC_SJK"
        ,n    = 60
        ,rule = {role   = ROL_SOME Employed
                ,livein = LIV_SOME TAC
                ,workat = WOR_SOME [(SJK,Corp)]
                }
        }) 
   @ GenTask.dup' (n, conf eff vac
        {tag = "EMP_60_JOJ_TKY"
        ,n    = 60
        ,rule = {role   = ROL_SOME Employed
                ,livein = LIV_SOME JOJ
                ,workat = WOR_SOME [(TKY,Corp)]
                }
        }) 
   @ GenTask.dup' (n, conf eff vac
        {tag = "EMP_60_JOJ_TAC"
        ,n    = 60
        ,rule = {role   = ROL_SOME Employed
                ,livein = LIV_SOME JOJ
                ,workat = WOR_SOME [(TAC,Corp)]
                }
        }) 

  val eff3 = {super = 0.3, park = 0.5, home = 1.5, corp = 4.0, school = 5.0,train = 6.0}

 
  fun tasks_form3 vac = hd (tasks_form 1 eff3 vac)

  fun mapConcat f = List.concat o map f
  val tasks: Trivial.conf list = 
     mapConcat (fn vacCover =>
       mapConcat (fn vacEff =>
          [tasks_form3 {vacEff = vacEff, vacTrCover = vacCover, vacSchCover = 0.0}
          ,tasks_form3 {vacEff = vacEff, vacTrCover = 0.0, vacSchCover = vacCover}
          ]
       ) [0.1, 0.3, 0.5]
     ) [0.5, 0.6, 0.7, 0.8, 1.0]

(*
fun come_from_test ()  = let
  val conf = List.nth(tasks, 16*1)
  val _ = Trivial.inirnd 0
  val city = Trivial.city conf
  val xs = 
    Vector.map (fn area => List.filter 
      (fn PERSON {health,...} => case hd health of EXP _ => true | _ => false) (#2 area)
    ) (#area city)
  val xs = Vector.foldl (fn (a,b) => List.@ (b,a)) nil xs
  fun corp (PERSON p) = valOf 
    (List.find (fn {place_k,...} => place_k = Corp) (#belong p))
in
  (map (Frame.whereComeFromTo city o corp) xs, city)
end
*)

fun main offset = let
  val t0 = Time.now();
  val _ = MPI.init();
  val nproc = MPI.comm_size();
  val me    = MPI.comm_rank();

  (* 担当タスクの決定 *)
  val outbase = "test/trial01b"
  val idxTask = Int32.toInt (me + offset)
  (*val tasks = GenTask.gen3 {setO = [1.2, 1.5, 1.8], setTr = [1.8, 2.0, 3.0], * nDup = 12} *)

  val nTasks = length tasks

  val _ = 
    if idxTask < nTasks then
      let
        val conf     = List.nth(tasks, idxTask)
        val tagbase  = #tag conf 
        val tag      = #tag conf ^ "@" ^ #mcid conf
        val _ = MPI.barrier MPI.COMM_WORLD
        val _ = 
          if valOf (Int.fromString (#mcid conf)) = 0
            then X_Misc.mkDir (outbase^"/"^tagbase) handle _ => () 
            else ()
        val _ = MPI.barrier MPI.COMM_WORLD

       (* まちを構成するまでは共通の乱数列を使う *)
        val _ = Trivial.inirnd 0;
        val city = Trivial.city conf;

        val tStop = 360*Type.days';

       (* シミュレーション中はMonte Carlo実験毎に違う乱数列を使う *)
        val _ = Trivial.inirnd (Alice.iS (#mcid conf))
      in
        ignore (
          Trivial.run1 {conf=conf, recstep=180, tStop=tStop, tag=tag
                       ,dir=outbase^"/"^tagbase, city=city}
        )
      end
    else
      (MPI.barrier MPI.COMM_WORLD
      ;MPI.barrier MPI.COMM_WORLD
      ;()
      )

  val _ = MPI.finalize();

  val t1 = Time.now();
  val () = print ("elapsed time: " ^ Time.toString (Time.-(t1,t0)) ^ "[sec]\n")

in
  OS.Process.success
end end
