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

  fun conf
    ({super, park, home, corp, school, train}) 
    (infectRule:{tag:string,n:int,rule:belongSpec}) = 
    {betaNSuper = super  * Type.gamma
    ,betaNPark  = park   * Type.gamma
    ,betaNHome  = home   * Type.gamma
    ,betaNCorp  = corp   * Type.gamma
    ,betaNSch   = school * Type.gamma
    ,betaNTrain = train  * Type.gamma
    ,infectRule = infectRule
    ,nPop       = 3000
    ,tag        = 
      String.concatWith "_" (
        map Real.toString [super,park,home,corp,school,train])
    ,mcid       = "0"
    }

  fun tasks_form n eff
   = GenTask.dup' (n, conf eff
        {tag = "EMP_30_JOJ_SJK"
        ,n    = 30
        ,rule = {role   = ROL_SOME Employed
                ,livein = LIV_SOME JOJ
                ,workat = WOR_SOME [(SJK,Corp)]
                }
        }) 
   @ GenTask.dup' (n, conf eff
        {tag = "EMP_30_JOJ_LOCAL"
        ,n    = 30
        ,rule = {role   = ROL_SOME Employed
                ,livein = LIV_SOME JOJ
                ,workat = WOR_LOCAL
                }
        })
   @ GenTask.dup' (n, conf eff
        {tag = "HUS_30_JOJ_LOCAL"
        ,n    = 30
        ,rule = {role   = ROL_SOME Hausfrau
                ,livein = LIV_SOME JOJ
                ,workat = WOR_LOCAL
                }
        }) 
   @ GenTask.dup' (n, conf eff
        {tag = "EMP_30_HAC_SJK"
        ,n    = 30
        ,rule = {role   = ROL_SOME Employed
                ,livein = LIV_SOME HAC
                ,workat = WOR_SOME [(SJK,Corp)]
                }
        }) 
   @ GenTask.dup' (n, conf eff
        {tag = "CRM_30_JOJ_SNJ"
        ,n    = 30
        ,rule = {role   = ROL_SOME Student
                ,livein = LIV_SOME HAC
                ,workat = WOR_SOME [(SJK,Cram)]
                }
        }) 

  val tasks 
    = tasks_form 12
      {super  = 0.3
      ,park   = 0.5 
      ,home   = 1.5 
      ,corp   = 2.0 
      ,school = 2.5 
      ,train  = 3.0
      }
    @ tasks_form 12
      {super = 0.3 
      ,park = 0.5
      ,home = 1.2 
      ,corp = 1.5 
      ,school = 1.8
      ,train = 3.0
      }

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
  val outbase = "test/trial05"
  val idxTask = Int32.toInt (me + offset)
  (*val tasks = GenTask.gen3 {setO = [1.2, 1.5, 1.8], setTr = [1.8, 2.0, 3.0], * nDup = 12} *)

  val nTasks = length tasks

  val _ = 
    if idxTask < nTasks then
      let
        val conf    = List.nth(tasks, idxTask)
        val tagbase = #tag conf ^ "_" ^ #tag (#infectRule conf)
        val tag      = #tag conf ^ "@" ^ #mcid conf
        val _ = MPI.barrier MPI.COMM_WORLD
        val _ = 
          if valOf (Int.fromString (#mcid conf)) = 0
            then OS.FileSys.mkDir (outbase^"/"^tagbase) handle _ => () 
            else ()
        val _ = MPI.barrier MPI.COMM_WORLD

         (* 判断がむつかしいが、まちを構成するまでは共通の乱数列を使う *)
        val _ = Trivial.inirnd 0;
        val city = Trivial.city conf;

        val tStop = 360*Type.days';

        (* シミュレーション中はランク毎に違う乱数列を使う *)
        val _ = Trivial.inirnd idxTask
      in
        ignore (
          Trivial.run1 {conf=conf, recstep=180, tStop=tStop, tag=tag
                       , dir=outbase^"/"^tagbase, city=city}
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
