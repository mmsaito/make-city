structure JCL1 = struct
  (*
  fun echoOff () = Control.Print.out := {flush = fn _ => (), say = fn _ => ()} ;
  fun echoOn () = Control.Print.out := {flush = Control.Print.flush, say = Control.Print.say} ;
  *)

(* ワクチンがかかったひと
 Vector.map (fn area => List.filter (fn PERSON p => List.exists (fn VAC _ => true | _ => false) (#health p)) (popArea area))  (#area city) ;
 *)

  structure M = MPI.Marshall;
  structure U = MPI.Unmarshall;
  structure P = EasyPrint;

  open Type
    ; infix 1 @@
    ; infix 9 $

  val novac = {vacEff = 0.0, vacTrCover = 0.0, vacSchCover = 0.0}

  val nPlaces = 
    #[{sch = 2, corp = 4, cram = 0, super = 10, park = 2}
     ,{sch = 2, corp = 8, cram = 1, super = 10, park = 2}
     ,{sch = 2, corp = 4, cram = 0, super = 10, park = 2}
     ,{sch = 3, corp =30, cram = 1, super = 10, park = 2}
     ,{sch = 2, corp =30, cram = 0, super = 10, park = 2}
     ]

  fun conf'
    ({super, park, home, corp, school, train})
    ({vacEff:real, vacTrCover:real, vacSchCover:real})
    (infectRule:{tag:string,n:int,rule:belongSpec, isRandom:bool})
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
    ,nPlaces    = nPlaces
    ,tag        = 
      String.concatWith "_" 
        ( map Real.toString [super,park,home,corp,school,train]
        @ [#tag infectRule]
        @ map Real.toString [vacEff,vacTrCover,vacSchCover])
    ,mcid       = "0"
    }: Trivial.conf

  fun conf a b {tag,n,rule} = conf' a b {tag = tag, n = n, rule = rule, isRandom= false}

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
  val eff4 = {super = 0.3, park = 0.5, home = 1.5, corp = 3.0, school = 4.0,train = 6.0}

 
  fun tasks_form3 vac = hd (tasks_form 1 eff4 vac)
(*
  fun mapConcat f = List.concat o map f
  val tasks: Trivial.conf list = 
     mapConcat (fn vacCover =>
       mapConcat (fn vacEff =>
          [tasks_form3 {vacEff = vacEff, vacTrCover = vacCover, vacSchCover = 0.0     }
          ,tasks_form3 {vacEff = vacEff, vacTrCover = 0.0     , vacSchCover = vacCover}
          ]
       ) [0.1, 0.3, 0.5]
     ) [0.5, 0.6, 0.7, 0.8, 1.0]
*)

  val tasks =
    GenTask.dup' (16384, conf' eff4 novac
        {tag  = "ARBIT_30_JOJ_ARBIT"
        ,n    = 30
        ,rule = {role   = ROL_ARBIT (* ignored *)
                ,livein = LIV_SOME JOJ
                ,workat = WOR_ARBIT (* ignored *)
                }:belongSpec
        ,isRandom = true
        }) 

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

  fun take (xs, n) = List.take(xs, Int.min(n,length(xs)))
(*
  val mem_cl = 
    List.concat (
      List.tabulate(4, fn i =>
        map (valOf o Int.fromString) (take(X_Misc.readF("cl_mem3@" ^ Int.toString (i + 1) ^ ".csv"), 256))
      )
    )

  val tasks =
    map (
      GenTask.setMcid (conf' eff4 novac
          {tag  = "ARBIT_30_JOJ_ARBIT"
          ,n    = 30
          ,rule = {role   = ROL_ARBIT (* ignored *)
                  ,livein = LIV_SOME JOJ
                  ,workat = WOR_ARBIT (* ignored *)
                  }:belongSpec
          ,isRandom = true
          }) 
    ) mem_cl
*)

val tStop = 360*Type.days';

fun main offset = let
  val t0 = Time.now();
  val _ = MPI.init();
  val nproc = MPI.comm_size();
  val me    = MPI.comm_rank();

  (* 担当タスクの決定 *)
  val outbase = "test/trial_big"
  val idxTask = Int32.toInt (me + offset)


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
        val _ = Trivial.inirnd 0
        val city = Trivial.city conf

       (* シミュレーション中はMonte Carlo実験毎に違う乱数列を使う *)
        val _ = Trivial.inirnd (Alice.iS (#mcid conf))
        val city = Trivial.infectVac conf city
      in
        ignore (
          Trivial.run1 {conf=conf, recstep=180, tStop=tStop, tag=tag
                       ,dir=outbase^"/"^tagbase, city=city
                       ,seq=true
                       }
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
