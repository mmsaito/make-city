structure JCL1 = struct
  structure M = MPI.Marshall;
  structure U = MPI.Unmarshall;
  structure P = EasyPrint;

  open Type
  val tasks   = Tasks2.tasks1 @ Tasks2.tasks2
  val tStop   = 360*Type.days';
  val outbase = "test/scale"

  fun evalConf {saveCity:bool, run:bool, mpi: bool} conf = let
    fun appIf true  f a = ignore (f a)
      | appIf false f a = ()
    
    val tagbase  = #tag conf
    val tag      = #tag conf ^ "@" ^ #mcid conf
    val () = appIf mpi MPI.barrier MPI.COMM_WORLD
    val () = 
      if valOf (Int.fromString (#mcid conf)) = 0
        then X_Misc.mkDir (outbase^"/"^tagbase) handle _ => () 
        else ()
    val _ = appIf mpi MPI.barrier MPI.COMM_WORLD

    (* まちを構成するまでは共通の乱数列を使う *)
    val _ = Trivial.inirnd 0
    val city = Trivial.city conf

    (* シミュレーション中はMonte Carlo実験毎に違う乱数列を使う *)
    val _ = Trivial.inirnd (Alice.iS (#mcid conf))
    val city = Trivial.infectVac conf city

    val () = appIf saveCity (Probe.writeCity city) (outbase^"/"^tagbase^".city")
  in
    appIf run Trivial.run1 {conf=conf, recstep=180, tStop=tStop, tag=tag
                           ,dir=outbase^"/"^tagbase, city=city
                           ,seq=true
                           }
  end

  fun main offset = let
    val t0    = Time.now();
    val _     = MPI.init();
    val nproc = MPI.comm_size();
    val me    = MPI.comm_rank();

    (* 担当タスクの決定 *)
    val idxTask = Int32.toInt (me + offset)
    val nTasks  = length tasks

    val _ = 
      if idxTask < nTasks then
        evalConf {saveCity = false, run = true, mpi = true} (List.nth(tasks, idxTask))
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
  end 
end

(* 以下は、後で使うかもなコード断片 *)
(*
  fun take (xs, n) = List.take(xs, Int.min(n,length(xs)))
  fun echoOff () = Control.Print.out := {flush = fn _ => (), say = fn _ => ()} ;
  fun echoOn () = Control.Print.out := {flush = Control.Print.flush, say = Control.Print.say} ;
*)
