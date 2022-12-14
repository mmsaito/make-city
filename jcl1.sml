structure JCL1 = struct
  structure M = MPI.Marshall;
  structure U = MPI.Unmarshall;
  structure P = EasyPrint;

  open Type
  val tasks   = Tasks2.tasks1 @ Tasks2.tasks2
  val tStop   = 360*Type.days';
  val outbase = ref "../code-unreal-2/test/scale2000"

  fun evalConf {saveCity:bool, run:bool, mpi: bool} conf = let
    fun appIf true  f a = SOME (f a)
      | appIf false f a = NONE
    
    val tagbase  = #tag conf
    val tag      = #tag conf ^ "@" ^ #mcid conf
    val _ = appIf mpi MPI.barrier MPI.COMM_WORLD
    val outdir = !outbase^"/"^tagbase
    val _ = print ("outdir = "^outdir^ "\n")
    val _ = 
      if valOf (Int.fromString (#mcid conf)) = 0
        then X_Misc.mkDir outdir handle _ => (print ("Can't dig " ^ outdir^ "\n"))
        else ()
    val _ = appIf mpi MPI.barrier MPI.COMM_WORLD

    (* まちを構成するまでは共通の乱数列を使う *)
    val _ = Trivial.inirnd 0;
    val city = Trivial.city conf

    (* シミュレーション中はMonte Carlo実験毎に違う乱数列を使う *)
    val _ = Trivial.inirnd (Alice.iS (#mcid conf))
    val interv = Trivial.mkIntervPlan conf city

    val _ = appIf saveCity (Probe.writeCity city) (!outbase^"/"^tagbase^"/model.city")
  in
    if run then
      Trivial.run1 {conf=conf, recstep=180, tStop=tStop, tag=tag
                   ,dir= !outbase^"/"^tagbase, city=city
                   ,seq=true}
    else
      city
  end
  val makeWriteCity = evalConf {saveCity=true, run=false, mpi=false}

  fun makeWriteInterv (conf) (city:city) subtag = let
    open Alice
    val interv = Trivial.mkIntervPlan conf city
    val () = X_Misc.mkDir (!outbase^"/"^(#tag conf)^"/"^subtag)
  in
    Probe.writeIntervPlan interv (!outbase^"/"^(#tag conf)^"/"^subtag^"/interv.csv")
  end

  val makeInterv = Trivial.mkIntervPlan

  fun writeInterv (conf:Trivial.conf) interv subtag = let
    open Alice
    val () = X_Misc.mkDir (!outbase^"/"^(#tag conf)^"/"^subtag)
  in
    Probe.writeIntervPlan interv (!outbase^"/"^(#tag conf)^"/"^subtag^"/interv.csv")
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
        ignore ( evalConf {saveCity = false, run = true, mpi = true} (List.nth(tasks, idxTask)))
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
