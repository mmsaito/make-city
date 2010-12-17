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

  fun main offset = let
  val t0 = Time.now();
  val _ = MPI.init();
  val nproc = MPI.comm_size();
  val me    = MPI.comm_rank();

  (* 担当タスクの決定 *)
  val outbase = "test"
  val idxTask = Int32.toInt (me + offset)
  (*val tasks = GenTask.gen3 {setO = [1.2, 1.5, 1.8], setTr = [1.8, 2.0, 3.0], * nDup = 12} *)


  val conf = let
    val super  = 0.3
    val park   = 0.5
    val home   = 1.2
    val corp   = 1.5 
    val school = 1.8
    val train  = 3.0
  in
    {betaNSuper = super  * Type.gamma
    ,betaNPark  = park   * Type.gamma
    ,betaNHome  = home   * Type.gamma
    ,betaNCorp  = corp   * Type.gamma
    ,betaNSch   = school * Type.gamma
    ,betaNTrain = train  * Type.gamma
    ,infectRule = 
      {tag = "EMP_15_JOJ_SNK"
      ,n    = 60
      ,rule = {role = ROL_SOME Employed, livein = LIV_SOME JOJ, workat = WOR_SOME [(SJK,Corp)]}
      }
    ,nPop       = 6000
    ,tag        = 
      String.concatWith "_" (
        map Real.toString [super,park,home,corp,school,train])
    ,mcid       = "0"
    }
  end

  val tasks = GenTask.dup' (14, conf:Trivial.conf): Trivial.conf list
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
