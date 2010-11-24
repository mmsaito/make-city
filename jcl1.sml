structure JCL1 = struct
(*
fun echoOff () = Control.Print.out := {flush = fn _ => (), say = fn _ => ()} ;
fun echoOn () = Control.Print.out := {flush = Control.Print.flush, say = Control.Print.say} ;
*)
structure M = MPI.Marshall;
structure U = MPI.Unmarshall;

fun main offset = let
val t0 = Time.now();
val _ = MPI.init();
val nproc = MPI.comm_size();
val me    = MPI.comm_rank();

(* 担当タスクの決定 *)
val outbase = "test"
val idxTask = Int32.toInt (me + offset)
(* val tasks = GenTask.gen2 {setO = [1.2, 1.5, 1.8], setTr = [1.8, 2.0, 3.0], nDup = 8} *)
val tasks = GenTask.gen2 {setO = [1.21], setTr = [1.81], nDup = 8}
val nTasks = length tasks

val _ = 
  if idxTask < nTasks then
    let
      val conf    = List.nth(tasks, idxTask)
      val tagbase = #tag conf
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

      val tStop = Type.days';

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
end


end
