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
val tasks = GenTask.gen2 {setO = [1.2, 1.5, 1.8], setTr = [1.8, 2.0, 3.0], nDup = 8}
val conf = List.nth(tasks, idxTask)
val tagbase = #tag conf
val tag = #tag conf ^ "@" ^ #mcid conf

(*
val tagbase =
  if me = 0 then
    let val tagbase = Misc.timestamp3 ()
        val others = List.tabulate (Int32.toInt nproc - 1, fn i => Int32.fromInt (i + 1))
    in (OS.FileSys.mkDir (outbase^"/"^tagbase) handle _ => () (* すでにあると出る例外を黙殺 *)
       ;map (fn i => 
           MPI.send M.string {buf=tagbase, dest=i, tag=100, comm=MPI.COMM_WORLD}
         ) others
       ;tagbase)
    end
  else 
    (#1 o MPI.recv U.string) {source = 0, tag = 100, comm = MPI.COMM_WORLD}
val tag = tagbase ^ "@" ^ Int32.toString me;

(* 感染率等設定 *)
val gamma = Type.gamma
val conf: Trivial.conf =
  {betaNHome  = 1.2 * gamma
  ,betaNSch   = 1.2 * gamma
  ,betaNSuper = 1.0 * gamma
  ,betaNCorp  = 1.2 * gamma
  ,betaNTrain = 1.5 * gamma
  ,betaNPark  = 0.5 * gamma
  ,e0_JOJ     = 30
  ,nPop       = 3000
  ,tag        = "sample2"
  ,mcid       = ""
  };
*)

(* 判断がむつかしいが、まちを構成するまでは共通の乱数列を使う *)
val _ = Trivial.inirnd 0;
val city = Trivial.city conf;

val tStop = 360*Type.days';

(* シミュレーション中はランク毎に違う乱数列を使う *)
val _ = Trivial.inirnd idxTask;
val _ = Trivial.run1 {conf=conf, recstep=180, tStop=tStop, tag=tag, dir=outbase^"/"^tagbase, city=city};

val _ = MPI.finalize();

val t1 = Time.now();
val () = print ("elapsed time: " ^ Time.toString (Time.-(t1,t0)) ^ "[sec]\n")

in
  OS.Process.success
end


end
