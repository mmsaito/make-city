CM.make "unreal.cm";

structure M = MPI.Marshall;
structure U = MPI.Unmarshall;

val t0 = Time.now();

MPI.init();
val nproc = MPI.comm_size();
val me    = MPI.comm_rank();

(* �^�O�ƌ��ʕۑ��� *)
val outbase = "test"
val tagbase =
  if me = 0 then
    let val tagbase = Misc.timestamp3 ()
        val others = List.tabulate (Int32.toInt nproc - 1, fn i => Int32.fromInt (i + 1))
    in (OS.FileSys.mkDir (outbase^"/"^tagbase) handle _ => () (* ���łɂ���Əo���O��َE *)
       ;map (fn i => 
           MPI.send M.string {buf=tagbase, dest=i, tag=100, comm=MPI.COMM_WORLD}
         ) others
       ;tagbase)
    end
  else 
    (#1 o MPI.recv U.string) {source = 0, tag = 100, comm = MPI.COMM_WORLD}
val tag = tagbase ^ "@" ^ Int32.toString me;

(* ���������ݒ� *)
val gamma = Type.gamma
val conf: Trivial.conf =
  {betaNHome  = 1.2 * gamma
  ,betaNSch   = 1.5 * gamma
  ,betaNSuper = 1.2 * gamma
  ,betaNCorp  = 1.5 * gamma
  ,betaNTrain = 1.6 * gamma
  ,betaNPark  = 0.5 * gamma
  ,e0_JOJ     = 30
  ,nPop       = 3000
  };

(* ���f���ނ��������A�܂����\������܂ł͋��ʂ̗�������g�� *)
Trivial.inirnd 0;
val city = Trivial.city conf;

(* �V�~�����[�V�������̓����N���ɈႤ��������g�� *)
Trivial.inirnd (Int32.toInt me);
Trivial.run1 {conf=conf, recstep=180, tStop=1440*360, tag=tag, dir=outbase^"/"^tagbase, city=city};

MPI.finalize();

val t1 = Time.now();
val () = print ("elapsed time: " ^ Time.toString (Time.-(t1,t0)) ^ "[sec]\n");

OS.Process.exit OS.Process.success;
