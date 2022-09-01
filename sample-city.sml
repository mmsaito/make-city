(* sample-city.sml
 * --------------
 * 都市生成スクリプトのサンプル
 *)

(* 都市生成モジュールの取り込み *)
val home = 
((fn {arcs,isAbs,...} => OS.Path.toString {arcs=arcs,isAbs=isAbs,vol=""}) 
  o OS.Path.fromString o valOf) 
    (OS.Process.getEnv "HOME");
  
val unreal = "./";
CM.make (unreal ^ "unreal.cm");
(* ここまで *)

local 
  open Type;
  open Alice;
  infix 9 $;

  (* outbase: 生成ファイルの出力先のトップディレクトリ *)
  val () = JCL1.outbase := "."
in
  (* ワクチン奏功率 *)
  fun vacResponse age =
    if      0.0 <= age andalso age < 5.0  then 0.6
    else if 5.0 <= age andalso age < 65.0 then 0.8
    else                                       0.5
  (* ワクチンによる減感作率 ------- これはダミー *)
  fun vacHyposensitize age = 
    if      0.0 <= age andalso age < 5.0  then 0.8
    else if 5.0 <= age andalso age < 65.0 then 0.4
    else                                       0.8

  (* 都市のスケーリング *)
  val mapV = Vector.map
  val scaleCoef = 10.0
  fun scale x = Int.max(1, iR (rI x*scaleCoef))
  fun scale' x = Int.max (30, scale x)

  (* 人口 *)
  val nPop = mapV scale #[571641, 176866, 138684, 314861, 44680]
  val nAll = Vector.foldl (op +) 0 nPop

  val wPop = Vector.map (fn n => rI n / rI nAll) nPop

  val k = 2.0
  val reproNum7 = 
    {super  = 0.3*k, park  = 0.5*k, home = 1.2*k, corp = 1.5*k
    ,school = 1.8*k, train = 1.0*k, hosp = 1.2*k}

  (* 公共の場所の数 *)
  val nPlaces6 = 
    #[{sch = 70, corp =  100, cram = 40, super = 100, park = 2, hosp = 36+36}
     ,{sch = 20, corp =  100, cram = 25, super = 100, park = 2, hosp =  9+ 7}
     ,{sch = 12, corp =  100, cram =  6, super = 100, park = 2, hosp = 11+10}
     ,{sch = 29, corp = 2000, cram = 40, super = 100, park = 2, hosp =  9+16}
     ,{sch =  8, corp = 2000, cram =  4, super = 100, park = 2, hosp =  2+16}
     ]
  fun mapPS f {sch, corp, cram, super, park, hosp} =
    {sch   = f sch, corp  = f corp, cram  = f cram, super = f super, park  = f park, hosp  = f hosp}
 
  val hospPop = {doc = 10, inpat = 30, outpat = 70}

  (* 初期感染者の選択
   * ----------------
   *   設定を同じくするバリエーションをつくるのためのハックをしている。
   *   都市住民をリストとして持っているため乱択できない。これは、はっきりいって
   *   設計ミス。コード変更はやりたくないので、ここでは目的の感染者数×パターン数を
   *   初期感染者にとって、フィルタリングすることで、実現する。
   *
   *   後述の ※ を参照 *)
  val nDup = 1 
  val nInf0 = scale 300
  val infEMP = 
       {kind = OPT_INTERV_INF
       ,n    = nInf0*nDup (* 32通り *)
       ,rule = {role   = ROL_SOME Employed
               ,livein = LIV_SOME JOJ
               ,workat = WOR_SOME [(SJK,Corp)]
               }
       ,time = 0
       ,isRandom = false
       ,tag  = "EMP30J2S"
       } 
  val infSCH = 
        {kind = OPT_INTERV_INF
        ,n    = nInf0*nDup
        ,rule = {role   = ROL_SOME Student
                ,livein = LIV_SOME JOJ
                ,workat = WOR_ARBIT 
                }
        ,time = 0
        ,isRandom = false
        ,tag = "SCH30J2S"
        }
  local
    val {super, park, home, corp, school, train, hosp} = reproNum7
  in
  fun genConf {vacRule, vacResponse, vacHyposensitize}
  = {betaNSuper = super  * Type.gamma
    ,betaNPark  = park   * Type.gamma
    ,betaNHome  = home   * Type.gamma
    ,betaNCorp  = corp   * Type.gamma
    ,betaNSch   = school * Type.gamma
    ,betaNTrain = train  * Type.gamma
    ,betaNHosp  = hosp   * Type.gamma
    ,infectRule = ()
    ,intervRule = infEMP :: infSCH :: vacRule
    ,vacResponse = vacResponse
    ,vacHyposensitize = vacHyposensitize
    ,vacEff     = 0.0
    ,vacTrCover = 0.0
    ,vacSchCover= 0.0
    ,hospPop    = {doc    = scale (#doc hospPop)
                  ,inpat  = scale (#inpat hospPop)
                  ,outpat = scale (#outpat hospPop)}
    ,nPop       = nPop
    ,nPlaces    = mapV (mapPS scale) nPlaces6

    (* 設定につけるタグ。都市ファイルの出力先ディレクトリ名となる。*)
    ,tag        = "scale" ^ sR scaleCoef
(*
      String.concatWith "_" 
        ( map Real.toString [super,park,home,corp,school,train]
        @ Misc.listV (Vector.map Alice.sI (nPop))
        )
    *)
    ,mcid       = "0"
    }: Trivial.conf
  end

  val conf0 = genConf {vacRule = nil, vacResponse=NONE, vacHyposensitize=NONE};
  val () = print ">>>";
  val city = Trivial.city conf0;
  val () = print "<<<";
  val done = Probe.writeCity city ("./" ^ #tag conf0 ^ "/model.city");

  fun nRolePop cond = Vector.map (length o List.filter cond o Type.popArea) (#area city);
  val nStu = nRolePop (fn PERSON {role=Student,...} => true | _ => false);
  val nEmp = nRolePop (fn PERSON {role=Employed,...} => true | _ => false);
  fun hasMultipleAreas (vs: Type.place_t list) =
    case vs 
      of {area_t=a1,...}::vs =>
           List.exists (fn {area_t=a2,...} => a1 <> a2) vs
       | _ => false
  val nTrUser = nRolePop (fn PERSON {belong,...} => hasMultipleAreas belong)
  val nTrStu  = nRolePop (fn PERSON {role=Student, belong,...} => hasMultipleAreas belong | _ => false)
  val nTrEmp  = nRolePop (fn PERSON {role=Employed, belong,...} => hasMultipleAreas belong | _ => false)
  val nullVac = {vacResponse = fn _ => 0.0, vacHyposensitize = fn _ => 0.0}

  (* xs を n要素ずつに分割 *)
  fun splitL (xs, n) = 
    if length xs > n
      then List.take (xs, n) :: splitL (List.drop(xs,n), n)
      else [xs]

  (* ※ *)
  val itvEmpSet = splitL (Frame.ruleInterv nullVac infEMP city, nInf0)
  val itvStuSet = splitL (Frame.ruleInterv nullVac infSCH city, nInf0)

  val itvL = let
    val i = ref 0
    fun idx() = Int.toString (!i) before i := !i + 1
  in
    ListPair.app 
     (fn (emp,stu) => JCL1.writeInterv conf0  (emp@stu) ("uncontrolled-" ^ idx()))
       (itvEmpSet, itvStuSet)
  end  

  val gomi = OS.Process.exit (Unsafe.cast 0) ;
end
