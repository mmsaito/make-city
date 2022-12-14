structure ReducePlacewise = struct
  structure T = TextIO
  open Alice
  val $ = Vector.sub; infix 9 $;
  val file1 = "poppw_sample.csv"
  val file2 = "poppw_0.6_1.0_1.2_3.0_3.6_6.0_EMP_30_JOJ_SJK_571641_176866_138684_314861_44680.csv"
  type nVis = {s:int, e:int , i:int, r:int, d:int, t:int}
  val nVis0 = {s = 0, e = 0, i = 0, r = 0, t = 0, d = 0}
  type place_t = {peak:nVis ref, seq:nVis list ref}
  fun readCsvLine os = Option.map (CSV.split {seps=",",spcs=" \n"}) (T.inputLine os)
    : string list option
  val idxCram  = 0
  val idxSch   = 1
  val idxCorp  = 2
  val idxHome  = 3
  val idxSuper = 4
  val idxPark  = 5
  val idxHosp  = 6
  val idxTrain = 7
  fun readH is = let
    val _ = T.inputLine is
    val nAreas = iS (hd (valOf (readCsvLine is)))
    val nKinds = 8
    fun readArea i = let
      val sizes = Array.array (nKinds, 0)
      fun loop () =
        case valOf (readCsvLine is)
          of n :: "nCram" :: _ => (Array.update(sizes, idxCram , iS n);loop())
           | n :: "nSch"  :: _ => (Array.update(sizes, idxSch  , iS n);loop())
           | n :: "nCorp" :: _ => (Array.update(sizes, idxCorp , iS n);loop())
           | n :: "nSuper":: _ => (Array.update(sizes, idxSuper, iS n);loop())
           | n :: "nPark" :: _ => (Array.update(sizes, idxPark , iS n);loop())
           | n :: "nHome" :: _ => (Array.update(sizes, idxHome , iS n);loop())
           | n :: "nHosp" :: _ => (Array.update(sizes, idxHosp , iS n);loop())
           | n :: "nTrain" :: _ => (Array.update(sizes, idxTrain , iS n);loop())
           | "--" :: _     => ()
           | _             => loop()
    in
      (loop(); Array.vector sizes)
    end
    val sizeTable = Vector.tabulate(nAreas, readArea)
    val s = ref 0
    val offsetTable =
      Vector.map (fn nVec =>
        Vector.map (fn i => !s before s := i + !s) nVec) sizeTable
      
  in
    { sizeTable = sizeTable, offsetTable = offsetTable
    , size   = fn {place_k, area_t} => sizeTable $ area_t $ place_k
    , offset = fn {place_k, area_t, id} => offsetTable $ area_t $ place_k + id + 1
    , is = is
    }
  end
  val openF = readH o T.openIn

  fun readB {is, size, offset, sizeTable, offsetTable} = let
    (* tokenize?????????????????????????????? *)
    fun getLine () = 
      (T.inputLine is   (* 180 *)
      ;T.inputLine is   (* 360 *)
      ;T.inputLine is   (* 540 *)
      ;readCsvLine is) before  (* 720 = ???? *)
      ignore (T.inputLine is   (* ... *)
      ;T.inputLine is
      ;T.inputLine is
      ;T.inputLine is
      )

    fun getLine () = 
      (T.inputLine is   (* 180 / 720 + 180*)
      ;T.inputLine is   (* 360 *)
      ;T.inputLine is   (* 540 *)
      ;readCsvLine is)  (* 720 = ???? / 1440 = ?^???? *)

    fun getLine () = readCsvLine is
    fun openL xs = 
      let val s = ref xs in fn () => hd (!s) before s := tl (!s) 
        handle s => (print "some exn\n"; raise s)
      end
    type place_t = {i: int list ref, t_peak:int}
    val city = 
      Vector.map (fn kinds =>
        Vector.map (fn size =>
          Vector.tabulate (size, fn _ => {seq = ref nil, peak = ref nVis0})
        ) kinds
      ) sizeTable
    fun cmpI (p:nVis, q:nVis) = Int.compare (#i p, #i q)
    fun parse col = let
      (* ?L?^???????????o?O?????????A?l?????????????????????? *)
      val getCol = (fn i => if i < 0 then 0 else i) o iS o openL col
      val t = getCol()
      val () = print (sR (rI t/1440.0) ^ "\n")
    in
      Vector.app (fn area =>
        Vector.appi (fn (kind,places) =>
          Vector.app (fn at => 
            let 
              val point = {t = t
                           ,s = getCol()
                           ,e = getCol()
                           ,i = getCol()
                           ,r = getCol()
                           ,d = getCol()}
            in
              if kind <> idxHome
                then (#seq  at := point :: !(#seq at)
                     ;#peak at := Misc.maxG cmpI (point, !(#peak at))) 
                else ()
            end
          ) places 
        ) area
      ) city
    end
  in
    (Iterator.appReader (fn col => parse col) getLine 
    ;city)
  end

  fun nTimePoints (b: place_t) = length (!(#seq b))

  (* val sortPeakTime = Misc.qsortV (fn (x:place_t,y:place_t) => Int.compare((#t o ! o #peak) x, (#t o ! o #peak) y)) *)
  val sortPeakI    = Misc.qsortV (fn (x:place_t,y:place_t) => Int.compare((#i o ! o #peak) x, (#i o ! o #peak) y))

  fun illRate ({s,e,i,r,d,t}:nVis) = (rI (r + d))/rI (s + e + i + r + d);
  fun nPerson ({s,e,i,r,d,t}:nVis)    = s + e + i + r;

  fun estN (p:place_t) = foldl Int.max 0 (map nPerson (!(#seq p)));
  fun finalIllRate (p:place_t) = foldl Real.max 0.0 (map illRate (!(#seq p)));

  fun sumPlace (sel:nVis -> int) (a:place_t, b:int list) = 
    ListPair.map (fn (x,y) => sel x + y) (!(#seq a), b);

  fun sumPlace2 (a:place_t, b:place_t): place_t = let
    fun sum (a:nVis,b:nVis) =
      {s = #s a + #s b 
      ,e = #e a + #e b
      ,i = #i a + #i b
      ,r = #r a + #r b
      ,d = #d a + #d b
      ,t = #t a
      }
    val seq_a = (! o #seq) a
    val seq_b = (! o #seq) b
  in
    {peak = #peak a
    ,seq  = ref (ListPair.map sum (seq_a,seq_b))
    }
  end

  fun sumPlaces (ps:place_t vector): place_t = 
    case ps
      of #[] => {peak = ref {s=0,e=0,i=0,r=0,d=0,t=0}
                ,seq = ref []} : place_t
       | _   => Misc.foldl1V sumPlace2 ps

  fun sumKindwise (city: place_t vector vector vector) = 
    Vector.map (Vector.map sumPlaces) city 

  fun writeL show f v = 
    (fn os => (app (fn x => T.output (os, show x ^ "\n")) v;T.closeOut os)) (T.openOut f);
  fun writeV show f v = 
    (fn os => (Vector.app (fn x => T.output (os, show x ^ "\n")) v;T.closeOut os)) (T.openOut f);

  fun writeSeqSEIRV sel os v = 
    Vector.app (fn {seq,peak} => 
      (List.app (fn (nVis:nVis) => T.output (os, sI (sel nVis) ^ ",")) (!seq)
      ;T.output(os,"\n"))
    ) v
  val writeSeqIV = writeSeqSEIRV #i
  fun writeSeqSEIRVF sel v f =
    (fn os => (writeSeqSEIRV sel os v; TextIO.closeOut os)) (TextIO.openOut f)

  fun writeIllRate f (v: place_t vector) = let
    val data = Vector.map (fn p => (estN p, finalIllRate p)) v
  in
    writeV (fn (x,y) => sI x^","^sR y) f data
  end

(*
  fun writeSeqReproNumV os (v: place_t vector) kind  = 
    Vector.app (fn {seq,peak} => 
      (List.app (fn ({s,r,e,i,...}: nVis) => T.output (os, sR (rI s/ rI(s+e+i+r)) ^ ",")) (!seq)
      ;T.output(os,"\n"))
    ) v
    *)
end 
