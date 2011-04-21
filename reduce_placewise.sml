structure ReducePlacewise = struct
  structure T = TextIO
  open Alice
  val $ = Vector.sub; infix 9 $;
  val file1 = "poppw_sample.csv"
  val file2 = "poppw_0.6_1.0_1.2_3.0_3.6_6.0_EMP_30_JOJ_SJK_571641_176866_138684_314861_44680.csv"
  type nVis = {s:int, e:int , i:int, r:int, t:int}
  val nVis0 = {s = 0, e = 0, i = 0, r = 0, t = 0}
  type place_t = {peak:nVis ref, seq:nVis list ref}
  fun readCsvLine os = Option.map (CSV.split {seps=",",spcs=" \n"}) (T.inputLine os)
    : string list option
  val idxCram  = 0
  val idxSch   = 1
  val idxCorp  = 2
  val idxHome  = 3
  val idxSuper = 4
  val idxPark  = 5
  val idxTrain = 6
  fun readH is = let
    val _ = T.inputLine is
    val nAreas = iS (hd (valOf (readCsvLine is)))
    val nKinds = 7
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
    (* tokenizeに時間がかかるので時間を間引く *)
    (*
    fun getLine () = 
      (T.inputLine is   (* 180 *)
      ;T.inputLine is   (* 360 *)
      ;T.inputLine is   (* 540 *)
      ;readCsvLine is) before  (* 720 = 正午 *)
      ignore (T.inputLine is   (* ... *)
      ;T.inputLine is
      ;T.inputLine is
      ;T.inputLine is
      )
      *)
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
      (* 記録のとり方にバグがあって、人数が負になる場合がある *)
      val getCol = (fn i => if i < 0 then 0 else i) o iS o openL col
      val t = getCol()
      val () = print (sR (rI t/1440.0) ^ "\n")
    in
      Vector.app (fn area =>
        Vector.appi (fn (kind,places) =>
          Vector.app (fn at => 
            let 
              val point = {t = t, s = getCol(), e = getCol(), i = getCol(), r = getCol ()}
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

  val sortPeakTime = Misc.qsortV (fn (x:place_t,y:place_t) => Int.compare((#t o ! o #peak) x, (#t o ! o #peak) y))
  val sortPeakI    = Misc.qsortV (fn (x:place_t,y:place_t) => Int.compare((#i o ! o #peak) x, (#i o ! o #peak) y))

  fun writeSeqSEIRV sel os v = 
    Vector.app (fn {seq,peak} => 
      (List.app (fn (nVis:nVis) => T.output (os, sI (sel nVis) ^ ",")) (!seq)
      ;T.output(os,"\n"))
    ) v
  val writeSeqIV = writeSeqSEIRV #i
  fun writeSeqSEIRVF sel v f =
    (fn os => (writeSeqSEIRV sel os v; TextIO.closeOut os)) (TextIO.openOut f)

  fun writeSeqReproNumV os (v: place_t vector) kind  = 
    Vector.app (fn {seq,peak} => 
      (List.app (fn ({s,r,e,i,...}: nVis) => T.output (os, sR (rI s/ rI(s+e+i+r)) ^ ",")) (!seq)
      ;T.output(os,"\n"))
    ) v
end 
