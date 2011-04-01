(* 会社をナイーブにピーク時刻でソートして、書き出す 
 *  2次元配列なので、ことは容易ではない。さしあたり、八王子をやってみよう。
 * *)
structure T = TextIO;
structure PW = ReducePlacewise;
infix 9 $; open Type;
infix 1 `; fun op ` (f,x) = f x;
open Alice;

val org = "C:/Users/saitohm/Documents/Simoto/code-unreal/";
val dir = "C:/Users/saitohm/Documents/Simoto/code-unreal-2/test/scale2000/unsafe0.5ei_pout0.9_repro2.0_EMP_and_SCH/";

val file = "poppw_0.6_1.0_1.2_3.0_3.6_6.0_EMP_30_SCH_30_JOJ_SJK_571641_176866_138684_314861_44680.csv";

val () = OS.FileSys.chDir dir;

fun reload() = use (org ^ "jcl_reduce_1.sml");

(*
val h = PW.openF file;
val b = PW.readB h
*)


fun writeSeqMaxR f (v: PW.place_t vector) = 
  ( fn os => 
    (Vector.app ((fn x => T.output (os, sR x ^ "\n")) o foldl (fn (a,b) => Real.max(b,ar a)) 0.0 o ! o #seq) v
    ;T.closeOut os)
  ) (T.openOut f);

fun writeL show f v = 
  (fn os => (app (fn x => T.output (os, show x ^ "\n")) v;T.closeOut os)) (T.openOut f);
fun writeV show f v = 
  (fn os => (Vector.app (fn x => T.output (os, show x ^ "\n")) v;T.closeOut os)) (T.openOut f);

fun rRate {s,e,i,r,t} = rI r / rI (s + e + i + r);
fun nPerson {s,e,i,r,t} = s + e + i + r;

fun estN (p:PW.place_t) = foldl Int.max 0 (map nPerson (!(#seq p)));
fun maxRRate (p:PW.place_t) = foldl Real.max 0.0 (map rRate (!(#seq p)));

fun writePlacesMaxRRate f (v: PW.place_t vector) = let
  val data = Vector.map (fn p => (estN p, maxRRate p)) v
in
  writeV (fn (x,y) => sI x^","^sR y) f data
end



(* データを取ったときの記憶に従って...。本来ヘッダに書いておくべき *)
(*
val reproNum = let open Tasks3 in mult reproNum5 2.0 end
fun reproNum2V rr = let
  val sel = Array.array(6,0.0)
in
  (Array.update(sel, PW.idxCram, #school rr)
  ;Array.update(sel, PW.idxSch, #school rr)
  ;Array.update(sel, PW.idxCorp, #corp rr)
  ;Array.update(sel, PW.idxHome, #home rr)
  ;Array.update(sel, PW.idxSuper, #super rr)
  ;Array.update(sel, PW.idxPark, #park rr)
  ;Array.vector sel
  )
end
and gomi () = reproNum2V reproNum

val reproNumV = reproNum2V reproNum
*)


fun sumPlace sel (a:PW.place_t, b:int list) = 
  ListPair.map (fn (x,y) => sel x + y) (!(#seq a), b);

  
fun fold1V f v = let
  val n = Vector.length v
  fun g (i,s) = if (i < n) then g(i+1, f(Vector.sub(v,i),s)) else s
in
  g(1,Vector.sub(v,0))
end;


fun reduceForID sel (city: PW.place_t vector vector vector) = 
  Vector.map (fn area =>
    Vector.map (fn places => 
      if Vector.length places > 0 
        then Vector.foldl (sumPlace sel) (List.tabulate(length(!(#seq (places $ 0))), fn _ => 0)) places 
        else nil
    ) area) city
   

(* S-R比 -------------------------------------------- *)
(*
val corpSNJ = PW.sortPeakI (b $ 3 $ PW.idxCorp)
val _ = (fn os => (PW.writeSeqReproNumV os corpSNJ PW.idxCorp; T.closeOut os)) (T.openOut "snj-corp-repronum.csv");
val _ = (fn os => (PW.writeSeqSEIRV #i os (Unsafe.cast corpSNJ); T.closeOut os)) (T.openOut "snj-corp-i.csv");
val _ = (fn os => (PW.writeSeqSEIRV #e os (Unsafe.cast corpSNJ); T.closeOut os)) (T.openOut "snj-corp-e.csv");

val schSNJ = PW.sortPeakI (b $ 3 $ PW.idxSch);
val _ = (fn os => (PW.writeSeqReproNumV os schSNJ PW.idxSch; T.closeOut os)) (T.openOut "snj-sch-repronum.csv");
val _ = (fn os => (PW.writeSeqSEIRV #i os (Unsafe.cast schSNJ); T.closeOut os)) (T.openOut "snj-sch-i.csv");
val _ = (fn os => (PW.writeSeqSEIRV #e os (Unsafe.cast schSNJ); T.closeOut os)) (T.openOut "snj-sch-e.csv");

val superSNJ = PW.sortPeakI (b $ 3 $ PW.idxSuper);
val _ = (fn os => (PW.writeSeqReproNumV os superSNJ PW.idxSuper; T.closeOut os)) (T.openOut "snj-super-repronum.csv");
val _ = (fn os => (PW.writeSeqSEIRV #i os (Unsafe.cast superSNJ); T.closeOut os)) (T.openOut "snj-super-i.csv");
val _ = (fn os => (PW.writeSeqSEIRV #e os (Unsafe.cast superSNJ); T.closeOut os)) (T.openOut "snj-super-e.csv");
*)

(*
val corpHAC = sortPeakI (b $ 0 $ PW.idxCorp)
val _ = (fn os => (writeSeqV os corpHAC; T.closeOut os)) (T.openOut "hac-peak-corp-i.csv");

val schHAC = sortPeakI (b $ 0 $ PW.idxSch);
val _ = (fn os => (writeSeqV os schHAC; T.closeOut os)) (T.openOut "hac-peak-sch-i.csv");

val superHAC = sortPeakI (b $ 0 $ PW.idxSuper);
val _ = (fn os => (writeSeqV os superHAC; T.closeOut os)) (T.openOut "hac-peak-super-i.csv");
*)



(*
local
  open PW
  in
fun setPeak (city: place_t vector vector vector) = 
      Vector.app (fn area =>
        Vector.appi (fn (k,places) =>
          if k <> idxHome then
            Vector.app (fn (at:place_t) => 
              #peak at := 
                {i = #i (Misc.maxGL (fn (x,y) => Int.compare (#i x, #i y)) (!(#seq at)))
                ,t = ~ 1}
            ) places 
          else
            ()
        ) area
      ) city
      end
*)
