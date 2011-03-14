CM.make "$mylib/import-all.cm";
val op *^ = Math.pow; infix 9 *^;
val rnd = Random.rand(0,1);
fun paretoSel {min, max, n, beta} = let
  val u  = Random.randReal 
  val cumN = 1.0 - (min/max)*^beta
  val k = beta/(beta - 1.0)
  val a = (1.0 - cumN/n)*^(1.0/k)
  val b = (1.0 - cumN)*^(1.0/k)
in
  fn rnd => Real.ceil (n/cumN*(1.0 - (a - (a-b)*u rnd)*^k)) - 1
end

fun paretoSel' {alpha, gamma, n, m, beta} = let
  val n  = n - 1.0 (* trick to get n + m - 1 as the maximal index *)
  val Q1 = 1.0 - (alpha/gamma)*^beta
  val P1 = alpha*beta/(beta - 1.0)*(1.0 - (alpha/gamma)*^(beta - 1.0))
  val P2 = P1 + gamma*Q1*m/n
  fun u rnd = P2*Random.randReal rnd
  fun clip x = Real.round x
in
  fn rnd => let
    val u = u rnd
    val z = 
      if u < P1 
      then 1.0 - (1.0 - (beta - 1.0)*u/alpha/beta)*^(beta/(beta-1.0))
      else Q1 + (u - P1)/gamma
  in
    clip (n*z/Q1)
  end
end

val n = 20000
val m = 222892

val box = Array.array(n, 0)
val idxSel = paretoSel' {alpha = 1.0, gamma = 10000.0, beta=2.0
  , n = Real.fromInt n - 20.0, m = 20.0}
val idx = List.tabulate(m, fn _ => idxSel rnd)
val () = app (fn idx => Array.update(box,idx,Array.sub(box,idx)+1)
 handle _ => print (Int.toString idx)
) idx

val size = Misc.qsortV (fn (a,b) => Int.compare (b,a)) (Array.vector box);

fun saveCorpSize f size = 
  (fn os => 
    (Vector.appi (fn (i,s) => TextIO.output(os, Int.toString (i+1) ^ "," ^ Int.toString s ^ "\n ")) size
    ; TextIO.closeOut os)) (TextIO.openOut f)

val _ = saveCorpSize "c.csv" size
