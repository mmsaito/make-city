(*
fun echoOff () = Control.Print.out := {flush = fn _ => (), say = fn _ => ()} ;
fun echoOn () = Control.Print.out := {flush = Control.Print.flush, say = Control.Print.say} ;
*)
CM.make "unreal.cm";
  fun echoOff () = Control.Print.out := {flush = fn _ => (), say = fn _ => ()} ;
  fun echoOn () = Control.Print.out := {flush = Control.Print.flush, say = Control.Print.say} ;

val offset = 
  case SMLofNJ.getArgs()
    of nil => (print "WARNING: argument is empty. regarding task id offect is 0.\n"
              ; 0)
     | offset::xs => valOf (Int32.fromString offset);
(* fn () => *)


JCL1.main offset;

OS.Process.exit OS.Process.success;
