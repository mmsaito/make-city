CM.make "unreal.cm";
val offset = 
  case SMLofNJ.getArgs()
    of nil => (print "empty args error: usage sml <this-file> offset\n"
              ;raise Empty)
     | offset::xs => valOf (Int32.fromString offset);
(* fn () => *)
JCL1.main offset;
