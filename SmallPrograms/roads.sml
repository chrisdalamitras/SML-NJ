fun parse file =
    let
	(* a function to read an integer from an input stream *)

        fun next_int input =
	    Option.valOf (TextIO.scanStream (Int.scan StringCvt.DEC) input)

	(* open input file and read the three integers in the first line *)

        val stream = TextIO.openIn file
        val N = next_int stream
        val L = next_int stream
        val X = next_int stream
	val _ = TextIO.inputLine stream
        val BeginEnd = Array.array (2,0)
	
        (* a function to read the pair of integer & integer in subsequent lines *)
        fun scanner 0 acc = acc
          | scanner i acc =
            let
                val d = next_int stream
                val v = next_int stream
            in
                scanner (i - 1) ((d, v) :: acc)
            end
    in
        Array.update (BeginEnd,1,L);
        (X,  rev(scanner N []), Array.array (L,0), BeginEnd)
    end

fun FixRoad  (S,E,P) = if (S < E) then
                         if (Array.sub (P,S) = 0) then
                           let
                             val pin = Array.update (P,S,E)
                           in
                             FixRoad (S+1,E,P)
                           end                        
                         else FixRoad (Array.sub (P,S),E,P) 
                       else P;

fun FindMax (i,start,acc,max,P,BeginEnd) = if (i < Array.length P) then
                                             if (Array.sub (P,i) = 0) then FindMax (i+1,start,acc+1,max,P,BeginEnd)
                                             else 
                                               if (acc > max) then
                                                 let
                                                   val b = Array.update (BeginEnd,0,start)
                                                   val e = Array.update (BeginEnd,1,i)
                                                 in 
                                                   FindMax (Array.sub (P,i),Array.sub (P,i),0,acc,P,BeginEnd)
                                                 end
                                               else FindMax (Array.sub (P,i),Array.sub (P,i),0,max,P,BeginEnd)                               
                                           else 
                                             if (acc > max) then
                                               let
                                                  val b = Array.update (BeginEnd,0,start)
                                                  val e = Array.update (BeginEnd,1,i)
                                                in 
                                                  acc
                                                end
                                             else max;

fun ConditionalMax (S,E,BeginEnd,P) = 
    let
      val b = Array.sub (BeginEnd,0)
      val e = Array.sub (BeginEnd,1)
    in
      if ((S<=b andalso E>b) orelse (S>=b andalso E<=e) orelse (S<e andalso E>=e )) then FindMax (0,0,0,0,P,BeginEnd)
      else ~1
    end;

fun compare (X,~1) = false
  | compare (X,max) = if (max <= X) then true
                      else false;

fun mysolution ((X,lista,P,BeginEnd),meres,true) = meres
  | mysolution ((X,nil,P,BeginEnd),meres,condition) = ~1
  | mysolution ((X,(S,E)::t,P,BeginEnd),meres,condition) =  
          let 
            val pin = FixRoad (S,E,P) 
            val Max = ConditionalMax (S,E,BeginEnd,pin)
            val con = compare (X,Max) 
          in
            mysolution ((X,t,pin,BeginEnd),meres+1,con)
          end;

fun dromoi filename = mysolution (parse filename,0,false)









