fun parse file =
    let
	(* a function to read an integer from an input stream *)
        fun next_int input =
	    Option.valOf (TextIO.scanStream (Int.scan StringCvt.DEC) input)
	(* a function to read a real that spans till the end of line *)
        fun next_real input =
	    Option.valOf (TextIO.inputLine input)
	(* open input file and read the two integers in the first line *)
        val stream = TextIO.openIn file
        val n = next_int stream
        val l = next_int stream
	val _ = TextIO.inputLine stream
	(* a function to read the pair of integer & real in subsequent lines *)
        fun scanner 0 acc = acc
          | scanner i acc =
            let
                val d = next_int stream
                val (SOME v) = Real.fromString (next_real stream)
            in
                scanner (i - 1) ((d, v) :: acc)
            end
	
    val revscanner = rev(scanner n [])
	
	fun insertion_sort cmp a =
  Array.appi (fn (i, v) =>
    let
  val j = ref (i - 1)
in
  while !j >= 0 andalso cmp (Array.sub (a, !j), v) = GREATER do (
    Array.update (a, !j + 1, Array.sub (a, !j));
    j := !j - 1
  );
  Array.update (a, !j + 1, v)
end
)
    a;
	
fun atl (a,i) = Array.sub (a,i);

fun tel (a,~1) = []
  | tel (a,n) = atl(a,n)::tel(a,n-1);
  
fun artolist a = rev(tel(a,(Array.length a)-1));

fun gramf 0 = []
  | gramf n = n::gramf(n-1);
  
fun gram n = rev(gramf n);  

fun find (n:real,a,i) = 
   let 
     val k = i+1;
	 val m = Array.sub (a,i)
   in
     if (n>m orelse n<m) then find (n,a,k)
	 else i
end

fun findall (arr1, i, arr2) = 
   let 
     val j = i+1;
     val m = Array.sub (arr1,i)
   in 
     if (i=n-2) then [find (m, arr2, 0)]
     else (find(m, arr2, 0))::(findall(arr1,j,arr2))
end

	fun dist [] = [] 
	  | dist [(a,b)] = [a]
	  | dist ((a,b)::t) = a::dist t;
	  
	fun vel [] = []
	  | vel [(a,b)] = [b]
	  | vel ((a,b)::t) = b::vel t;
	    
	val DA = Array.fromList (dist revscanner);
	val VA = Array.fromList (vel revscanner);
	val XA = Array.array (n,5000.0);
	
	fun calct v1 v2 d curt = if (v1-v2>0.0 orelse v2-v1>0.0) then d/(v2-v1) else curt;
	
	fun calcX (i,j) = 
	    let
		  val reallength = real l
		  val curt = Array.sub (XA,i)
		  val k = j + 1
		  val m = i + 1
		  val v1 = Array.sub (VA,i)
		  val v2 = Array.sub (VA,j)
		  val d1 = Array.sub (DA,i)
		  val d2 = Array.sub (DA,j)
		  val r1 = real d1
		  val r2 = real d2
		  val apost = if r2>r1 then reallength-r2+r1 else r1-r2
		  val newt = calct v1 v2 apost curt 
		  val curt2 = if (curt>0.0) then curt else newt+1.0
		  val gg = if (v2>v1 andalso newt < curt2) then Array.update (XA,i,newt) else Array.update (XA,i,curt)
		  val DA = XA
	    in 
		    if k < n then
		      calcX(i,k) 
			else if m < n then
			  calcX(m,0)
		    else
			  DA
	    end;
	
	fun add1 lst =
         if null lst then nil
         else hd lst + 1 :: add1(tl lst);
	
	fun teliki a = if (insertion_sort Real.compare a = ()) then a else (Array.fromList[]);
	
	val CA = artolist(calcX(0,0));
	
val MA = Array.fromList CA;
	
    in
        (add1(findall (teliki MA,0,XA)))
    end


fun agonas fileName = parse fileName