fun grafo file =
   let
    
     val input = TextIO.openIn file
     val n = Option.valOf (TextIO.scanStream (Int.scan StringCvt.DEC) input)
     val garbage = TextIO.inputLine input

     fun charList (inputFile,n,i) =
       if (i>n)  then []
       else Option.valOf (TextIO.scanStream (Int.scan StringCvt.DEC) inputFile) :: charList (inputFile,n,i+1);
    
     fun halve nil = (nil,nil)
             |   halve [a] = ([a],nil) 
             |   halve (a::b::rest) =
                   let
                     
                     val (x,y) = halve rest

                   in

                     (a::x,b::y)

                   end;

     fun MergeSort1 nil = nil 
     |   MergeSort1 [e] = [e]    
     |   MergeSort1 elementList =
           let
     
             fun merge (nil,ys) = ys              
             |   merge (xs,nil) = xs
             |   merge (x::xs,y::ys) = 
                  if (x<y) then x::merge(xs,y::ys)
                  else y::merge(x::xs,ys);
                     
             val (x,y) = halve elementList 

           in

             merge(MergeSort1 x,MergeSort1 y)

           end;

    fun MergeSort2 nil = nil 
    |   MergeSort2 [e] = [e]    
    |   MergeSort2 elementList =
          let
     
            fun merge (nil,ys) = ys              
            |   merge (xs,nil) = xs
            |   merge (x::xs,y::ys) = 
                 if (x>y) then x::merge(xs,y::ys)
                 else y::merge(x::xs,ys);
                     
            val (x,y) = halve elementList 

          in

            merge(MergeSort2 x,MergeSort2 y)

           end;

     fun pathLimit (first::[]) = first     
     |   pathLimit (first::rest) = if (first= hd rest) then first
                                   else pathLimit rest;

     fun finalList (pFN,pSN) = if (pFN=pSN) then [pFN]
                               else pFN::finalList (pFN+1,pSN);


     val list = charList(input,2*(n-1),1) 
     val twoList = halve list
     val list1 = #1 twoList
     val list2 = #2 twoList
     val sortList1 = MergeSort1 list1      
     val sortList2 = MergeSort2 list2
     val pathFirstNode = pathLimit sortList2
     val pathSecondNode = pathLimit sortList1      

  in
     
   finalList (pathFirstNode,pathSecondNode)   

  end; 