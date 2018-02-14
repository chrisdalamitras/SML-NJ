fun sublst (h::t,0,result) = rev result 
  | sublst (h::t,cnt,result) = sublst (t,cnt-1,h::result);

fun constructlst (lst,0,result) = result 
  | constructlst (lst,cnt,result) = constructlst (lst,cnt-1,lst@result);

fun srpH (lst,0,cnt) = []
  | srpH (lst,len,cnt) = 
            if (cnt <= len div 2) then
              if (len mod cnt = 0) then
                let 
                   val sl = sublst (lst,cnt,[])
                   val cl = constructlst (sl,len div cnt,[])
                in
                   if lst = cl then sl
                   else srpH (lst,len,cnt+1)
                end
              else srpH (lst,len,cnt+1) 
            else lst;  

fun srp lst = srpH(lst,length lst,1);                