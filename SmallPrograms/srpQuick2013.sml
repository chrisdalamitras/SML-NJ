fun constructlst (lst,0,result) = result 
  | constructlst (lst,cnt,result) = constructlst (lst,cnt-1,lst@result);

fun srpH (lst,[],sublst,len,cnt) = sublst
  | srpH (lst,h::t,sublst,len,cnt) = 
            if (cnt <= len div 2) then
              if (len mod cnt = 0) then
                let 
                   val cl = constructlst (sublst,len div cnt,[])
                in
                   if lst = cl then sublst
                   else srpH (lst,t,sublst@[h],len,cnt+1)
                end
              else srpH (lst,t,sublst@[h],len,cnt+1) 
            else lst;  

fun srp lst = if null lst then [] 
              else srpH(lst, tl lst,[hd lst],length lst,1); 