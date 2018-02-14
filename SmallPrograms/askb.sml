fun counters (nil,cnt,result) = result
  | counters ([a],cnt,result) =  rev (cnt::result)
  | counters (a::b::cs,cnt,result) = if (a-b=1 orelse a-b= ~ 1) then 
                                          counters (b::cs,cnt+1,result)
                                     else counters (b::cs,1,cnt::result);


fun incseqs (nil,h::t,subLst,result) = rev (subLst::result)
  | incseqs (a::cs,h::t,subLst,result) = if h <> 0 then
                                             incseqs (cs,(h-1)::t,subLst@[a],result)
                                         else incseqs (a::cs,t,[],subLst::result);

fun inc1seqs lst = incseqs (lst, counters (lst,1,[]), [], []);
