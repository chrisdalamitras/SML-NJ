fun transf nil = []
  | transf [a] = [a]
  | transf (a::b::cs) = a-b::transf cs;
    
fun altsum list = foldr (op +) 0 (transf list);
         


