fun lines W H Lmin Lmax =
  let
    open Math     
    fun ThirdLoop k i = if (k>W-i) then IntInf.fromInt 0
                        else IntInf.fromInt 2 + ThirdLoop (k+1) i; 

    fun SecondLoop i j = if (i>W) then IntInf.fromInt 0
                         else if ((i<>j) orelse (i=1 andalso j=1)) then 
                                if (sqrt(pow(real j,2.0)+pow(real i,2.0))>=real Lmin) then
                                  if (sqrt(pow(real j,2.0)+pow(real i,2.0))<=real Lmax) then 
                                    (ThirdLoop 0 i)*IntInf.fromInt(H+1-j) + SecondLoop (i+1) j
                                  else IntInf.fromInt 0  
                                else SecondLoop (i+1) j
                              else SecondLoop (i+1) j;  

    fun FirstLoop j = if (j>H) then IntInf.fromInt 0    
                      else SecondLoop 1 j + FirstLoop (j+1); 
     
     
     
    val HorVerResult = if (Lmin=1) then IntInf.fromInt (4+3*(W-1)+(3+2*(W-1))*(H-1))
                       else 0;  

    val DiagResult = FirstLoop 1; 

  in
    HorVerResult+DiagResult
  
  end;     