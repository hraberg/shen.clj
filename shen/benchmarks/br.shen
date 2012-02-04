(defcc <br> 
    40 <br> 41 <br$> := ok;
    <item> <br> := ok;
    <e> := ok;) 

(defcc <br$> 
    <br> := ok;) 

(defcc <item> 
    -*- := (if (element? -*- [40 41]) 
            (fail) 
            ok);) 

