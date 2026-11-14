 exception Start;
 fun up1 (x, y) = (x, y+1);
 fun down1 (x, y) = (x, y-1);
 fun left1 (x, y) = (x-1, y);
 fun right1 (x, y) = (x+1, y);
local
    fun pred (x, y) =
      let val absx = abs(x) and absy = abs(y) 
      in 
          if  (x,y) = (0,0) then raise Start
          else if  (x <= 0 andalso y >= x+1 andalso y <= ~x+1) then down1 (x, y)
          else if  (y > 0  andalso x >= 2-y andalso x <= y) then left1 (x, y) 
          else if  (x > 0 andalso y >= ~x andalso y <= x-1) then up1 (x,y)
          else (* y < 0 andalso x >= y andalso x <= ~y-1 *) right1 (x,y)
      end
      fun cspiral1_tr ((x, y), p) =
        if (x, y) = (0,0) then p
        else cspiral1_tr (pred(x,y), p+1)
in fun cspiral1_iter4 (x, y) = cspiral1_tr ((x, y), 0)
end

local
    fun succ (x,y) =
      if  (x <= 0 andalso y >= x andalso y <= ~x) then up1 (x,y)
      else if (y > 0  andalso x >= ~y+1 andalso x <= y-1) then right1 (x,y)
      else if (x > 0 andalso y >= ~x+1 andalso y <= x) then down1 (x,y)
      else (* y < 0 andalso x >= y+1 andalso x <= ~y*) left1 (x,y)      
      fun laripsc1_tr (n, (x,y)) =
        if n = 0 then (x, y)
        else laripsc1_tr (n-1, succ(x, y))
in fun laripsc1_iter4 n = laripsc1_tr (n, (0, 0))
end

