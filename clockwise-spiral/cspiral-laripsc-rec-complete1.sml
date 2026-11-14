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
in
   fun cspiral_rec (x, y) = if (x=0 andalso y = 0) then 0
                         else (* pred (x, y) is the predecessor of (x, y) *)
                              1 + cspiral_rec (pred (x, y))
end

local
    fun succ (x,y) =
      if  (x <= 0 andalso y >= x andalso y <= ~x) then up1 (x,y)
      else if (y > 0  andalso x >= ~y+1 andalso x <= y-1) then right1 (x,y)
      else if (x > 0 andalso y >= ~x+1 andalso y <= x) then down1 (x,y)
      else (* y < 0 andalso x >= y+1 andalso x <= ~y*) left1 (x,y)      
in
   exception negative
   fun laripsc_rec n = if n < 0 then raise negative
                    else if n = 0 then (0,0) 
                    else succ (laripsc_rec(n-1))       
end
