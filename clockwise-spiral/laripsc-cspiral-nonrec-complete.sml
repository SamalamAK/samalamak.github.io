exception Negative
fun sqr z = z*z
fun cspiral2 (x, y) =
    let val n = Int.max (abs(x), abs(y))
        val two_n = 2*n
    in  if (y = n andalso x > ~n+1) then sqr (two_n -1)+x+n-1
        else if (x=n andalso y < n) then two_n*(two_n - 1)+ n-y
        else if (y = ~n andalso x < n) then sqr(two_n) + n - x
        else (* if x = ~n andalso y > ~n then *) two_n*(two_n+1) + y + n
    end

fun laripsc2 m =
    if m < 0 then raise Negative
    else if m = 0 then (0,0)
    else
        let val s = Real.trunc(Real.Math.sqrt(real(m+1)))  
            val n = if s*s < m+1 then (s+1) div 2 else s div 2
            val two_n = 2*n
            val p = m - sqr(two_n - 1)
        in  if p < two_n then (p+1-n, n)
            else if p < 2*two_n then (n, 3*n-p-1)
            else if p < 3*two_n then (5*n-p-1,n)
            else (* if p < 4*two_n then *) (~n,p+1-7*n)
        end
