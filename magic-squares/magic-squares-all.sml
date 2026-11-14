local (* all *)

    (* ============== begin Globals =================== *)
    fun odd n = (n mod 2 = 1);
    fun even n = not(odd n);
    fun doublyEven n = (n mod 4 = 0)
    fun singlyEven n = (n mod 4 = 2)
    exception NotPositiveInteger
    exception NotDoublyEven    
    val i2s = Int.toString
    exception NotOddPositiveInteger
    exception NotSinglyEven
    exception ImpossibleFor2
		 
    (* ============== end Globals =================== *)
		 
    local (* odd order; Loubere *)

	fun succxy (n, x, y) = (* start from top row left to right and proceed down *)
            if (0 <= x) andalso (x < n) andalso (0<= y) andalso (y<n)
            then if (x+1<n) then (x+1, y)
		 else (0, y-1) 
            else (0,~1) (* wrap around to the "next row left hand square" *)
		     
	fun printMagicSquareFrom (n, x0, y0, d0, k, x, y) =
            let val d = (x-y) mod n
		val j = (d-d0) mod n
		val i = (x - x0 + j) mod n
		val Mxy = k+j*n+i
		val (xs, ys) = succxy (n, x, y)
            in  (  if (x = n-1) then print (i2s(Mxy)^"\n")
		   else print (i2s(Mxy)^", ");
		   if (xs < n) andalso (ys >= 0) 
		   then  printMagicSquareFrom (n, x0, y0, d0, k, xs, ys)
		   else  print ("\n")
		)
            end
    in
    fun loubere (n, k) = (* Give the dimension and the starting number *)
	if (n < 0 orelse not(odd n)) then raise NotOddPositiveInteger
	else let val (x0, y0) = ((n-1) div 2, n-1)
		 val d0 = (x0-y0) mod n
             in  (* start generating from the top row cells (0, n-1) to (n-1, n-1) *)
		 printMagicSquareFrom (n, x0, y0, d0, k, 0, n-1)
             end
    end (* local loubere *)
    
(* ========= begin succ for even numbers ================= *)
    fun succ (n, r, c) = (* start from top row left to right and proceed down *)
        if (0 <= r) andalso (r < n) andalso (0<= c) andalso (c<n)
        then if (c+1<n) then (r, c+1)
             else (* c = n-1 *)
                 if (r+1 < n) then (r+1, 0) 
                 else (n,0)
        else (* r < 0 or r >= n or c < 0 or c >= n *) (n,0)
(* ========== end succ for even numbers ================== *)

    local (* doubly even; umar *)

	fun printMagicSquareFrom (n, k, m, p, nsqr, r, c) =
	    let 
		val Mrc = (k-1) +
			  (if (r,c) = (m, m-1) then p
			   else if (r,c) = (n-1, m-1) then p+m
			   else if (r, c) = (m, m) then p+1
			   else if (r, c) = (n-1, m) then p-m+1
			   else if r < m then
                               if even (r) then
				   if c < m then c*n+r+1
				   else (* c >= m *) c*n+n-r
                               else (* odd (r) *)
				   if c < m then nsqr - c*n - r
                               else (* x >= m *) nsqr-n-c*n + r + 1
			   else (* r >= m *)
                               if even (r) then
				   if c < m then nsqr - c*n - r
				   else (* x >= m *) nsqr-n-c*n+r+1
                               else (* odd (y) *)
				   if c < m then c*n+r+1
				   else (* c >= m *) c*n + n - r
			  )
		val (rs, cs) = succ (n,r,c)
	    in  (  if (c = n-1) then print (Int.toString(Mrc)^"\n")
		   else print (Int.toString(Mrc)^", ");
		   if (rs < n) andalso (cs >= 0) 
		   then  printMagicSquareFrom (n, k, m, p, nsqr, rs, cs)
		   else  print ("\n")
		)
	    end
    in
    fun umarDoublyEven (n, k) = (* Give the dimension and the starting number *)
	if (n < 0 orelse not(doublyEven n)) then raise NotDoublyEven
	else
	    let val m = n div 2
		val nsqr = n*n
		val p = nsqr  div 2
	    in  printMagicSquareFrom (n, k, m, p, nsqr, 0, 0)
	    end (* let *) 
    end (* local doubly even *)
    
(* ====================================================== *)

    local (* singly even; umar *)
	
	fun printMagicSquareFrom (n, k, m, p, nsqr, r, c) =
	    let val Mrc = (k-1) +
			  ( (* Step E' *)
			    if (r,c) = (m-2, m-2) then p+5*m-3
			    else if (r,c) = (m+1, m-2) then p+5*m-2
			    else if (r, c) = (m-2, m+1) then p-5*m+4 
			    else if (r, c) = (m+1, m+1) then p-5*m+3
			    else (* middle rows; Step D' *)
				if r = m-1 then 
				    if c < m then
					if even(c) then p+n-c
					else (* odd(c) *) p-n+c+1
				    else (* c >= m *)
					if (c < m+2 orelse c > n-3) then p+m-c
					else (* m+2 <= c <= n-3 *)
					    if even (c) then p+m-c
					    else (* odd (c) *) p-m+c+1
				else (* r <> m-1 *)
			      if r= m then
				  if c < m then
				      if even(c) then p-n+c+1
				      else (* odd(c) *) p+n-c
				  else (* c >= m *)
				      if (c < m+2 orelse c > n-3) then p-m+c+1
				      else (* m+2 <= c <= n-3 *)
					  if even (c) then p-m+c+1
					  else (* odd (c) *) p+m-c
			      else (* r is not middle row; Step B' *)
				  if r < m-1 then
				      if even (r) then
					  if c < m then c*(n-2)+r+1
					  else (* c >= m *) c*(n-2)+3*n-r-2
				      else (* odd (r) *)
					  if c < m then nsqr-c*(n-2)-r
					  else (* c >= m *) nsqr-c*(n-2)-3*n+r+3
				  else (* r > m *)
				      if even (r) then
					  if c < m then nsqr-c*(n-2)-(r-2)
					  else (* c >= m *) nsqr-c*(n-2)-3*n+r+1
				      else (* odd (r) *)
					  if c < m then c*(n-2)+r-1
					  else (* c >= m *) c*(n-2)+3*n-r 
			  )
		val (rs, cs) = succ (n,r,c)
	    in (if (c = n-1) then print (Int.toString(Mrc)^"\n") else print (Int.toString(Mrc)^", ");
		if (rs < n) andalso (cs >= 0) then  printMagicSquareFrom (n, k, m, p, nsqr, rs, cs)
		else  print ("\n"))
	    end (* let *)
    in fun umarSinglyEven (n, k) = (* Give the dimension and the starting number *)
	   if (n < 0 orelse not(singlyEven n)) then raise NotSinglyEven
	   else if n=2 then raise ImpossibleFor2
	   else let val m = n div 2; val nsqr = n*n; val p = nsqr  div 2
		in  printMagicSquareFrom (n, k, m, p, nsqr, 0, 0)
		end (* let *) 
    end (* local singly even umar *)

in
(* =================== begin Main ============================= *)
fun magicSquares (n, k:int) =
    if n <= 0 then raise NotPositiveInteger
    else ( case (n mod 4)
	    of 0 => umarDoublyEven (n, k)
	    |  1 => loubere (n, k)
	    |  2 => umarSinglyEven (n, k)
	    |  3 => loubere (n, k)
	 )
end (* local all *)

fun msk (n, k) = magicSquares (n, k)
fun ms1 n = magicSquares (n, 1)
(* =================== end Main =============================== *)

(* try for k=1 
Standard ML of New Jersey v110.79 [built: Tue Aug  8 23:21:20 2017]
- [opening /home/self/sak/sml/programs/magic-squares/magic-squares-all.sml]
[autoloading]
[library $SMLNJ-BASIS/basis.cm is stable]
[library $SMLNJ-BASIS/(basis.cm):basis-common.cm is stable]
[autoloading done]
/home/self/sak/sml/programs/magic-squares/magic-squares-all.sml:165.12-169.28 Warning: match nonexhaustive
          0 => ...
          1 => ...
          2 => ...
          3 => ...
  
val magicSquares = fn : int * int -> unit
val msk = fn : int * int -> unit
val ms1 = fn : int -> unit
val it = () : unit
- ms1 0;

uncaught exception NotPositiveInteger
  raised at: /home/self/sak/sml/programs/magic-squares/magic-squares-all.sml:164.26-164.44
- ms1 1;
1

val it = () : unit
- ms1 2;

uncaught exception ImpossibleFor2
  raised at: /home/self/sak/sml/programs/magic-squares/magic-squares-all.sml:155.28-155.42
- ms1 3;
8, 1, 6
3, 5, 7
4, 9, 2

val it = () : unit
- ms1 4;
1, 5, 12, 16
15, 11, 6, 2
14, 8, 9, 3
4, 10, 7, 13

val it = () : unit
- ms1 5;
17, 24, 1, 8, 15
23, 5, 7, 14, 16
4, 6, 13, 20, 22
10, 12, 19, 21, 3
11, 18, 25, 2, 9

val it = () : unit
- ms1 6;
1, 5, 9, 28, 32, 36
35, 30, 27, 10, 7, 2
24, 14, 22, 18, 17, 16
13, 23, 15, 19, 20, 21
34, 31, 26, 11, 6, 3
4, 8, 12, 25, 29, 33

val it = () : unit
- ms1 7;
30, 39, 48, 1, 10, 19, 28
38, 47, 7, 9, 18, 27, 29
46, 6, 8, 17, 26, 35, 37
5, 14, 16, 25, 34, 36, 45
13, 15, 24, 33, 42, 44, 4
21, 23, 32, 41, 43, 3, 12
22, 31, 40, 49, 2, 11, 20

val it = () : unit
- ms1 8;
1, 9, 17, 25, 40, 48, 56, 64
63, 55, 47, 39, 26, 18, 10, 2
3, 11, 19, 27, 38, 46, 54, 62
61, 53, 45, 37, 28, 20, 12, 4
60, 52, 44, 32, 33, 21, 13, 5
6, 14, 22, 30, 35, 43, 51, 59
58, 50, 42, 34, 31, 23, 15, 7
8, 16, 24, 36, 29, 41, 49, 57

val it = () : unit
- ms1 9;
47, 58, 69, 80, 1, 12, 23, 34, 45
57, 68, 79, 9, 11, 22, 33, 44, 46
67, 78, 8, 10, 21, 32, 43, 54, 56
77, 7, 18, 20, 31, 42, 53, 55, 66
6, 17, 19, 30, 41, 52, 63, 65, 76
16, 27, 29, 40, 51, 62, 64, 75, 5
26, 28, 39, 50, 61, 72, 74, 4, 15
36, 38, 49, 60, 71, 73, 3, 14, 25
37, 48, 59, 70, 81, 2, 13, 24, 35

val it = () : unit
- ms1 10;
1, 9, 17, 25, 33, 68, 76, 84, 92, 100
99, 91, 83, 75, 67, 34, 26, 18, 10, 2
3, 11, 19, 27, 35, 66, 74, 82, 90, 98
97, 89, 81, 72, 65, 36, 29, 20, 12, 4
60, 42, 58, 44, 56, 50, 49, 53, 47, 46
41, 59, 43, 57, 45, 51, 52, 48, 54, 55
96, 88, 80, 73, 64, 37, 28, 21, 13, 5
6, 14, 22, 30, 38, 63, 71, 79, 87, 95
94, 86, 78, 70, 62, 39, 31, 23, 15, 7
8, 16, 24, 32, 40, 61, 69, 77, 85, 93

val it = () : unit
- 
*)
