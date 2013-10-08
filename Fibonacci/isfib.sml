fun isfib (L) =
		if L = 0 then true
		else if L = 1 then true
		else if fib(L,0,1) then true
		else false
	and
fib(L, N, M ) = 
	if L = (N + M) then true
	else if L < (N + M) then false
	else fib(L, M, (N+M));

	
