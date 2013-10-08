fun subset(ls1,ls2) = 
	let 
		fun issubset(ls1,ls2) = if ls1 = nil then true
		else if isin ((hd(ls1)),ls2)
		then issubset ((tl(ls1)),ls2)
		else false
	
		and isin (n,ls2) =
			if ls2 = nil then false
			else if n = (hd(ls2)) then true
			else isin (n,(tl(ls2)));
	in		
		if issubset(ls1,ls2) 
		then if issubset(ls2,ls1) 
		then false 
		else true
		else false
	end;