(*Daniel Cody*)

(*This function will create a list of touples in which they are 
each a stable match which means that there is no case based on their
prefrences in which they would rather switch their pairing*)

fun stableMatch filenameMen filenameWomen = 
	let
		
		(*file of men prefrences*)
		val fileMen = TextIO.openIn(filenameMen)
		(*file of womens preferences*)
		val fileWomen = TextIO.openIn(filenameWomen)
		
		(*get the cell of the csv file*)	
	    fun getcell(file) = 
			case TextIO.lookahead(file) of
				NONE       => "" |
				SOME #"\n" => "" |
					SOME c =>
				case TextIO.input1(file) of
				NONE => "" |
				SOME c => if c = #"," then ""
					  else str(c) ^ getcell(file);
		
		(*get the line of the csv file*)
		fun getLine(file) =
			case TextIO.lookahead(file) of
				NONE       => nil |
				SOME #"\n" => let val nl = TextIO.input1(file) in nil end |
				SOME c => getcell(file) :: getLine(file);

		(*make a list of the lines*)			
		fun makelinelist(file) = 
			let
				val nextline = getLine(file)
			in
				if nextline = nil then nil
				else nextline :: makelinelist(file)
			end;
		
		(*make the single man list*)
		fun singleMen(nil) = nil
		|	singleMen(x::xs) = 
				hd(x) :: singleMen(xs);
		
		(*mans prefrence list*)
		val menList = makelinelist(fileMen)
		(*womens prefrence list*)
		val womenList =	makelinelist(fileWomen)
		(*single man list*)
		val singleManList = singleMen(menList)
		
		fun makeMatch(menChoiceList, womenChoiceList, EngagedCouples, singleManList) = 
			let
				(*current man list*)
				val currManList = hd(menChoiceList)
				(*current Man*)
				val currentMan = hd(currManList)
				(*mans top choice*)
				val currWomen = hd(tl(currManList))
				
				(*checks list of enaged men/women to see if that women/man is in it*)
				fun engagedWomenCheck(person:string,nil) = false
				|	engagedWomenCheck(person,(x,y)::xs) = 
					if person = y then true
					else engagedWomenCheck(person,xs);
						
				(*get womans current husband*)
				fun getWomanMan(woman:string, nil) = ""
				|	getWomanMan(woman, (x,y)::xs) = 
					if woman = y then x
					else getWomanMan(woman,xs);
					
				(*check is man is married*)
				fun checkManMarried(man:string,nil) = false
				|	checkManMarried(man,(x,y)::xs) =
					if man = x then true
					else checkManMarried(man,xs);	
				
				(*add couple to engaged list*)
				fun addToSingleWoman(currMan:string, currWoman, EngagedCouplesList) = 
						(currMan, currWoman) :: EngagedCouplesList;
				
				(*remove man from the list*)
				fun removeMan(currMan:string, nil) = nil
				|	removeMan(currMan, x::xs) =
					if currMan = x then xs
					else removeMan(currMan,xs);
					
				(*is the single man  list empty*)
				fun singleManEmpty(nil) = true
				|	singleManEmpty(x) = false;
				
				fun removeManPref(manChoiceList, currentMan:string, currentWoman:string) = 
					let
					
						val currentLine = hd(manChoiceList)
						val lineMan = hd(currentLine)
						
						fun removeWoman(woman:string,nil) = nil
						|	removeWoman(woman,x::xs) =
							if woman = x then xs
							else removeWoman(woman,xs);
												
					in
						
						if currentMan = lineMan then 
							currentMan :: removeWoman(currentWoman,currentLine)
						else removeManPref(tl(manChoiceList), currentMan, currentWoman)					
						
					end;
					
				fun womenPrefNewMan(currWomen,currentMan, newMan, nil) = false
				|	womenPrefNewMan(currWomen,currentMan, newMan, womenList) =
					let
						val womanLine = hd(womenList)
						val woman = hd(womanLine)
						val prefList = tl(womanLine)
						
						fun checkPref(currentMan,newMan,nil) = false
						|	checkPref(currentMan:string,newMan:string, x::xs) = 
							if x = currentMan then false
							else if x = newMan then true
							else checkPref(currentMan, newMan, xs);
						
					in
						if currWomen = woman then
							checkPref(currentMan,newMan,prefList)
						else
							womenPrefNewMan(currWomen,currentMan, newMan, tl(womenList))
					end;
				
				(*Remove the engaged couple*)				
				fun removeEngaged(currWomen:string,nil) = nil
				|	removeEngaged(currWomen,(x,y)::xs) =
					if currWomen = y then xs
					else (x,y) :: removeEngaged(currWomen,xs); 	
					
			in
			
				(*check is singleman list is empty*)
				if not(singleManEmpty(singleManList)) then
					(*check is man is married*)
					if not(checkManMarried(currentMan,EngagedCouples)) then
						(*check is the woman is engaged*)
						if not(engagedWomenCheck(currWomen, EngagedCouples)) then
							(*get them engaged then take him off the single man list*)
							makeMatch((tl((menChoiceList @ [currManList]))), womenChoiceList, (addToSingleWoman(currentMan, currWomen, EngagedCouples)), (removeMan(currentMan,singleManList)))
						else
							(*check is women perfers new man*)
							if(womenPrefNewMan(currWomen, getWomanMan(currWomen, EngagedCouples), currentMan, womenChoiceList)) then
								(*marries new man and removes old husband*)
								makeMatch((tl((menChoiceList @ [currManList]))), womenChoiceList,((currentMan,currWomen) :: (removeEngaged(currWomen,EngagedCouples))), ((getWomanMan(currWomen, EngagedCouples)) :: (removeMan(currentMan,singleManList))))
							else
								(*stays married and removes her from prefrence list*)
								makeMatch(([removeManPref(menChoiceList, currentMan, currWomen)] @ menChoiceList ), womenChoiceList, EngagedCouples, singleManList)
					else
						(*nothing happens*)
						makeMatch((tl((menChoiceList @ [currManList]))),womenChoiceList,EngagedCouples, singleManList)	
				else
					(*program ends*)
					EngagedCouples
			end;
		
	in
		
		makeMatch(menList, womenList, nil, singleManList)
	
	end;