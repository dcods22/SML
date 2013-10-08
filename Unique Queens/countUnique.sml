	(*Dan Cody*)
	fun countUnique(queenList) =
		let
			
			(* make final list*)
			fun makefinallist(nil) = nil
			|	makefinallist(queenList) = 
				let
					(*get the current unique element*)
					val current = hd(queenList)
			
					(*check if the strings are equal *)
					fun checkIfEqual(nil,nil) = true
					|	checkIfEqual(x:int list,nil) = false
					|	checkIfEqual(nil,xs) = false
					|	checkIfEqual(x::xs,y::ys) =
						if x = y then checkIfEqual(xs,ys)
						else false;	
					
					(*flip vertically *)
					fun flipVertical(L:int list) = (* function to reverse a list*)
						if L = nil then nil
						else flipVertical(tl(L)) @ [hd(L)];
					
					(* flip horizontally *)					
					fun flipHorizontal(nil) = nil
					|	flipHorizontal(x::xs) =
						if x = 1 then 8 :: flipHorizontal(xs)
						else if x = 2 then 7 :: flipHorizontal(xs)
						else if x = 3 then 6 :: flipHorizontal(xs)
						else if x = 4 then 5 :: flipHorizontal(xs)
						else if x = 5 then 4 :: flipHorizontal(xs)
						else if x = 6 then 3 :: flipHorizontal(xs)
						else if x = 7 then 2 :: flipHorizontal(xs)
						else if x = 8 then 1 :: flipHorizontal(xs)
						else nil;
					
					(*rotate 180 degrees*)
					fun rotate180(x) =
						let
							val reverse = flipVertical(x)
						in
							flipHorizontal(reverse)
						end;
						
					(*write a get string position by value function*)	
					fun rotate90(nil) = nil
					|	rotate90(queens) =
						let
							
							fun getPosByVal(value:int,nil) = 0
							|	getPosByVal(value,x::xs) =
								if value = x then 1
								else 1 + getPosByVal(value,xs);
								
							val pos1 = getPosByVal(8,queens)
							val pos2 = getPosByVal(7,queens)
							val pos3 = getPosByVal(6,queens)
							val pos4 = getPosByVal(5,queens)
							val pos5 = getPosByVal(4,queens)
							val pos6 = getPosByVal(3,queens)
							val pos7 = getPosByVal(2,queens)
							val pos8 = getPosByVal(1,queens)
							
						in
						
							[pos1,pos2,pos3,pos4,pos5,pos6,pos7,pos8]
						
						end;	
					
					(* rotate in the negative XY *)
					fun rotateNegXY(nil) = nil
					|	rotateNegXY(queens) = 
						let
							
							fun getPosByVal(value:int,nil) = 0
							|	getPosByVal(value,x::xs) =
								if value = x then 1
								else 1 + getPosByVal(value,xs);
								
							val col1 = getPosByVal(1,queens)
							val col2 = getPosByVal(2,queens)
							val col3 = getPosByVal(3,queens)
							val col4 = getPosByVal(4,queens)
							val col5 = getPosByVal(5,queens)
							val col6 = getPosByVal(6,queens)
							val col7 = getPosByVal(7,queens)
							val col8 = getPosByVal(8,queens)
						
						in
						
							[col1,col2,col3,col4,col5,col6,col7,col8]
						
						end;
					
					(* all of the possible rotations*)
					val rotation90 = rotate90(current)	
					val rotation180 = rotate180(current)
					val rotation270 = rotate90(rotate180(current))
					val horizontal = flipHorizontal(current)
					val vertical = flipVertical(current)
					val negXY = rotateNegXY(current)
					val rotateXY = rotateNegXY(rotate180(current))
					
					(* check the entire list to see if any non unique are there *)
					fun checkList(current,nil) = nil
					|	checkList(current,x::xs) = 
						if checkIfEqual(current,x) then checkList(current,xs)	
						else if checkIfEqual(horizontal,x) then checkList(current,xs)
						else if checkIfEqual(vertical,x) then checkList(current,xs)
						else if checkIfEqual(rotation90, x) then checkList(current,xs)
						else if checkIfEqual(rotation180,x) then checkList(current,xs)
						else if checkIfEqual(rotation270, x) then checkList(current,xs)
						else if checkIfEqual(negXY, x) then checkList(current,xs)
						else if checkIfEqual(rotateXY, x) then checkList(current,xs)
						else x :: checkList(current,xs);
				
				in
					(*build the unique list*)
					current :: makefinallist(checkList(current,queenList))
				
				end;
			
			(* count the elements in the list *)
			fun count(nil) = 0
			|	count(x::xs) =
				1 + count(xs);
				
			val finalList = makefinallist(queenList)	
		in 
			(*count the number of unique elements*)
			count(finalList)
			
		end;
