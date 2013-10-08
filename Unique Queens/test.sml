local
  val allfiles = [1,2,3,4,5,6,7,8];
  val ranksep = "\n   -------------------------------------------------\n";
  val filelab =   "      A     B     C     D     E     F     G     H";

  (* get the display string for rank i *)
  fun getRankStringRec(_,0) = ""
  |   getRankStringRec(1,i) = "|  Q  " ^ getRankStringRec(~1,i-1)
  |   getRankStringRec(f,i) = "|     " ^ getRankStringRec(f-1,i-1);

  fun getRankString(f) = getRankStringRec(f,8) ^ "|";

  (* get the display string of the whole board *)
  fun getBoardString(nil, _) = "" (* never happens *)
  |   getBoardString([f], fnum) = Int.toString(fnum) ^ "  " ^ 
				  getRankString(f) ^
				  "  " ^ Int.toString(fnum)
  |   getBoardString(f::fs, fnum) = Int.toString(fnum) ^ "  " ^ 
				    getRankString(f) ^ 
				    "  " ^ Int.toString(fnum) ^
				    ranksep ^ 
				    getBoardString(fs, fnum+1);

  (* print out the board to display the solution *)
  fun showboard(board) = print("\n" ^ filelab ^ ranksep ^ getBoardString(board, 1) ^ ranksep ^ filelab ^ "\n\n");

  (* check if we can add a new rank with a queen at file f
   * to the solution built so far *)
  fun isSafeRec(_, _, _, nil) = true
  |   isSafeRec(f, fup, fdwn, bf::board) = 
      if f=bf orelse fup=bf orelse fdwn=bf then false
      else isSafeRec(f, fup+1, fdwn-1, board);

  fun isSafe(f, board) = isSafeRec(f, f+1, f-1, board);

  (* incrementally build a solution by adding one queen per rank *)
  fun solveEQ(files, board) = 
    if length(board) = 8 then (showboard(board); true)
    else if files=nil then false
    else 
      let
        val f::fs = files
      in
        if isSafe(f, board) andalso solveEQ(allfiles, f::board) then true 
	else solveEQ(fs, board)
      end;

  (* incrementally build all solutions by adding one queen per rank *)
  fun solveEQAll(files, board) = 
    if length(board) = 8 then [board]
    else if files=nil then nil
    else 
      let
        val f::fs = files
      in
        if isSafe(f, board) then solveEQAll(allfiles, f::board) @ solveEQAll(fs, board) else
	solveEQAll(fs, board)
      end;
in
  (* our public functions *)
  fun eightQueens() = solveEQ(allfiles, nil);
  fun eightQueensAll() = solveEQAll(allfiles, nil);
end;

val current = hd(eightQueensAll())

fun checkIfEqual(nil,nil) = true
|	checkIfEqual(x,nil) = false
|	checkIfEqual(nil,xs) = false
|	checkIfEqual(x::xs,y::ys) =
	if x = y then checkIfEqual(xs,ys)
	else false;
		
fun checkList(current,nil) = nil
|	checkList(current,x::xs) = 
	if checkIfEqual(current,x) then checkList(current,xs)
	else x :: checkList(current,xs)

fun count(nil) = 0
|	count(x::xs) =
	1 + count(xs);

fun flipVertical(L) = (* function to reverse a list*)
	if L = nil then nil
	else flipVertical(tl(L)) @ [hd(L)];
						
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
		
		fun getPosByVal(value,nil) = 0
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

	val rotation90 = rotate90(current)	
	val rotation180 = rotate180(current)
	val r180 = rotate90(rotate90(current))
	val r270 = rotate90(rotate90(rotate90(current)))
	val rotation270 = rotate90(rotate180(current))
	val horizontal = flipHorizontal(current)
	val vertical = flipVertical(current)
	val negXY = rotateNegXY(current)
	val rotateXY = rotateNegXY(rotate180(current))