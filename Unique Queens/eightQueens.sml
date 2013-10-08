(* 
 * Find solutions to the eight queens problem
 * @author Benjamin Carle
 *)
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




(* eightQueens(); *)
(* eightQueensAll(); *)
