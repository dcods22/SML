(*Open the Text reading functions*)
open TextIO;

(*Exception for a zipcode error*)
exception ZipException

fun printzips() =
	let
		fun getcell(file) = 
			case lookahead(file) of
				NONE       => "" |
				SOME #"\n" => "" |
					SOME c =>
			case input1(file) of
				NONE => "" |
				SOME c => if c = #"," then ""
						  else if c = #"\"" then ""
						  else str(c) ^ getcell(file);
							
		fun makelinelist(file) = 
			case TextIO.lookahead(file) of
				NONE       => nil |
				SOME #"\n" => let val nl = TextIO.input1(file) in nil end |
				SOME c => getcell(file) :: makelinelist(file);
		
		fun firstPass(infile) =
			let
				val line = makelinelist(infile)
			in
				hd(tl(tl(tl(tl(tl(tl(tl(tl(tl(tl((line))))))))))))
			end;
	in

		makelinelist(openIn("zips.csv"));
		firstPass(openIn("zips.csv"))
		
	end;