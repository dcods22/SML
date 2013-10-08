(*
Daniel Cody
Professor Carle
Language Study
Assignment 3
*)

(*Open the Text reading functions*)
open TextIO;
use "distance.sml";	
(*Exception for a zipcode error*)
exception NegativeThreshold of real;

(* main fucntion which allows to get the nearby 
   list of zipcodes based on the distance threshold  *)
fun getnearbylist(center:string, threshold:real, ziplist:string list) =
	let
	
		(*function to check the input of the threshold*)
		fun ThresholdOutOfRange1(n) = 
			if n >= 0.0 then nil 
			else raise NegativeThreshold(n);

		(*function to handle the exception NegativeThreshold*)	
		fun ThresholdOutOfRange(n) = ThresholdOutOfRange1(n) handle
			NegativeThreshold(n) => (
										print("Error: Negative Threshold");
										print("\n");
										nil
									 );	
			
		(*function that returns a list of an entire cells contents*)
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
		
		(*function that returns a list of each cell in an entire line*)
		fun makelinelist(file) = 
			case TextIO.lookahead(file) of
				NONE       => nil |
				SOME #"\n" => let val nl = TextIO.input1(file) in nil end |
				SOME c => getcell(file) :: makelinelist(file);
			
		(*function that returns the distance of two zipcodes based on their longitude and latitude*)
		
	
		(*function that gets the zipcode from a line make by the function makelinelist*)
		fun getzip(line) =
			hd(tl(line))
		
		(*function that gets the latitude from a line make by the function makelinelist*)
		fun getlat(line) =
			let
				val L = hd(tl(tl(tl(tl(tl(tl(tl(line))))))))
			in
				Real.fromString(L)
			end;
		
		(*function that gets the longitude from a line make by the function makelinelist*)
		fun getlong(line) =
			let
				val L = hd(tl(tl(tl(tl(tl(tl(tl(tl(tl(tl((line))))))))))))
			in
				Real.fromString(L)
			end;	
		
		(*function that takes an option and returns the value, used for real option to real*)
		fun getOption(x) = 
			Option.getOpt(x,0.0);	
		
		(*first pass through the csv file which takes the center and returns their longitude and latitude*)
		fun firstPass(center,infile) =
			let
				val line = makelinelist(infile)
			in
				if center = getzip(line) then
					(getlat(line),getlong(line))
				else	
					firstPass(center,infile)
			end;
					
		val (permlat, permlong) = firstPass(center, openIn("zips.csv"))
		val permlatitude = getOption(permlat)
		val permlongitude = getOption(permlong)
	
		(*function that checks if a zipcode is in the ziplist*)
		fun ziplistcheck(zipcode:string,nil) = false
		|	ziplistcheck(zipcode,x::nil) = if zipcode = x then true else false
		|	ziplistcheck(zipcode,x::xs) = if zipcode = x then true else ziplistcheck(zipcode,xs);
		
		(*function that returns a list of zipcodes that is in the ziplist and within the threshold based on the distance*)
		fun secondPass(threshold,ziplist,permlongitude,permlatitude,infile) =
			let
				val line = makelinelist(infile)
				
				val latitude = getOption(getlat(line))
				val longitude = getOption(getlong(line))
				val currentzip = getzip(line)
				
				val distance = Distance.distance(permlatitude, permlongitude, latitude, longitude)	
			in
				if not(endOfStream(infile)) then
					if ziplistcheck(currentzip,ziplist) then
						if distance <= threshold then
							currentzip :: secondPass(threshold,ziplist,permlongitude,permlatitude,infile)
						else 
							secondPass(threshold,ziplist,permlongitude,permlatitude,infile)
					else
						secondPass(threshold,ziplist,permlongitude,permlatitude,infile)
				else nil
			end;
			
	in
		
		ThresholdOutOfRange(threshold);
		secondPass(threshold,ziplist,permlongitude,permlatitude,openIn("zips.csv"))
		
	end;