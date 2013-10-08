structure Distance = struct

	fun distance(latitude1:real, longitude1:real, latitude2:real, longitude2:real) = 
			let
				val latitude1 = (90.0-latitude1) * ((2.0 * Math.pi)/360.0)
				val longitude1 = longitude1 * ((2.0 * Math.pi)/360.0)
				val latitude2 = (90.0-latitude2) * ((2.0 * Math.pi)/360.0)
				val longitude2 = longitude2 * ((2.0 * Math.pi)/360.0)
			in
				(3960.0 * (Math.acos((Math.sin(latitude1) * Math.sin(latitude2) * Math.cos(longitude1 - longitude2)) + (Math.cos(latitude1) * Math.cos(latitude2)))))
			end;

end;