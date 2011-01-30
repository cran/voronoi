angle <-
function(p1, p2){
	if(any(c(length(p1), length(p2)) != 2)){
		stop("p1 and p2 must be numeric vectors of length 2")
	}
	if(all(p1 == p2)){
		stop("p1 and p2 must be unique points")
	}
	p <- p2 - p1
	p <- p/sqrt(sum(p^2))
	if(p[2] == 0){
		if(p[1] == 1){
			0
		} else {
			pi
		}
	} else if(p[2] > 0){
		return(acos(p[1]))
	} else {
		return(2*pi - acos(p[1]))
	}
}
