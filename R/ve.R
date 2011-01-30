ve <-
function(x, rw=NULL, locations=NULL) {
	if(class(x) == "deldir") {
		p <- x$summary[,1:2]
		T <- x
	} else if(class(x) == "ve"){
		p <- x$p
		T <- x$T
	} else if(class(x) %in% c("matrix", "data.frame")){
		T <- deldir(x[,1], x[,2], rw = rw)
		p <- T$summary[,1:2]  # watch out, this things eats points
	} else {
		stop("Object x is not recognized")
	}

	tR     <- list()
	tR$p   <- p
	tR$z   <- 1/T$summary[,8]
	tR$T   <- T
	if(class(x) %in% c("matrix", "data.frame")){
		if(!is.null(rw)[1]){
			tR$rw <- rw
		}
	}
	if(is.null(tR$rw)[1]){
		tR$rw <- tR$T$rw
	}
	tR$est <- NULL
	class(tR) <- "ve"
	if(!is.null(locations)[1]){
		est <- rep(NA, nrow(locations))
		for(i in 1:nrow(locations)) {
			d2a  <- (tR$p[,1] - locations[i, 1])^2
			d2b  <- (tR$p[,2] - locations[i, 2])^2
			cell <- which.min(d2a + d2b)
			est[i] <- tR$z[cell]
		}
		tR$est <- cbind(locations, est)
	}
	return(tR)
}

