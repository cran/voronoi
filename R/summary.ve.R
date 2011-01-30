summary.ve <-
function(object, ...){
	n        <- object$T$n.data
	haveLocs <- !is.null(object$est)
	ve       <- object$z
	cat("Number of points:", n, "\n")
	if(haveLocs){
		cat("Special locations with estimates: ")
		cat(nrow(object$est), "\n")
	} else {
		cat("No user-specified location estimates available\n")
	}
	# summary of z's
	z  <- object$z
	co <- mean(z) + (-2:2)*sd(z)
	p  <- rep(-1, 6)
	for(i in 1:6){
		p[i] <- sum(z < co[i])
	}
	out  <- abs(z - mean(z)) > 2*sd(z)
	if(any(out)){ # potential outliers
		outV    <- ((z - mean(z))/sd(z))[out]
		outZ    <- z[out]
		outP    <- object$p[out,]
		if(sum(out) == 1){
			toPrint <- matrix(c(outP, outZ, outV), 1)
		} else {
			toPrint <- cbind(outP, outZ, outV)[order(outV),]
		}
		toPrint <- data.frame(toPrint)
		colnames(toPrint) <- c("  Dim 1", "    Dim 2",
				"  Estimate", "  Z score")
		cat("Observations more than two SD from the mean:\n")
		print(format(toPrint, digits=3))
		tR          <- list()
		class(tR)   <- "summaryve"
		tR$outliers <- toPrint
		tR$ve       <- object
		invisible(tR)
	} else {
		cat("No observations are further than 2 SD from the mean\n")
		tR          <- list()
		class(tR)   <- "summaryve"
		tR$outliers <- NULL
		tR$ve       <- object
		invisible(tR)
	}
}
