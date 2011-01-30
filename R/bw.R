bw <-
function(x, method = c("pythagoras", "cross-validation"), h=NULL, poly=NULL, extra=FALSE) {
	if(class(x) == "deldir") {
		p <- x$summary[,1:2]
	} else if(class(x) == "ve"){
		p <- x$p
	} else if(class(x) == "matrix"){
		p <- x[,1:2]
	} else if(class(x) == "data.frame"){
		p <- cbind(x[,1], x[,2])
	} else {
		stop("Object x is not recognized")
	}
	method <- tolower(substr(method[1],1,1))
	if(method == "p"){
		nrdx <- bw.nrd0(p[,1])
		nrdy <- bw.nrd0(p[,2])
		out  <- sqrt(nrdx^2 + nrdy^2)
		return(out)
	} else if(method == "c"){
		if(is.null(h)[1]){
			stop('Argument h is required with the cross validation method')
		}
		if(is.null(poly)[1]){
			stop('Argument poly is required with the cross validation method')
		}
		n   <- nrow(p)
		stt <- rep(NA, length(h))
		for(j in 1:length(h)) {
			est <- rep(NA, n)
			for(i in 1:n) {
				x      <- matrix(p[i,], 1, 2)
				est[i] <- log(lambdahat(p[-i,],h[j],x,poly)$lambda)
			}
			stt[j] <- -sum(est) # -(1/n)*sum(est)
		}
		mat <- cbind(h, stt)
		win <- mat[order(stt)[1], 1]
		if(extra){
			return(list(mat = mat, hStar = win))
		} else {
			temp        <- win[1]
			names(temp) <- NULL
			return(temp)
		}
	} else {
		stop('The method is not recognized')
	}
}
