pattern.sim <-
function(r=NULL, a=NULL, b=NULL, type=c('homogeneous', 'linear', 'quadratic', 'moon', 'ridge'), pts=NULL) {
	if(is.null(r)){ r <- 8 }
	if(is.null(a)){ a <- .5*r }
	if(is.null(b)){ b <- .5*r }
	
	if(type[1] %in% 'homogeneous'){
		if(is.null(r)){
			stop(paste('Argument "r" is required for type "',
				type[1], '.', sep=''))
		}
		n1  <- rpois(1, lambda = exp(r))
		x2  <- runif(n1)
		y2  <- runif(n1)
	} else if(type[1] %in% c('linear', 'quadratic')){
		if(is.null(a)){
			stop(paste('Argument "a" is required for type "',
				type[1], '.', sep=''))
		}
		if(is.null(b)){
			stop(paste('Argument "b" is required for type "',
				type[1], '.', sep=''))
		}
		n1  <- rpois(1, lambda = exp(a + b))
		x1  <- runif(n1)
		y1  <- runif(n1)
		z1  <- runif(n1)
		if(type[1] == "linear") {
			k1 <- (z1 < exp(a*x1 + b*y1)/exp(a + b))
		} else if(type[1] == "quadratic") {
			temp <- (a + b) - a*(x1 - .5)^2 - b*(y1 - .5)^2
			k1   <- (z1 < exp(temp)/exp(a + b))
		}
		x2  <- x1[k1]
		y2  <- y1[k1]
	} else if(type[1] == 'ridge'){
		if(is.null(r)){
			stop(paste('Argument "r" is required for type "',
				type[1], '.', sep=''))
		}
		n1   <- rpois(1, lambda = exp(r))
		x1   <- runif(n1)
		y1   <- runif(n1)
		z1   <- runif(n1)
#		temp <- dnorm(x1, .3 - .2*y1, .02) + .2
#		k1   <- (z1 < temp/20.14711)
		k1  <- (z1 < ridge(r, x1, y1)[,4])
		x2   <- x1[k1]
		y2   <- y1[k1]
	} else if(type[1] == 'moon'){
		if(is.null(r)){
			stop(paste('Argument "r" is required for type "',
				type[1], '.', sep=''))
		}
		n1  <- rpois(1, lambda = exp(r))
		x1  <- runif(n1)
		y1  <- runif(n1)
		z1  <- runif(n1)
		k1  <- (z1 < moon(r, x1, y1)[,4])
		x2  <- x1[k1]
		y2  <- y1[k1]
	} else {
		stop('The argument "type" is not recognized.')
	}
	if(!is.null(pts)) {
		x1 <- pts[,1]
		y1 <- pts[,2]
		if(type[1] == 'homogeneous') {
			lambda <- rep(exp(r), length(x1))
		} else if(type[1] == 'linear') {
			lambda <- exp(a*x1 + b*y1)
		} else if(type[1] == 'quadratic') {
			lambda <- exp((a + b) - a*(x1 - .5)^2 - b*(y1 - .5)^2)
		} else if(type[1] == 'ridge') {
			lambda <- ridge(r, x1, y1)[,3]
		} else if(type[1] == 'moon') {
			lambda <- moon(r, x1, y1)[,3]
		}
	}
	if(is.null(pts)){
		return(cbind(x=x2, y=y2))
	} else {
		temp           <- cbind(pts, lambda)
		colnames(temp) <- c("x", "y", "lambda")
		return(temp)
	}
}

