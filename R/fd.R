fd <-
function(x, index, rw = NULL, cut.to.border = FALSE) {
	if(class(x) == "deldir") {
		p <- x$summary[,1:2]
		T <- x
	} else if(class(x) == "ve"){
		p <- x$p
		T <- x$T
	} else if(class(x) %in% c("matrix", "data.frame")){
		if(is.null(rw)[1]){
			stop('Argument "rw" is required when "x" is a point pattern.')
		}
		T <- deldir(x[,1], x[,2], rw = rw)
		p <- T$summary[,1:2]  # watch out, this things eats points
	} else {
		stop("Object x is not recognized")
	}
	t.list   <- tile.list(T)
	x        <- T$summary[index,1]
	y        <- T$summary[index,2]
	vertices <- cbind(t.list[[index]]$x, t.list[[index]]$y)
	temp1    <- c(T$rw[1], T$rw[1], T$rw[2], T$rw[2])
	temp2    <- c(T$rw[3], T$rw[4], T$rw[4], T$rw[3])
	box      <- cbind(temp1, temp2)
	n        <- nrow(vertices)
	circles  <- list()
	radii    <- rep(NA, n)
	for(i in 1:n) {
		radii[i]     <- sqrt((vertices[i,1] - x)^2 + (vertices[i,2] - y)^2)
		Angle        <- angle(c(x,y), vertices[i,])
		Angles       <- seq(pi + Angle, 3*pi + Angle, length.out = 250)
		circ.x       <- vertices[i,1] + radii[i]*cos(Angles)
		circ.y       <- vertices[i,2] + radii[i]*sin(Angles)
		circles[[i]] <- cbind(circ.x, circ.y)
	}
	left.overlaps <- list()
	for(i in 1:(n - 1)) {
		temp1 <- (circles[[i]][,1] - vertices[i + 1, 1])^2
		temp2 <- (circles[[i]][,2] - vertices[i + 1, 2])^2
		left.overlaps[[i]] <- which(sqrt(temp1 + temp2) < radii[i + 1])
	}
	temp1 <- (circles[[n]][,1] - vertices[1, 1])^2
	temp2 <- (circles[[n]][,2] - vertices[1, 2])^2
	left.overlaps[[n]] <- which(sqrt(temp1 + temp2) < radii[1])
	right.overlaps <- list()
	for(i in 2:n) {
		temp1 <- (circles[[i]][,1] - vertices[i - 1, 1])^2
		temp2 <- (circles[[i]][,2] - vertices[i - 1, 2])^2
		right.overlaps[[i]] <- which(sqrt(temp1 + temp2) < radii[i - 1])
	}
	temp1 <- (circles[[1]][,1] - vertices[n, 1])^2
	temp2 <- (circles[[1]][,2] - vertices[n, 2])^2
	right.overlaps[[1]] <- which(sqrt(temp1 + temp2) < radii[n])
	all.overlaps <- list()
	for(i in 1:n) {
		all.overlaps[[i]] <- c(left.overlaps[[i]], right.overlaps[[i]])
	}
	for(i in 1:n) {
		circles[[i]] <- circles[[i]][-all.overlaps[[i]], ]
	}
	fun.dom <- matrix(0, 0, 2)
	for(i in 1:n) {
		fun.dom <- rbind(fun.dom, circles[[i]])
	}
	fun.dom <- fun.dom[which(!inout(fun.dom, vertices)), ]
	if(cut.to.border) {
		if((min(fun.dom[,1]) < T$rw[1]) | (max(fun.dom[,1]) > T$rw[2]) | 
				(min(fun.dom[,2]) < T$rw[3]) | (max(fun.dom[,2]) > T$rw[4])) {
			cut.fun.dom <- intersect(as(fun.dom, "gpc.poly"), as(box, "gpc.poly"))
			cut.fun.dom <- cbind(get.pts(cut.fun.dom)[[1]]$x, get.pts(cut.fun.dom)[[1]]$y)
			fun.dom     <- cut.fun.dom
		}
	}
	return(fun.dom)
}

#rw <- c(0,1,0,1)
#x  <- cbind(runif(30), runif(30))
#y  <- fd(x, 15, rw, TRUE)
#T  <- deldir(x[,1], x[,2], rw = rw)
#plot(T, wlines = 've', lty = 1)
#polygon(y, lty = 2, border = "red")