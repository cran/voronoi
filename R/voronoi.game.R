voronoi.game <-
function(rounds) {
	if(!is.numeric(rounds) | rounds < 0.500001){
		stop("The number of rounds is not valid")
	}
	rounds <- round(rounds)
	xt <- c()
	yt <- c()
	old.par <- par(no.readonly = TRUE)
	on.exit(par(old.par))
	par(mar = rep(3.5,4))
	plot(c(0,1), c(0,1), type = "n", xlab = "", ylab = "")
	title(main = "\n Voronoi Game")
	grid()
	for(i in 1:rounds) {
		for(j in 1:2) {
			abline(h = c(0,1), v = c(0,1))
			xy <- locator(1)
			xt <- c(xt, xy$x)
			yt <- c(yt, xy$y)
			z <- deldir(xt, yt, rw = c(0, 1, 0, 1))
			plot(z, wline = 'tess', pch = rep(20,4), lty = 1, cex = .001, xlab = "", ylab = "")
			title(main = "\n Voronoi Game")
			grid()
			points(xt, yt, pch = rep(19,4), col = c("blue", "red"))
			text(xt - .012, yt + .012, 1:((i - 1)*2 + j), col = c("blue", "red"))
			box()
			if((i == 1) & (j == 1)) {
				pt <- 1.0000
			} else {
				p <- z$summary[,8]
				blue <- seq(1, 2*i, 2)
				pt <- sum(p[blue])
			}
			text(.29, 1.02, paste(pt), col = "blue")
			text(.71, 1.02, paste(1 - pt), col = "red")
			if((i == rounds) & (j == 2)) {
				abline(h = c(0,1), v = c(0,1))
			}
		}
	}
}

