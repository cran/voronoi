plot.summaryve <- function(x, col.nout='#225588', pch.nout=19, cex.nout=0.4, col.out='#BB2255', pch.out=1, cex.out=1.1, ...){
	plot(x$ve, ...)
	points(x$ve$p, col=col.nout, pch=pch.nout, cex=cex.nout)
	if(!is.null(x$outliers)[1]){
		points(x$outliers[,1:2], col=col.out,
				pch=pch.out, cex=cex.out)
	} else {
		cat("No potential outliers identified\n")
	}
}
