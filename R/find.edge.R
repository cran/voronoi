find.edge <-
function(poly, y) {
	poly <- rbind(poly, poly[1,])
   	s <- sign(poly[,2] - y)
   	w <- which(diff(s) != 0)
   	line1 <- poly[w[1]+0:1,]
   	line2 <- poly[w[2]+0:1,]
   	x1 <- (line1[1,1]*(y-line1[2,2])-line1[2,1]*(y-line1[1,2]))/(line1[1,2]-line1[2,2])
   	x2 <- (line2[1,1]*(y-line2[2,2])-line2[2,1]*(y-line2[1,2]))/(line2[1,2]-line2[2,2])
   	return(sort(c(x1,x2)))
}
