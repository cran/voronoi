polygon.adapt <- 
function(poly, f, divisions=10, antideriv.x=NULL, antideriv.y=NULL, checkPoly=TRUE) { 
   #need some input checking on poly --- add a checking option (default to FALSE)
   if(checkPoly){
      if(any(poly[1,] != poly[nrow(poly),])){
         poly <- rbind(poly, poly[1,])
      }
      n <- nrow(poly)
      if(n > 4){ # triangles always pass
         ch <- chull(poly)
         if(length(ch) < n-1){
            stop("The points do not form a convex hull.")
         }
         temp <- which.min(ch)
         if(temp > 1){
            ch <- ch[c(temp:(n-1), 1:(temp-1))]
         }
         temp <- sign(diff(ch))
         t1   <- all(temp == 1)
         t2   <- temp[1] == 1 && all(temp[-1] == -1)
         if(!t1 && !t2){
            stop("The points are not ordered clockwise or counter-clockwise.")
         }
      }
   }
   if(!is.function(f)){
   	  g <- function(x, y){ rep(f, length(x)) }
   } else {
      g <- f
   }
   y.lims <- c(min(poly[,2]), max(poly[,2]))
   y.seq <- seq(y.lims[1], y.lims[2], by=(y.lims[2]-y.lims[1])/divisions)
   out <- 0
   for (i in 1:(length(y.seq)-1)) {
   	  y.lims <- c(y.seq[i], y.seq[i+1])
      x.lims <- find.edge(poly, mean(y.lims))
      out <- out + integrate.box(x.lims, y.lims, g, antideriv.x, antideriv.y)
   }
   return(out)
}