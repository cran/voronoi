# x  = deldir object
# z  = vector with one number per cell (typically 1/area)
#      may also be another estimator of the cell intensity
# rng = range for plotting colors

plot.ve <-
function(x, rng = NULL, cols=NULL, ...) {
   z      <- x$z
   rw     <- x$T$rw
   mats   <- list()
   t.list <- tile.list(x$T)
   for(i in 1:length(t.list)) {
      mats[[i]] <- cbind(t.list[[i]]$x, t.list[[i]]$y)
   }
   #z <- 1/x$summary[,8]
   if(is.null(cols)[1]){
      cols   <- (z - min(z))/diff(range(z))
      if(!is.null(rng)[1]) {
   	     cols   <- (z - min(rng))/diff(range(rng))
      }
      cols <- gray(1 - cols)
   }
   plot(rw[1:2], rw[3:4], type = "n", ...)
   for(i in 1:length(mats)) {
	  polygon(mats[[i]], col = cols[i])
   }
}
