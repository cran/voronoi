
ridge <- function(r, x1, y1) {
   ridgeL <- dnorm(x1, .3 - .2*y1, .02) + .2
   mx    <- 20.14711
   ratio <- ridgeL/mx
   l     <- exp(r)*ratio
   out   <- data.frame(x = x1, y = y1, lambda = l, ratio = ratio)
   return(out)
}
