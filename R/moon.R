
moon <- function(r, x1, y1) {
   R  <- sqrt((x1 - .3)^2 + (y1 - .1)^2) - .2
   
   W1 <- rep(NA, length(x1))
   for(i in 1:length(x1)) {
      temp  <- acos((x1[i] - .5)/(R[i] + .4))
      W1[i] <- 64*max(0, temp*R[i]*exp(-8*R[i]))
   }
   
   W2 <- rep(NA, length(x1))
   for(i in 1:length(x1)) {
      temp  <- (abs(x1[i] + y1[i] - 1.4)/.09)^4
      W2[i] <- max(0, 1 - temp)/.144
   }
   
   W3 <- dnorm(x1, .5, .05)*dnorm(y1, .5, .05)/6
   
   moonL <- (W1 + W2 + W3 + 1)
   mx    <- 15.07757
   ratio <- moonL/mx
   l     <- exp(r)*ratio
   out   <- data.frame(x = x1, y = y1, lambda = l, ratio = ratio)
   return(out)
}
