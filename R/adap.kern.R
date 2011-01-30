adap.kern <-
function(y, p, lambda, poly = NULL) {
   if(is.null(poly)) {
      g1   <- rep(NA, nrow(p))
      coef <- 1/(2*pi*lambda^2)
      for(i in 1:nrow(p)) {
         dist   <- sqrt((p[i,1] - p[,1])^2 + (p[i,2] - p[,2])^2)
         g1[i]  <- coef*sum(exp(-.5*(dist/lambda)^2))
      }
   } else {
      g1 <- lambdahat(p, lambda, p, poly)$lambda
   }
   c2   <- sqrt(mean(g1)/g1)
   est  <- rep(NA, nrow(y))
   coef <- 1/(2*pi*(lambda^2)*(c2^2))
   for(i in 1:nrow(y)) {
      dist   <- sqrt((y[i,1] - p[,1])^2 + (y[i,2] - p[,2])^2)
      est[i] <- sum(coef*exp(-.5*(dist/(lambda*c2))^2))
   }
   return(est)
}
