centroidal <-
function(x, eps = .01, max.iter = 100, f = NULL, antideriv.x = NULL, antideriv.y = NULL, divisions = 15, rw = NULL, verbose = 0){
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

   j <- 1
   max.dist <- list()

   if (is.null(f)) {
      repeat {
         t.list <- tile.list(T)
         p.gons <- vector("list", length(t.list))
         for(i in 1:length(t.list)) {
            p.gons[[i]] <- cbind(t.list[[i]]$x, t.list[[i]]$y)
         }
         old.p <- p
         p     <- matrix(NA, length(t.list), 2)
         dist.vec <- matrix(NA, length(t.list), 1)
         for(i in 1:length(t.list)) {
            p1     <- p.gons[[i]]
            p2     <- rbind(p1[-1,], p1[1,])
            P      <- p1[,1] * p2[,2] - p2[,1] * p1[,2]
            a3     <- 3 * sum(P)
            lon    <- sum((p1[,1] + p2[,1]) * P)
            lat    <- sum((p1[,2] + p2[,2]) * P)
            p[i,]  <- cbind(lon, lat) / a3
            dist.vec[i] <- dist(rbind(p[i,], old.p[i,]))
         }
         max.dist[[j]] <- max(dist.vec)
         T  <- deldir(p[,1], p[,2], rw = x$rw)
         if (j >= max.iter | max.dist[[j]] < eps) {
            break
         }
         if (verbose & !(j %% verbose)) {
            cat("Iteration", j, "completed.\n")
            cat("Maximum distance moved by any point:", max.dist[[j]], "(algorithm stops when <=", eps, ")\n\n")
         }
         j <- j + 1
      }
   } else {  
      funcx <- function(x, y) return(x*f(x, y))
      funcy <- function(x, y) return(y*f(x, y))      
      if (is.null(antideriv.x) & is.null(antideriv.y)) {
         innerfunc.xy <- NULL
         innerfunc.yx <- NULL
      } else if(is.null(antideriv.y)) {
         innerfunc.xy <- NULL
         innerfunc.yx <- function(x, y) return(y*antideriv.x(x,y))
      } else if(is.null(antideriv.x)){
         innerfunc.xy <- function(x, y) return(x*antideriv.y(x,y))
         innerfunc.yx <- NULL
      } else {
         innerfunc.xy <- function(x, y) return(x*antideriv.y(x,y))
         innerfunc.yx <- function(x, y) return(y*antideriv.x(x,y))
      }
      
   	  repeat {
         t.list <- tile.list(T)
         p.gons <- vector("list", length=length(t.list))
         for(i in 1:length(t.list)) {
            p.gons[[i]] <- cbind(t.list[[i]]$x, t.list[[i]]$y)
         }
         old.p <- p
         p     <- matrix(NA, length(t.list), 2)
         dist.vec <- matrix(NA, length(t.list), 1)
         for(i in 1:length(t.list)) {
            p1       <- p.gons[[i]]
            p.area   <- polygon.adapt(p1, f, divisions, antideriv.x, antideriv.y, checkPoly=FALSE)
            p.x.area <- polygon.adapt(p1, funcx, divisions, NULL, innerfunc.xy, checkPoly=FALSE)
            p.y.area <- polygon.adapt(p1, funcy, divisions, innerfunc.yx, NULL, checkPoly=FALSE)
            lon      <- p.x.area/p.area
            lat      <- p.y.area/p.area
            p[i,]    <- cbind(lon, lat)
            dist.vec[i] <- dist(rbind(p[i,], old.p[i,]))
         }
         max.dist[[j]] <- max(dist.vec)
         T  <- deldir(p[,1], p[,2], rw = x$rw)
         if (j >= max.iter | max.dist[[j]] < eps) {
            break
         }
         if (verbose & !(j %% verbose)) {
            cat("Iteration", j, "completed.\n")
            cat("Maximum distance moved by any point:", max.dist[[j]], "(algorithm stops when <=", eps, ")\n\n")
         }
         j <- j + 1
      }
   }

   # find in ve.R (needs $p, $z (1/area --- from deldir), $T is deldir)
   out          <- list()
   out$p        <- T$summary[,1:2]
   out$z        <- 1/T$summary[,8]
   out$T        <- T 
   out$est      <- NULL
   class(out)   <- "ve" 
   out$iter     <- j
   out$max.dist <- unlist(max.dist)
   return(out)
}



