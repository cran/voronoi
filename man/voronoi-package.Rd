\name{voronoi-package}
\alias{voronoi-package}
\alias{voronoi}
\docType{package}
\title{
Methods and applications related to Voronoi tessellations
}
\description{
Methods and applications related to Voronoi tessellations
}
\details{
\tabular{ll}{
Package: \tab voronoi\cr
Type: \tab Package\cr
Version: \tab 1.0\cr
Date: \tab 2011-01-30\cr
License: \tab GPL-2\cr
Depends: \tab R (>= 2.11.1), deldir, spatialkernel, splancs, gpclib\cr
LazyLoad: \tab yes\cr
}
}
\author{
Christopher D. Barr, Travis A. Gerke, David M. Diez

Maintainer: Christopher D. Barr <cdbarr@gmail.com>
}
\references{
C.D Barr and F.P Schoenberg. On the Voronoi Estimator for the Intensity of an Inhomogeneous Planar Poisson Process. Biometrika, 1:1-15, 2010.

Q. Du, V. Faber and M. Gunzburger. Centroidal Voronoi Tesselations: Applications and Algorithms. SIAM Review 41:637-676, 1999.

C.G. Wager, B.A. Coull and N. Lange. Modelling Spatial Intensity for Replicated Inhomogeneous Point Patterns in Brain Imaging. J.R. Statist. Soc. B, 66:429-446, 2004.
}
\keyword{ package }
\seealso{
\code{\link{ve}}, \code{\link{centroidal}}, \code{\link{pattern.sim}}, \code{\link{voronoi.game}}
}
\examples{
#=====> ve <=====#
p  <- pattern.sim(7, type='moon')
t5 <- ve(p, rw=c(0,1,0,1))
plot(t5)
points(t5, cex=0.2)


#=====> centroidal <=====#
f           <- function(x, y) x^4 + y^4
antideriv.x <- function(x, y) x^5/5 + x*y^4
antideriv.y <- function(x, y) y*x^4 + y^5/5
obj         <- deldir(runif(10), runif(10))
c.obj       <- centroidal(x = obj, eps = .005, f = f,
                          antideriv.x = antideriv.x,
                          antideriv.y = antideriv.y,
                          divisions = 10, verbose = 5)
par(mfrow=c(1,2))
plot(obj, wlines="tess", main="Original Tessellation")
plot(c.obj$T, wlines="tess", main="Centroidal Tessellation")


#=====> pattern.sim <=====#
par(mfrow = c(2,2))
plot(pattern.sim(type = "linear"), main = "linear")
plot(pattern.sim(type = "quadratic"), main = "quadratic")
plot(pattern.sim(type = "moon"), main = "moon")
plot(pattern.sim(type = "ridge"), main = "ridge")

for(i in 1:4) {
   plot(pattern.sim(i + 6, type = "moon"), 
     main = paste("r = ", i + 6), pch = 20, cex = 0.2)
}

for(i in 1:4) {
   plot(pattern.sim(i + 6, type = "ridge"), 
     main = paste("r = ", i + 6), pch = 20, cex = 0.2)
}
}
