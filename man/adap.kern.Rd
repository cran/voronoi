\name{adap.kern}
\alias{adap.kern}
\title{
Kernel estimation using adaptive bandwidth
}
\description{
Estimates the intensity of a point pattern using a gaussian kernel with spatially adaptive bandwidth.
}
\usage{
adap.kern(y, p, lambda, poly = NULL)
}
\arguments{
  \item{y}{A matrix of locations at which intensity is to be estimated.}
  \item{p}{A matrix of locations of observed points.}
  \item{lambda}{An initial (global) bandwidth.}
  \item{poly}{A matrix containing the \code{x} and \code{y} coordinates of the vertices of the polygon surrounding the space. If \code{NULL}, then a rectangle is used.}
}
\value{
A vector of intensity estimates for locations \code{x}.
}
\references{
Stock C and Smith EGC. Adaptive Kernel Estimation and Continuous Probability Representation of Historical Earthquake Catalogs. Bulletin of the Seismological Society of America. Vol 92, No 3, p 904-912.
}
\author{
Christopher D. Barr
}
\seealso{
\code{\link{bw}}, \code{\link{pattern.sim}}
}
\examples{

par(mfrow = c(2,2))

#=====> Create point pattern <=====#
gon  <- cbind(c(0,1,1,0), c(0,0,1,1))
r    <- 8
p    <- pattern.sim(r, type = "moon")
plot(p, main = "points")

#=====> Create grid and plot truth <=====#
n    <- 75
gd   <- seq(0, 1, len = n)
y    <- as.matrix(expand.grid(gd, gd))
z0   <- pattern.sim(r, type = "moon", pts = y)[,3]
m0   <- matrix(z0, n, n)
grsc <- grey(seq(1, 0, len = 1500))
image(m0, col = grsc, axes = FALSE, main = "truth")
box()

#=====> Adaptive kernel <=====#
l    <- bw(p)
z1   <- adap.kern(y, p, l, gon)
m1   <- matrix(z1, n, n)
image(m1, col = grsc, axes = FALSE, main = "adaptive kernel")
box()

#=====> Fixed kernel <=====#
z2   <- lambdahat(p, l, y, gon)$lambda
m2   <- matrix(z2, n, n)
image(m2, col = grsc, axes = FALSE, main = "fixed kernel")
box()
}