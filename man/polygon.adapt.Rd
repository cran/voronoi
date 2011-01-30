\name{polygon.adapt}
\alias{polygon.adapt}
\alias{find.edge}
\title{
Integral over a convex polygon
}
\description{
Performs Riemann-type integration of a function over a convex polygon.
}
\usage{
polygon.adapt(poly, f, divisions=10, antideriv.x=NULL, antideriv.y=NULL,
   checkPoly=TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{poly}{A 2-column matrix containing the x-y coordinates of the vertices, ordered as they appear on the convex hull.}
  \item{f}{A function taking two numerical arguments that is to be integrated over the specified polygon. This function should return a numerical vector of the same length as the inputs.}
  \item{divisions}{A scalar which controls the number of subintervals over which the Riemann sum is computed. Higher values produce more precise integral calculations.}
  \item{antideriv.x}{A function taking two arguments that is the antiderivative of \code{f} with respect to x.}
  \item{antideriv.y}{A function taking two arguments that is the antiderivative of \code{f} with respect to y.}
  \item{checkPoly}{Whether to check that the polynomial is convex and its points are ordered clockwise or counter-clockwise.}
}
\details{
Specification of one or both of the antiderivatives yields more efficient performance by reducing the dimension in which integration is performed. 
}
\value{
The scalar result of the integration.
}
\author{
Travis A. Gerke and David M. Diez
}
\seealso{
\code{\link{integrate.box}}, \code{\link{centroidal}}
}
\examples{
#=====> Rectangle <=====#
pts <- rbind(c(0,2), c(0,4), c(4,4), c(4,2), c(0,2))
plot(pts, type="l")
f   <- function(x, y) x + y^3
polygon.adapt(poly = pts, f = f)

#=====> Random Convex Polygon <=====#
pts <- cbind(runif(5), runif(5))
pts <- pts[chull(pts),]
plot(pts, type="n")
polygon(pts)
antideriv.x <- function(x, y) x^2/2 + x*y^3
antideriv.y <- function(x, y) x*y + y^4/4
polygon.adapt(poly = pts, f = f, divisions=100,
   antideriv.x = antideriv.x, antideriv.y = antideriv.y)
}
\keyword{ integration }
\keyword{ polygon }