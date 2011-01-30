\name{integrate.box}
\alias{integrate.box}
\title{
Integral over a rectangular region
}
\description{
Calculates the integral of a function over a rectangular region in the plane.
}
\usage{
integrate.box(x.lims, y.lims, f = NULL, antideriv.x = NULL,
   antideriv.y = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x.lims}{A vector of length 2 describing the lower and upper limits of integration in the x-direction.}
  \item{y.lims}{A vector of length 2 describing the lower and upper limits of integration in the y-direction.}
  \item{f}{A function taking two arguments that is to be integrated over the specified region.}
  \item{antideriv.x}{A function taking two arguments that is the antiderivative of \code{f} with respect to x.}
  \item{antideriv.y}{A function taking two arguments that is the antiderivative of \code{f} with respect to y.}
}
\details{
Only one of \code{f}, \code{antideriv.x}, or \code{antideriv.y} need be specified. Specification of either of the antiderivatives yields more efficient performance by reducing the dimension in which integration is performed.
}
\value{
The scalar result of the integration.
}
\author{
Travis A. Gerke
}
\seealso{
\code{\link{centroidal}},
\code{\link{polygon.adapt}}
}
\examples{
func <- function(x, y) x + y^3
integrate.box(x.lims = c(0,4), y.lims = c(2,4), f = func)

antideriv.x <- function(x, y) x^2/2 + x*y^3
integrate.box(x.lims = c(0,4), y.lims = c(2,4), antideriv.x = antideriv.x)

antideriv.y <- function(x, y) x*y + y^4/4
integrate.box(x.lims = c(0,4), y.lims = c(2,4), antideriv.y = antideriv.y)
}
\keyword{ Integration }
