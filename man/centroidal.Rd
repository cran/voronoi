\name{centroidal}
\alias{centroidal}
\title{
Centroidal Voronoi Diagram
}
\description{
Iteratively searches for the centroidal Voronoi diagram by shifting each point to the weighted centroid of its cell, re-tessellating, shifting each point to its new weighted centroid, and repeating until convergence.
}
\usage{
centroidal(x, eps = .01, max.iter = 100, f = NULL, antideriv.x = NULL,
   antideriv.y = NULL, divisions = 15, rw = NULL, verbose = 0)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{An \code{M}-by-\code{2} matrix or data frame representing a point pattern, or an object of class \code{deldir} or \code{ve}.}
  \item{eps}{The algorithm will stop when the maximum distance moved by any point between iterations is less than \code{eps}.}
  \item{max.iter}{Specifies the maximum number of iterations, provided the \code{eps} criterion is not met.}
  \item{f}{A function taking two arguments over which the centroidal Voronoi diagram is constructed.}
  \item{antideriv.x}{A function taking two arguments that is the antiderivative of \code{f} with respect to its first argument. Specification of \code{antideriv.x} is optional, but will improve efficiency if specified.}
  \item{antideriv.y}{A function taking two arguments that is the antiderivative of \code{f} with respect to its second argument. Specification of \code{antideriv.y} is optional, but will improve efficiency if specified.}
  \item{divisions}{A scalar which controls the number of subintervals over which a Riemann sum is computed. Higher values produce more precise centroid location calculations.}
  \item{rw}{An optional vector of length 4 describing the corners of the rectangular space of the data. The values should be in the order of (xmin, xmax, ymin, ymax).}
  \item{verbose}{If set to an integer value greater than 0, the user will be notified upon the completion of every \code{verbose} iterations.}
}
\details{
The function \code{centroidal} iteratively searches for the centroidal Voronoi diagram by shifting each point to the weighted centroid of its cell, re-tessellating, and shifting each point to its new weighted centroid. If \code{f} is set to \code{NULL}, each location in the space has equal weight, and the resulting centroid calculations merely involve finding the geometric center of any given cell. If \code{f} is specified, each location \eqn{v=(x, y)} has weight assigned by \eqn{f(v)}. Given such weights, calculation of the weighted centroid for cell \eqn{C} proceeds as
\deqn{[\int_C vf(v) dC]/[\int_C f(v) dC],}
which may intuitively be viewed as the center of mass for \eqn{C}. Integrals in the above expression are approximated via \link{polygon.adapt} and \link{integrate.box}. Precision of the approximation can be improved by increasing the value of \code{divisions}. Specification of one or both of \code{antideriv.x} and \code{antideriv.y} may substantially improve efficiency by reducing the dimension in which integration occurs. 
}

\value{
The output object is of class \code{ve}, which itself is a list with the following items:
  \item{p }{A matrix of the points.}
  \item{z }{A vector of the Voronoi estimates at the points.}
  \item{T }{A \code{deldir} object for the points.}
  \item{est }{Set to \code{NULL}. See \code{\link{ve}}.}

Appended to the \code{ve} list are:
  \item{iter}{The number of iterations performed.}
  \item{max.dist}{A vector containing the maximum distance moved by any point at each iteration.}
}
\references{
Q. Du, V. Faber and M. Gunzburger. Centroidal Voronoi Tesselations: Applications and Algorithms. SIAM Review 41:637-676, 1999.

C.G. Wager, B.A. Coull and N. Lange. Modelling Spatial Intensity for Replicated Inhomogeneous Point Patterns in Brain Imaging. J.R. Statist. Soc. B, 66:429-446, 2004.

C.D Barr and F.P Schoenberg. On the Voronoi Estimator for the Intensity of an Inhomogeneous Planar Poisson Process. Biometrika, 1:1-15, 2010.
}
\author{
Travis A. Gerke and Christopher D. Barr
}
\seealso{
\code{\link{polygon.adapt}}, \code{\link{integrate.box}}
}
\examples{
#=====> Uniform Function <=====#
obj   <- deldir(runif(10), runif(10))
c.obj <- centroidal(x = obj, eps = .001)
par(mfrow=c(1,2))
plot(obj, wlines="tess", main="Original Tessellation")
plot(c.obj$T, wlines="tess", main="Centroidal Tessellation")

#=====> Polynomial Function <=====#
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
}
\keyword{ Centroidal Voronoi }
\keyword{ Tessellation }
\keyword{ Centroid }