\name{ve}
\alias{ve}
\alias{plot.ve}
\alias{points.ve}
\alias{summary.ve}
\alias{plot.summaryve}
\alias{print.summaryve}
\title{
Voronoi intensity estimation
}
\description{
Obtain the Voronoi intensity estimate for a point pattern.
}
\usage{
ve(x, rw=NULL, locations=NULL)
}
\arguments{
  \item{x}{An \code{M}-by-\code{2} matrix or data frame representing a point pattern, a \code{deldir} object, or a \code{ve} object.}
  \item{rw}{An optional vector of length 4 describing the corners of the rectangular space of the data. The values should be in the order of \code{(xmin, xmax, ymin, ymax)}. The default window is 20 percent larger than the range in each dimension -- 10 percent below and 10 percent above the minimum and maximum. This argument is only utilized when \code{x} is a matrix or data frame. }
  \item{locations}{A \code{N}-by-\code{2} matrix of locations to identify the Voronoi estimate.}
}
\details{
The intensity estimate is the inverse-area of each Voronoi cell.
}
\value{
The output object is of class \code{""}, which itself is a list with the following items:
  \item{p }{A matrix of the points.}
  \item{z }{A vector of the Voronoi estimates at the points.}
  \item{T }{A \code{deldir} object for the points.}
  \item{rw }{The rectangular space used for the data in the order of \code{(xmin, xmax, ymin, ymax)}.}
  \item{est }{A matrix with three columns, where the first two columns represent the argument \code{locations} and the third column is the Voronoi estimates at those locations.}
}
\references{
C.D Barr and F.P Schoenberg. On the Voronoi Estimator for the Intensity of an Inhomogeneous Planar Poisson Process. Biometrika, 1:1-15, 2010.
}
\author{
Christopher D. Barr, Travis A. Gerke, David M. Diez
}
\seealso{
\code{\link{centroidal}}, \code{\link{fd}}
}
\examples{
#=====> Equivalent Constructions <=====#
x <- rnorm(50, 1)
y <- rnorm(50, 6, 3)
d <- deldir(x, y)
t1 <- ve(cbind(x,y))
t2 <- ve(d)
identical(t1,t2)
par(mfrow=c(1,2))
plot(t1)
plot(t2)
points(t2)

#=====> Four Simulated Patterns <=====#
par(mfrow=c(2,2))

x  <- rnorm(100)
y  <- rnorm(100)
d1 <- deldir(x,y)
t3 <- ve(d1)
temp <- summary(t3)
plot(temp)

p    <- pattern.sim(6.5, type='linear')
t4   <- ve(p, rw=c(0,1,0,1))
st4  <- summary(t4)
plot(st4)

p  <- pattern.sim(6.5, type='moon')
t5 <- ve(p, rw=c(0,1,0,1))
plot(t5)
points(t5, cex=0.2)

p   <- pattern.sim(6.5, type='ridge')
t6  <- ve(p, rw=c(0,1,0,1))
plot(t6)
points(t6, cex=0.2)
}
\keyword{ Voronoi estimator }
\keyword{ intensity estimation }
\keyword{ Voronoi }
\keyword{ tesselation }
