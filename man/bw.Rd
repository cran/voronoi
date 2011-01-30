\name{bw}
\alias{bw}
\title{
Bandwidth for 2-d kernel estimation
}
\description{
Calculates a fixed bandwidth parameter for intensity estimation (with gaussian kernel) using one of two options.
}
\usage{
bw(x, method = c("pythagoras", "cross-validation"),
     h=NULL, poly=NULL, extra=FALSE)
}
\arguments{
  \item{x}{An \code{M}-by-\code{2} matrix or data frame representing a point pattern, a \code{deldir} object, or a \code{ve} object.}
  \item{method}{The default is \code{"pythagoras"}, which uses Silverman's rule in each dimension separately, then computes the square root of the sum of squared bandwidths. The \code{"cross-validation"} method evaluates a set of proposed bandwidths (\code{h}).}
  \item{h}{A vector of bandwidths that are to be attempted using the cross-validation method.}
  \item{poly}{A matrix specifying the coordinates of a polygon boundary, where the vertices are in an anti-clockwise order. This argument is used only with the cross-validation method.}
  \item{extra}{Output the evaluation of the bandwidths when using the cross-validation method.}
}
\details{
Under the cross-validation approach, \code{bw} tests bandwidths provided by the user. Each bandwidth is evaluated by considering the pseudo-log-likelihood (up to the addition of a constant):
\deqn{
log-likelihood_{h} = sum_{i=1}^{n} K_{h, -i}(p_{i})
}
where \eqn{n} is the number of points. The expression \eqn{K_{h,-i}(p_{i})} represents the kernel computed at point \eqn{p_{i}} using all points except \eqn{p_{i}}. The bandwidth that maximizes the log-likelihood expression above is selected as the optimal bandwidth.
}
\value{
A scalar bandwidth.

If \code{extra=TRUE} and the cross-validation method is being used, then a list is returned. The first item in the list is a matrix providing the bandwidths paired with their computed negative pseudo-log-likelihoods. The second item in the list is the selected bandwidth.
}
\references{
Silverman BW. Density Estimation for Statistics and Data Analysis. London: Chapman and Hall, 1986.
}
\author{
Christopher D. Barr
}
\seealso{
\code{\link{adap.kern}}
}
\examples{
par(mfrow = c(2,2))
type <- c("homogeneous", "linear", "quadratic", "moon", "ridge")[1]

#=====> Create point pattern <=====#
gon  <- cbind(c(0,1,1,0), c(0,0,1,1))
r    <- 7
p    <- pattern.sim(r, type = type)
plot(p, main = "points")

#=====> Create grid and plot truth <=====#
n    <- 75
gd   <- seq(0, 1, len = n)
y    <- as.matrix(expand.grid(gd, gd))
z0   <- pattern.sim(r, type = type, pts = y)[,3]
m0   <- matrix(z0, n, n)
grsc <- grey(seq(1, 0, len = 1500))
image(m0, col = grsc, axes = FALSE, main = "truth")
box()

#=====> BW using Silverman's rule <=====#
sBW  <- bw(p)
z1   <- lambdahat(p, sBW, y, gon)$lambda
m1   <- matrix(z1, n, n)
image(m1, col = grsc, axes = FALSE, main = "Silverman's bandwidth")
box()
mtext(round(sBW, 4), 3)

#=====> BW using cross-validation <=====#
h    <- seq(sBW/10, 3*sBW, sBW/10)
cvBW <- bw(p, 'c', h, gon)
z2   <- lambdahat(p, cvBW, y, gon)$lambda
m2   <- matrix(z2, n, n)
image(m2, col = grsc, axes = FALSE, main = "cross-validation bandwidth")
box()
mtext(round(cvBW, 4), 3)

#=====> Detailed output for c-v case <=====#
cvBW.extra <- bw(p, 'c', h, gon, TRUE)
}
\keyword{ Silverman }
\keyword{ bandwidth }
