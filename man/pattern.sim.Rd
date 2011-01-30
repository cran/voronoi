\name{pattern.sim}
\alias{pattern.sim}
\alias{moon}
\alias{ridge}
\title{
Simulate an inhomogeneous Poisson process
}
\description{
Draw simulations from one of five pre-defined Poisson point process models. Parameters for thre of the models - \code{'homogeneous'}, \code{'linear'} and \code{'quadratic'} - can be adjusted by the user. For the two other models - \code{'moon'}, and \code{'ridge'} - the user may specify the maximum intensity, but cannot change any other features of the model. All simulations are on the unit square.
}
\usage{
pattern.sim(r=NULL, a=NULL, b=NULL, type=c('homogeneous', 'linear', 
   'quadratic', 'moon', 'ridge'), pts=NULL)
}
\arguments{
  \item{r}{The maximum intensity of the process, specified on the log scale.}
  \item{a}{Parameter used to control the increasing intensity from left to right when \code{type} is \code{'linear'}. When \code{type='quadratic'}, controls the how strongly peaked the intensity is from left to right.}
  \item{b}{Parameter used to control the increasing intensity from bottom to top when \code{type} is \code{'linear'}. When \code{type='quadratic'}, controls the how strongly peaked the intensity is from bottom to top.}
  \item{type}{One of five Poisson point process models: \code{"homogeneous"}, \code{"linear"}, \code{"quadratic"}, \code{"moon"}, or \code{"ridge"}.}
  \item{pts}{An \code{m}-by-\code{2} matrix of points, where column 1 represents the horizontal location of the points and column 2 the vertical location. If \code{pts} is specified, \code{pattern.sim} returns an \code{m}-by-\code{3} matrix; the first two columns are identical to the input \code{pts} object and the third column is the intensity at those locations for the model.}
}
\details{
Point patterns are simulated by first drawing a sample from a homogeneous Poisson point process with intensity equal to the user-specified maximum, then thinning according to the desired model. The models for \code{type='linear'} and \code{'quadratic'} are actually log-linear and log-quadratic. The \code{'ridge'} model includes a tall ridge along the western side of the unit square, similar to that used by Barr and Schoenberg (2011). The \code{'moon'} type model includes a ridge, peak and crescent, similar to that used by Heikkinen and Arjas (1998) and Barr and Schoenberg (2011).
}
\value{
An \code{n}-by-\code{2} matrix. Each row represents a point, and each column represents a dimension. If \code{pts} is specified, then an \code{m}-by-\code{3} matrix; the first two columns are identical to the input \code{pts} object and the third column is the intensity at those locations for the model.
}
\references{
Barr CD and Schoenberg FP (2011). On the Voronoi estimator for the intensity of an inhomogeneous planar Poisson process. Biometrika, 97(4), 977-984.

Heikkinen J and Arjas E (1998). Non-parametric Bayesian estimation of a spatial Poisson intensity. Scandinavian Journal of Statistics, 25, 435-50.
}
\author{
Christopher D. Barr
}
\seealso{
\code{\link{ve}}, \code{\link{centroidal}}, \code{\link{bw}}
}
\examples{
par(mfrow = c(2,2))
plot(pattern.sim(type = "linear"), main = "linear")
plot(pattern.sim(type = "quadratic"), main = "quadratic")
plot(pattern.sim(type = "moon"), main = "moon")
plot(pattern.sim(type = "ridge"), main = "ridge")

for(i in 1:4) {
   plot(pattern.sim(a = 2*i - 1, b = 11 - 2*i, type = "linear"), 
     main = paste("a = ", 2*i - 1, ", ", "b = ", 11 - 2*i),
     pch = 20, cex = 0.2)
}


for(i in 1:4) {
   plot(pattern.sim(a = 2*i - 1, b = 11 - 2*i, type = "quadratic"), 
     main = paste("a = ", 2*i - 1, ", ", "b = ", 11 - 2*i),
     pch = 20, cex = 0.2)
}

for(i in 1:4) {
   plot(pattern.sim(i + 6, type = "moon"), 
     main = paste("r = ", i + 6), pch = 20, cex = 0.2)
}

for(i in 1:4) {
   plot(pattern.sim(i + 6, type = "ridge"), 
     main = paste("r = ", i + 6), pch = 20, cex = 0.2)
}

n      <- 101
x      <- seq(0, 1, length = n)
y      <- seq(0, 1, length = n)
xy     <- expand.grid(x, y)
r      <- 7
a1     <- pattern.sim(r, type = "linear", pts = xy)
a2     <- pattern.sim(r, type = "quadratic", pts = xy)
a3     <- pattern.sim(r, type = "moon", pts = xy)
a4     <- pattern.sim(r, type = "ridge", pts = xy)
cols   <- gray(seq(1, 0, len = 1500))

image(matrix(a1[,3], n, n), col = cols, main = "linear")
image(matrix(a2[,3], n, n), col = cols, main = "quadratic")
image(matrix(a3[,3], n, n), col = cols, main = "moon")
image(matrix(a4[,3], n, n), col = cols, main = "ridge")
}
