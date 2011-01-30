\name{angle}
\alias{angle}
\title{
Angle of a vector
}
\description{
Finds the angle (in radians) of the vector from \code{p1} to \code{p2}.
}
\usage{
angle(p1, p2)
}
\arguments{
  \item{p1}{A vector of length two containing an \code{x} coordinate and a \code{y} coordinate.}
  \item{p2}{A vector of length two containing an \code{x} coordinate and a \code{y} coordinate.}
}
%\details{
%The angle is calculated via the \code{atan} function and handles exceptions.
%}
\value{
A scalar angle in radians.
}
\author{
David M. Diez and Christopher D. Barr
}
\seealso{
\code{\link{fd}}
}
\examples{
show.angle <- function(p1, p2, theta, ...) {
   r      <- as.vector(dist(rbind(p1, p2)))
   thetas <- seq(0, theta, length.out = 100)
   arc    <- cbind(p1[1]+(r/3)*cos(thetas), p1[2]+(r/3)*sin(thetas))
   plot(c(p1[1]-r, p1[1]+r), c(p1[2]-r, p1[2]+r), 
      type = "n", axes = FALSE, ...)
   segments(p1[1], p1[2], p2[1], p2[2], lty = 2, col = gray(.5))
   segments(p1[1], p1[2], p1[1] + r, p1[2], lty = 2, col = gray(.5))
   lines(arc, col = "red", lty = 2)
   points(rbind(p1, p2))
   box()
}

par(mfrow = c(2,2))
for(i in 1:4) {
   p1   <- runif(2, -10, 10)
   p2   <- runif(2, -10, 10)
   an   <- angle(p1, p2)
   main <- paste("Theta = ", round(an, 2))
   show.angle(p1, p2, an, main = main, xlab = "", ylab = "")
}
}
