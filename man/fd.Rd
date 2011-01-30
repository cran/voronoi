\name{fd}
\alias{fd}
\title{
Fundamental domain of a Voronoi cell
}
\description{
Calculates the fundamental domain of a cell corresponding to a selected point.
}
\usage{
fd(x, index, rw = NULL, cut.to.border = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{An \code{M}-by-\code{2} matrix or data frame representing a point pattern, a \code{deldir} object, or a \code{ve} object.}
  \item{index}{A scalar identifying for which cell the fundamental domain is to be computed.}
  \item{rw}{An optional vector of length 4 describing the corners of the rectangular space of the data. The values should be in the order of (xmin, xmax, ymin, ymax).}
  \item{cut.to.border}{A logical argument. If \code{TRUE}, the function restricts the fundamental domain to the window \code{rw} from \code{deldir}.}
}
\details{
For a Voronoi tessellation of any arbitrary point pattern, the shape of each individual cell is unaffected by points lying outside of its fundamental domain (Okabe, 2000). For any location y belonging uniquely to the cell C_{i}, the disk B(y ; ||y - {p_i}||) centered at y will pass through p_i and contain no other elements of the point pattern. The fundamental domain is the union of these disks. 
}
\value{
A matrix of points corresponding to the boundary of the fundamental domain.
}
\references{
Okabe A, Boots B, Sugihara K and Chiu S (2000). Spatial Tessellations. Chichester: Wiley.
}
\author{
Christopher D. Barr
}
\seealso{
\code{\link{angle}}, \code{\link{ve}}
}
\examples{
rw <- c(0,1,0,1)
x  <- cbind(runif(50), runif(50))
y  <- fd(x, 10, rw, TRUE)
T  <- deldir(x[,1], x[,2], rw = rw)
plot(T, wlines = 'tess', lty = 1)
polygon(y, lty = 2, border = "red")
}
\keyword{ Fundamental domain }
