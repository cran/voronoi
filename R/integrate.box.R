integrate.box <- 
function(x.lims, y.lims, f = NULL, antideriv.x = NULL, antideriv.y = NULL) {
   if (is.null(antideriv.x) & is.null(antideriv.y)) {
      int.func <- function(t) {
         vapply(t, function(t) integrate(function(s) f(s,t), x.lims[1], x.lims[2])$value, 0)
      }
      return(integrate(int.func, y.lims[1], y.lims[2])$value)
   } else if(is.null(antideriv.y)) {
      int.func <- function(t) return(antideriv.x(x.lims[2], t) - antideriv.x(x.lims[1], t))
      return(integrate(int.func, y.lims[1], y.lims[2])$value)
   } else {
      int.func <- function(t) return(antideriv.y(t, y.lims[2]) - antideriv.y(t, y.lims[1]))
      return(integrate(int.func, x.lims[1], x.lims[2])$value)
   }
}
