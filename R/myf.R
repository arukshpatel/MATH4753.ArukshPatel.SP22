#' myf
#'
#' @param x
#' @param coef
#'
#' @return
#' @export
#'
#' @examples \dontrun{myf(0, coef=tmp$coefficients[,"Estimate"])}
myf = function(x, xk, coef){
  coef[1]+coef[2]*(x) + coef[3]*(x-xk)*(x-xk>0)
}
