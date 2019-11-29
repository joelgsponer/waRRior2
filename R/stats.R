# Compare correlation ----------------------------------------------------
#' Compare two pearson correlation coefficients
#'
#' @param r1 First pearson correlation
#' @param r2
#' @param n1
#' @param n2
#'
#' @return
#' @export
#'
#' @examples
compare_pearson_r <- function(r1, r2, n1, n2){
  fisher.z<- function (r1,r2,n1,n2) (atanh(r1) - atanh(r2)) / ((1/(n1-3))+(1/(n2-3)))^0.5
  return(2*(1-pnorm(abs(fisher.z(r1= r1,r2= r2,n1= n1 ,n2= n2)))))
}

