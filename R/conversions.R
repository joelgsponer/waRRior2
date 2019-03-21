#' Reverse factor levels
#'
#' @param x A vector of factors
#'
#' @return vector as a factor
#' @export
#'
#' @examples
#' a <- factor(seq(1,10))
reverse_factor_levels <- function(x) {
  return(factor(x, levels = rev(levels(factor(x)))))
}
