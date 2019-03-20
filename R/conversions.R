#' Reverse factor levels
#'
#' @param x
#'
#' @return vector as a factor
#' @export
#'
#' @examples
reverse_factor_levels <- function(x) {
  return(factor(x, levels = rev(levels(factor(x)))))
}
