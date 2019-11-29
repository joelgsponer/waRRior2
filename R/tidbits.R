#' WaRRior p-value
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples
waRRior_p <- function(x) {
  s <- symnum(x
              , corr = FALSE
              , na = FALSE
              , legend = FALSE
              , cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1)
              , symbols = c("***", "**", "*", ".", " ")
          )
  return(paste(formatC(x, format = "e", digits = 2), s))
}
