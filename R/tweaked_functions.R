#' No NA -Inf Max
#' Returns a max value ignoring NAs while calculating the max and replacing
#' -Inf with NA (which occurs if the max is taken from only NA vales while
#' setting na.rm = T)
#' @param x
#'
#' @return Either a Number of NA
#' @export
#'
#' @examples
max_noNA_noInf <- function(x){
  res <- suppressWarnings(max(x, na.rm=T))
  res <- if_else(res == -Inf | res == Inf, NA_real_, res)
  return(res)
}
# -------------------------------------------------------------------
#' No NA -Inf Mean
#' Returns a mean value ignoring NAs while calculating the mean and replacing
#' -Inf with NA (which occurs if the mean is taken from only NA vales while
#' setting na.rm = T)
#' @param x
#'
#' @return Either a Number of NA
#' @export
#'
#' @examples
mean_noNA_noInf <- function(x){
  res <- suppressWarnings(mean(x, na.rm=T))
  res <- if_else(res == -Inf | res == Inf, NA_real_, res)
  return(res)
}
# -------------------------------------------------------------------
#' No NA -Inf Min
#' Returns a min value ignoring NAs while calculating the min and replacing
#' -Inf with NA (which occurs if the min is taken from only NA vales while
#' setting na.rm = T)
#' @param x
#'
#' @return Either a Number of NA
#' @export
#'
#' @examples
min_noNA_noInf <- function(x){
  res <- suppressWarnings(min(x, na.rm=T))
  res <- if_else(res == -Inf | res == Inf, NA_real_, res)
  return(res)
}

