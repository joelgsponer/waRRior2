#' No NA -Inf Max
#' Returns a max value ignoring NAs while calculating the max and replacing
#' -Inf with NA (which occurs if the max is taken from only NA vales while
#' setting na.rm = T)
#' @param x A vector
#'
#' @return Either a Number of NA
#' @export
#'
#' @examples
#' max_noNA_noInf(c(1,2))
#' max_noNA_noInf(c(NA,1))
#' max_noNA_noInf(c(NA,NA))
max_noNA_noInf <- function(x){
  res <- suppressWarnings(max(x, na.rm=T))
  res <- dplyr::if_else(res == -Inf | res == Inf, NA_real_, res)
  return(res)
}
# -------------------------------------------------------------------
#' No NA -Inf Mean
#' Returns a mean value ignoring NAs while calculating the mean and replacing
#' -Inf with NA (which occurs if the mean is taken from only NA vales while
#' setting na.rm = T)
#' @param x A vector
#'
#' @return Either a Number of NA
#' @export
#'
#' @examples
#' mean_noNA_noInf(c(1,2))
#' mean_noNA_noInf(c(NA,1))
#' mean_noNA_noInf(c(NA,NA))
mean_noNA_noInf <- function(x){
  res <- suppressWarnings(mean(x, na.rm=T))
  res <- dplyr::if_else(res == -Inf | res == Inf, NA_real_, res)
  return(res)
}
# -------------------------------------------------------------------
#' No NA -Inf Min
#' Returns a min value ignoring NAs while calculating the min and replacing
#' -Inf with NA (which occurs if the min is taken from only NA vales while
#' setting na.rm = T)
#' @param x A vector
#'
#' @return Either a Number of NA
#' @export
#'
#' @examples
#' min_noNA_noInf(c(1,2))
#' min_noNA_noInf(c(NA,1))
#' min_noNA_noInf(c(NA,NA))
min_noNA_noInf <- function(x){
  res <- suppressWarnings(min(x, na.rm=T))
  res <- dplyr::if_else(res == -Inf | res == Inf, NA_real_, res)
  return(res)
}

