# -----------------------------------------------------------------------------
#' Categorical cutoff
#' Determines the opital cutoff for groups with numerical data according to
#' a chisquared statistics
#' @param makers the different markers to be analyses
#' @param values the values of the markers
#' @param strata  the grouping
#' @param sig significance level (Default = 0.05)
#' @param minN minimun number per group (Default = 5)
#'
#' @return
#' @export
#'
#' @examples
#' markers <- sample(letters[1:3], 100, replace = T)
#' values <- c(rnorm(50), rnorm(50, mean = 3))
#' strata <- c(rep("A", 50), rep("B", 50))
#' categorical_cutoff(markers, values, strata)
categorical_cutoff <- function(markers, values, strata, sig = 0.05, minN = 5, ...){
  errorWarrior({
    # Split dataframe
    d <- dplyr::tibble(markers, values, strata)
    d <- dplyr::arrange(d, markers)
    # saving marker names for later use
    names_for_list <- unique(d$markers)
    d <- dplyr::group_by(d, markers)
    d <- dplyr::group_split(d)
    # Map calculation of cutoffs
    r <- purrr::map(d, function(d){
      r <- tibble::tibble()
      cutoffs <- sort(unique(d$values))
      # Map grouping
      r <- purrr::map(cutoffs, function(x){
        d <- dplyr::mutate(d, group = dplyr::case_when(
            values >= x ~ TRUE
          , values < x ~ FALSE
        ))
        t <- infer::chisq_test(d, group ~ strata)
        r <- cbind(cutoff = x, t
                   , Npos = as.numeric(dplyr::tally(dplyr::filter(d, group == T)))
                   , Nneg = as.numeric(dplyr::tally(dplyr::filter(d, group == F)))
              )
        return(r)
      })
      # collecting
      r <- do.call(rbind.data.frame, r)
      r <- dplyr::arrange(r, p_value)
      #r <- dplyr::filter(r, p_value <= sig, Npos >= minN, Nneg >= minN)
      return(r)
    })
    names(r) <- names_for_list
    return(r)
  }, ...)
}
# -----------------------------------------------------------------------------

