#' Create a summary like dataframe with piping
#'
#' @param data data frame to be summarised
#' @param col_name the columns to be included
#' @col_name column names to be summarises, have to be numeric.
#' @param verbose for verbose printing
#' @param debug for debug printing
#'
#' @return
#' @export
#'
#' @examples
#' dplyr_summary(mtcars,cyl)
dplyr_summary <- function(data, col_name, verbose = F, debug = F) {
  col_name  <- dplyr::enquo(col_name)
  dplyr::summarise(data, Mean = waRRior::mean_noNA_noInf(!!col_name)
            , Median = waRRior::median_noNA_noInf(!!col_name)
            , SD = waRRior::sd_noNA_noInf(!!col_name)
            , Min = waRRior::min_noNA_noInf(!!col_name)
            , Max = waRRior::max_noNA_noInf(!!col_name)
            , Missing = sum(is.na(!!col_name))
  )
}
# -----------------------------------------------------------------------------
#' Title
#'
#' @param t
#'
#' @return
#' @export
#'
#' @examples
dplyr_tuple <- function( t )
{
  as.list( setNames( t[[2]], t[[1]] ) )
}
