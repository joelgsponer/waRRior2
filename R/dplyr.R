
# dplyr summary ----------------------------------------------------------
#' Create a summary like dataframe with piping
#'
#' @param data data frame to be summarised
#' @param col_name the columns to be included
#' @col_name column names to be summarises, have to be numeric.
#'
#' @return
#' @export
#'
#' @examples
#' dplyr_summary(mtcars,cyl)
dplyr_summary <- function(data,col) {
  col <- dplyr::enquo(col)
  r <- dplyr::summarise(data
            , Mean = waRRior::mean_noNA_noInf(!! col)
            , Median = waRRior::median_noNA_noInf(!! col)
            , SD = waRRior::sd_noNA_noInf(!! col)
            , Min = waRRior::min_noNA_noInf(!! col)
            , Max = waRRior::max_noNA_noInf(!! col)
            , Missing = sum(is.na(!! col))
  )
  return(r)
}

# dplyr tuple -------------------------------------------------------------
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
# -----------------------------------------------------------------------------
replace_na_all <- function(d = ., replacement, verbose = F, debug = F) {
  replace(d, is.na(d), replacement)
}


