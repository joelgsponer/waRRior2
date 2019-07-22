#' Impute matrix
#'
#' @param d_mat matrix to be imputed
#' @param col_max_NA maximum number of NA values per column. Columns with more will be removed
#' @param verboseFlag Vebose printing?
#' @param remove_collinear T/F
#' @param diagnostics T/F
#'
#' @return an imputed matrix
#' @export
#'
#' @examples
impute <- function(
    d_mat
  , col_max_NA = 4
  , verboseFlag = F
  , remove_collinear = T
  , diagnostics = T
){
  d_mat %>%
    janitor::remove_empty(c("rows", "cols")) ->
    d_mat

  # removing colums with more than 4 NAs
  d_mat <- d_mat[,apply(d_mat, function(x) sum(is.na(x)), MARGIN = 2) < col_max_NA]
  col_names_remaining <- colnames(d_mat)

  # impute missing values
  imp <- mice::mice(  data = t(d_mat)
              , method = "pmm"
              , printFlag = verboseFlag
              , remove_collinear = remove_collinear
              , diagnostics = diagnostics
              )

  if(verboseFlag) print(imp$loggedEvents)

  # transpose
  d_mat_imp <- t(as.matrix(complete(imp,2)))
  colnames(d_mat_imp) <- col_names_remaining
  return(list(originial = d_mat, imputed = d_mat_imp))
}
