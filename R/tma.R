#' tidy missing values
#'
#' @param tbl
#' @param values
#' @param colums
#'
#' @return
#' @export
#'
#' @examples
tidy_missing_values <- function(tbl, values, colums){
  fnc_replace <- function(x){replace(x, x %in% values, NA)}
  tbl <- mutate_at(tbl, colums, fnc_replace)
  return(tbl)
}
# -------------------------------------------------------------------
#' Tidy column names
#'
#' @param tbl
#' @param pattern
#' @param replacement
#'
#' @return
#' @export
#'
#' @examples
tidy_column_names <- function(
  tbl
  , pattern = "[-() /:]"
  , replacement = "_"
){
  colnames(tbl) <- str_replace_all(colnames(tbl), pattern, replacement)
  colnames(tbl) <- str_replace_all(colnames(tbl), "__", "_")
  return(tbl)
}
# -------------------------------------------------------------------
#' Tidy unwanted characters
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples
tidy_unwanted_characters <- function(tbl, value, colums){
  fnc_replace <- function(x){sub(value, "", x)}
  tbl <- mutate_at(tbl, colums, fnc_replace)
  return(tbl)
}
