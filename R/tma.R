# -----------------------------------------------------------------------------
#' tidy missing values
#' Replaces all specified missing values (e.g. "n.a", "x") with proper NA
#' in the specified columns
#' @param tbl The table to be cleaned
#' @param values Values to be replaces (e.g. "n.a", "x")
#' @param columns The columns to be cleaned
#'
#' @return The cleaned table
#' @export
#'
#' @examples
#' fruits <- c("Pear", "Apple", "n.a", "X")
#' veggies <- c("Cauliflower", "X", "Cauliflower", "n.a")
#' d <- data.frame(veggies,fruits)
#' d <- waRRior::tidy_missing_values(d, c("n.a", "X"), c("veggies", "fruits"))
tidy_missing_values <- function(tbl, values, columns){
  fnc_replace <- function(x){replace(x, x %in% values, NA)}
  tbl <- dplyr::mutate_at(tbl, columns, fnc_replace)
  return(tbl)
}
# -------------------------------------------------------------------
#' Tidy column names
#'
#' @param tbl Table where collumn names should be tidied
#' @param pattern What charcters should be removed
#' @param replacement Repclament character
#'
#' @return The cleaned table
#' @export
#'
#' @examples
#' fruits <- c("Pear", "Apple", "n.a", "X")
#' veggies <- c("Cauliflower", "X", "Carrot", "n.a")
#' d <- data.frame(veggies,fruits)
#' d <- waRRior::tidy_missing_values(d, c("n.a", "X"), c("veggies", "fruits"))
tidy_column_names <- function(
  tbl
  , pattern = "[-() /:]"
  , replacement = "_"
){
  colnames(tbl) <- stringr::str_trim(colnames(tbl))
  colnames(tbl) <- stringr::str_replace_all(colnames(tbl), pattern, replacement)
  colnames(tbl) <- stringr::str_replace_all(colnames(tbl), "__", "_")
  return(tbl)
}
# -----------------------------------------------------------------------------
#' Tidy unwanted characters in column
#'
#' @param tbl The table to be cleaned
#' @param value The character to be removed
#' @param colums The columns to be cleaned
#'
#' @return The cleaned table
#' @export
#'
#' @examples
#' fruits <- c("Pear", "Apple", "n.a", "X")
#' veggies <- c("Cauliflower", "X", "Carrot", "n.a")
#' d <- data.frame(veggies,fruits)
#' colnames(d) <- c("() ", "//-")
#' d <- waRRior::tidy_column_names(d)
tidy_unwanted_characters <- function(tbl, value, colums){
  fnc_replace <- function(x){sub(value, "", x)}
  tbl <- dplyr::mutate_at(tbl, colums, fnc_replace)
  return(tbl)
}
# -----------------------------------------------------------------------------
