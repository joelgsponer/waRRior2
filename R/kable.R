#' Title
#'
#' @param h
#' @param path
#' @param style
#'
#' @return
#' @export
#'
#' @examples
kable_styling_with_css <- function(h = ., path, style = NULL){
  x <- as.character(shiny::HTML('<link rel="stylesheet" type="text/css" href="style.css">'))
  print(x)
  write(  x = x
        , file = path
        , append = F)
  write(  x = "\n\n"
        , file = path
        , append = TRUE)
  write(  x = h
        , file = path
        , append = TRUE)
}
