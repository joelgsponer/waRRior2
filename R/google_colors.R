#' Load Google colors
#'
#' @param url
#'
#' @return
#' @export
#'
#' @examples
google_colors_load <- function(
  url = "https://raw.githubusercontent.com/joelgsponer/waRRior/master/google/colors/waRRior.google.colors.json"
){
  waRRior::print_if_verbose("loading google.colors.", verbose = T)
  google_colors <- RJSONIO::fromJSON(getURL(url))
  assign("google_colors", google_colors, envir = .GlobalEnv)
  waRRior::print_if_verbose("google_colors loaded.", verbose = T)
}
# ---------------------------------------------------------------------
#' Google color accents
#'
#' @param level
#' @param shuffle
#' @param show
#'
#' @return
#' @export
#'
#' @examples
google_colors_accents <- function(level = 4, shuffle = F, show = F){
  if(!exists("google.colors")) google_colors_load()
  r <- c()
  for(i in seq(1,length(google_colors))){
    r <- c(r, google_colors[[i]]$accent[level])
  }
  if(shuffle) r <- sample(r)
  if(show){
    par(mar = c(4,8,4,4))
    barplot(rep(1,16), col = r, names = paste(seq(1,16), r), las = 2, horiz = T, xaxt = "n")
  }
  return(r)
}
