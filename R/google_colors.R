#' Load Google colors
#'
#' @param url the url to the JSON file where the colors are stored
#' @param verbose toggle verbose printing.
#'
#' @return Returns a list with the "google" colors
#' @export
#'
#' @examples
#' g <- google_colors_load()
#' print(g$BlueGrey$palette)
#' barplot(seq(1,length(g$BlueGrey$palette)), col = g$BlueGrey$palette)
google_colors_load <- function(
  url = "https://raw.githubusercontent.com/joelgsponer/waRRior/master/google/colors/waRRior.google.colors.json",
  verbose = T
){
  waRRior::print_if_verbose("loading Google colors", verbose = verbose)
  google_colors <- RJSONIO::fromJSON(RCurl::getURL(url))
  waRRior::print_if_verbose("Google colors loaded", verbose = verbose)
  return(google_colors)
}
# ---------------------------------------------------------------------
#' Google color accents
#'
#' @param level Choose the level of saturation 1-4
#' @param shuffle shuffle the colors takes T or F
#' @param show show plot of the colors
#'
#' @return A vector of colors drawn from the google color pallets
#' @export
#'
#' @examples
#' g <- google_colors_accents(level = 1, show = TRUE)
#' print(g)
google_colors_accents <- function(level = 4, shuffle = F, show = F){
  google_colors <- google_colors_load(verbose = F)
  r <- c()
  for(i in seq(1,length(google_colors))){
    r <- c(r, google_colors[[i]]$accent[level])
  }
  if(shuffle) r <- sample(r)
  if(show){
    graphics::par(mar = c(4,8,4,4))
    graphics::barplot(rep(1,16), col = r, names = paste(seq(1,16), r), las = 2, horiz = T, xaxt = "n")
  }
  return(r)
}
