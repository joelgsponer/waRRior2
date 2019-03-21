# -----------------------------------------------------------------------------
#' Creates a section
#' @description To be uses as a snippet
#' @param title Section heading
#' @param description Section description
#' @param ts Add timestamp of creation (T/F)
#' @param verbose For verbose printing
#' @param debug For debug printing
#'
#' @return A section comment
#' @export
#'
#' @examples
#' snippet_section(title = "1st Chapter",
#'  description = "Let the saga begin"
#'  , ts = TRUE, verbose = TRUE)
snippet_section <- function(title, description, ts, verbose = F, debug = F) {
  start <- paste(title, "[START]")
  description <- paste("## ",dplyr::if_else(ts, as.character(Sys.time()), ""), description)
  end <- paste(title, "[END]")
  start <- waRRior::snippet_full_line_title( input = start
                                         , alignment = 40
                                         , filling = "-")
  end <- waRRior::snippet_full_line_title( input = end
                                             , alignment = 38
                                             , filling = "-")
  res <- paste(start,description,"\n\n",end, sep = "\n")
  waRRior::print_if_verbose(paste0("\n",res), verbose = verbose)
  return(res)
}
# -----------------------------------------------------------------------------
#' Generates a 80 chr sequence to be used in documentation
#'
#' @param input Central input, e.g. title
#' @param filling Chr to be used as filler
#' @param alignment Lower numbers more to the left
#' @param verbose For verbose printing
#' @param debug For Debuging
#'
#' @return A string of 80 chr
#' @export
#'
#' @examples
#' snippet_full_line_title(input = "Hello", filling = "-", alignment = 40)
snippet_full_line_title <- function(input,filling,alignment, verbose = F, debug = F) {
  l <- stringr::str_length(input)
  waRRior::print_if_debug("Length of String",l, debug = debug)
  a <- alignment - round(l/2,0)
  if(a < 0) a <- 0
  waRRior::print_if_debug("Alignment",a, debug = debug)
  s <- paste0( "# "
               , paste0(rep(filling, a), collapse = "")
               , " "
               , input
               , " "
               , collapse="")
  ll <- stringr::str_length(s)
  #In case alignment is to far to the right decrease alignemt until it fits
  while(ll>80){
    waRRior::print_if_debug("Alignment",a, debug = debug)
    s <- paste0( "# "
                 , paste0(rep(filling, a), collapse = "")
                 , " "
                 , input
                 , " "
                 , collapse="")
    ll <- stringr::str_length(s)
    a <- a - 1
  }
  paste0(s, paste0(rep(filling, 80-ll), collapse=""))
}
