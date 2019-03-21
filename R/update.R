#' Self Update from Github
#' Self updates the package
#' @export
#'
#' @examples
#' self_update()
self_update <- function(){
  waRRior::print_if_verbose("Initiating self update", T)
  devtools::update_packages("waRRior")
  waRRior::print_if_verbose("Done", T)
}
