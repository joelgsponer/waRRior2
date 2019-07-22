#' Self Update from Github
#' Self updates the package, acctually it reinstalls it.
#' @export
#'
#' @examples
#' self_update()
self_update <- function(){
  waRRior::print_if_verbose("Initiating self update", T)
  install_github("joelgsponer/warrior2")
  waRRior::print_if_verbose("Done", T)
}
