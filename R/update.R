#' Self Update from Github
#'
#' @return
#' @export
#'
#' @examples
self_update <- function(){
  waRRior::print_if_verbose("Initiating self update", T)
  update_packages(waRRior)
  waRRior::print_if_verbose("Done", T)
}
