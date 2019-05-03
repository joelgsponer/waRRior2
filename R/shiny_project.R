# -----------------------------------------------------------------------------
#' Set up shiny app inside project
#'
#' @param verbose Verbose printing
#' @param debug Verbose printing
#' @export
shiny_setup <- function(verbose = T, debug = F){
  waRRior::print_if_verbose("Setting up shiny", verbose = verbose)
  dir.create("./shiny")
  waRRior::print_if_verbose("Adding symlinks", verbose = verbose)
  waRRior::shiny_symlink()
  waRRior::print_if_verbose("Adding ui", verbose = verbose)
  write("# User interface", file = "./shiny/ui.R")
  waRRior::print_if_verbose("Adding server", verbose = verbose)
  write("# Server\nsource('setup.R')\nsource('ui.R')", file = "./shiny/server.R")
}
# -----------------------------------------------------------------------------
#' Create symlinks for shiny
#' @param file file to linked, if null standard files will be linked
#' @param verbose Verbose printing
#' @export
shiny_symlink <- function(file = NULL, verbose = T){
  if(is.null(file)){
    file.link(SETUP(), "./shiny/setup.R")
    file.link(LIB(), "./shiny/lib.R")
  } else {
    link_file <- sprintf("./shiny/%s", stringr::str_replace(file,"^(.*[\\/])", ""))
    waRRior::print_if_verbose(sprintf("Adding Symlink from %s to %s", file, link_file), verbose = verbose)
    file.link(file, link_file)
  }
}
# -----------------------------------------------------------------------------
