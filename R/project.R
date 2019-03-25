# Functions to work with projects
# -----------------------------------------------------------------------------
#' Add library to be used in project
#'
#' @param lib The library to be added
#' @param file Where to add
#' @param verbose For verbose printing
#' @param debug For debug printing
#'
#' @export
project_use_library <- function(lib,file = "./src/lib.R", verbose = F, debug = F) {
  lib <- paste0("library(", lib, ")")
  write(lib, file = file, append = TRUE)
}
# -----------------------------------------------------------------------------
#' Setup project structure
#'
#' @param verbose Verbose printing
#' @param debug Debug printing
#' @export
project_setup_structure <- name <- function(verbose = T, debug = F) {
  waRRior::print_if_verbose("Creating folder [src] for source files", verbose = verbose)
  dir.create("./src")
  waRRior::print_if_verbose("Creating folder [raw] for raw data", verbose = verbose)
  dir.create("./raw")
  waRRior::print_if_verbose("Creating folder [data] for processed", verbose = verbose)
  dir.create("./data")
  waRRior::print_if_verbose("Creating folder [img] for plots", verbose = verbose)
  dir.create("./img")
  waRRior::print_if_verbose("Creating folder [tbl] for tables", verbose = verbose)
  dir.create("./tbl")
  waRRior::print_if_verbose("Creating folder [reports] for reports", verbose = verbose)
  dir.create("./reports")
}
# -----------------------------------------------------------------------------
#' Setup files
#'
#' @param verbose Verbose printing
#' @param debug Debug printing
#' @export
project_setup_files <- name <- function(verbose = F, debug = F) {
  write("# Libraries", "./src/lib.R")
  write("# Load libraries", "./src/setup.R")
  write("\n", "./src/setup.R", append = TRUE)
  write("# Set seed", "./src/setup.R", append = TRUE)
  write("set.seed(42)", "./src/setup.R", append = TRUE)
}
# -----------------------------------------------------------------------------
#' Set up a project
#'
#' @param verbose Verbose printing
#' @param debug Debug printing
#' @export
project_create <- name <- function(verbose = F, debug = F) {
  waRRior::project_setup_structure()
  waRRior::project_setup_files()
}
# -----------------------------------------------------------------------------

