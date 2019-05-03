# Functions to work with projects
# -----------------------------------------------------------------------------
#' Run the entire project
#' @export
project_run <- function(){
  source(RUN())
}
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
  lib <- paste0("require(", lib, ")")
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
  waRRior::print_if_verbose("Creating folder [data] for processed data ready for analysis", verbose = verbose)
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
  # Libraries
  write("# Libraries", LIB())
  # Setup
  write("# Load libraries", SETUP())
  write("\n", SETUP(), append = TRUE)
  write("# Set seed", SETUP(), append = TRUE)
  write("set.seed(42)", SETUP(), append = TRUE)
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
#' Add a new src file
#' This file will automatically be soruced and added to the setup file
#' @param name name of the file and function (prefixed with 'src_')
#' @param verbose Verbose printing
#' @param debug Debug printing
#'
#' @return
#' @export
project_add_src <- function(name, verbose = F, debug = F){
  write(  x = sprintf("src_%s <- function(verbose = T, debug = F){\n}", name)
        , file = sprintf("./src/%s.R", name))
  write(x = sprintf("source('./src/%s.R')", name), file = SETUP(), append = T)
  write(x = sprintf("src_%s()", name), file = RUN(), append = T)
}
# -----------------------------------------------------------------------------
#' DATA
#' Wrapper for data path
#' @param name name of the file
#' @param verbose Verbose printing
#' @export
DATA <- function(name, verbose = T){
  paste0("./data/", name)
}
# -----------------------------------------------------------------------------
#' IMG
#' Wrapper for img path
#' @param name Name of the file
#' @param verbose Verbose printing (logical)
#' @export
IMG <- function(name, verbose = T){
  paste0("./img/", name)
}
# -----------------------------------------------------------------------------
#' TBL
#' Wrapper for tbl path
#' @param name Name of the file
#' @param verbose Verbose printing (logical)
#' @export
TBL <- function(name, verbose = T){
  paste0("./tbl/", name)
}
# -----------------------------------------------------------------------------
#' SRC
#' Wrapper for src path
#' @param name Name of the file
#' @param verbose Verbose printing (logical)
#' @export
SRC <- function(name, verbose = T){
  paste0("./src/", name)
}
# -----------------------------------------------------------------------------
#' RUN file
#' @export
RUN <- function(){
  "./src/run.R"
}
# -----------------------------------------------------------------------------
#' SETUP file
#' @export
SETUP <- function(){
  "./src/setup.R"
}
# -----------------------------------------------------------------------------
#' LIB file
#' @export
LIB <- function(){
  "./src/lib.R"
}
# -----------------------------------------------------------------------------


