#' Verbose printig
#' @description Prints the message to the console if the verbose parameter has
#'   been set to TRUE.
#' @param message The messsage that should be printed in the console
#' @param verbose Prints to console only if T
#'
#' @return
#' @export
#'
#' @examples
#' print_if_verbose("Hello this is a verbose message", T)
print_if_verbose <- function(message, verbose = F){
  if (verbose) {
    cat(crayon::blue("[",format(Sys.time()),"] waRRior > ",message,"\n", sep = ""))
  }
}
# -------------------------------------------------------------------
#' Debug printig
#' @description Prints the message to the console if the debug parameter has
#'   been set to TRUE.
#' @param message The messsage that should be printed in the console
#' @param object The object to be printed to the console
#' @param verbose Prints to console only if T
#'
#' @return
#' @export
#'
#' @examples
#' print_if_verbose("Hello this is a verbose message", T)
print_if_debug <- function(message, object, debug = F){
  if (debug) {
    cat(crayon::bgYellow(sprintf("####Start DEBUG: Message####:%s\n", message)))
    print(object)
    cat(crayon::bgYellow(sprintf("####End####\n", message)))
  }
}
