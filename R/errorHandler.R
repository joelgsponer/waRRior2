#' Error Hanlder
#'
#' @param x Expression to be evaluated
#' @param abort_on_error should the execution be stoped upon an error (raises a proper Error)
#' @param interactive should you be prompted to enter an interactive browser session
#'
#' @export
#'
#' @examples
#' errorHandler(prin("a"))
#' errorHandler(prin(a))
errorWarrior <- function(x, abort_on_error = F, interactive = F){
  Yes <- function(){
    browser()
    return(T)
  }
  No <- function(){
    return(T)
  }
  invalidResponse <- function(){
    message(crayon::yellow("Please give a valid response y or n") )
    return(F)
  }

  withCallingHandlers({
    x
  }, error = function(e){
    message(crayon::bgRed(crayon::white("!! waRRior detected an Error:")))
    message("---")
    message(e)
    message("\n---")
    if(interactive){
      valid_res = F
      while(!valid_res){
        res <- readline("Do you want to open an interactive browser session (y/n): ")
        valid_res <- switch(res,
                              Y = Yes()
                            , y = Yes()
                            , N = No()
                            , n = No()
                            , invalidResponse())
      }
    }
    if(abort_on_error){
      message("aborting further execution!")
      stop(e)
    }
  })
}
