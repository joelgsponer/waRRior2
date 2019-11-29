#' Create a tibble with random values
#'
#' @param cols
#' @param rows
#' @param add_id
#' @param all_chr
#' @param verbose
#' @param debug
#'
#' @return
#' @export
#'
#' @examples
#' # Normal distributed values
#' create_tibble(rows = 100, type = "rnorm")
create_tibble <- function(
    cols = 5
  , rows = 100
  , add_id = TRUE
  , type = "runif"
  , verbose = F
  , debug = F
  ) {
  # helper functions
  generate_rnorm <- function(){
    r <- c()
    for(i in 1:cols){
      r <- c(r, rnorm(rows, mean = runif(1), sd = runif(1)))
    }
    return(r)
  }

  # generate random data
  values <- switch (type,
    "runif" = runif(rows * cols),
    "letters" = sample(letters, size = rows * cols, replace = T),
    "rnorm" = generate_rnorm()
  )
  values = matrix(values, nrow = rows, ncol = cols)
  r = tibble::as_tibble(values)
  # add id
  if(add_id){
    id <- sample(LETTERS, size = rows, replace = T)
    r <- dplyr::mutate(r, id = id)
    r <- r[seq(1,rows), c(cols+1, seq(1:cols))]
  }
  # return
  return(r)
}

