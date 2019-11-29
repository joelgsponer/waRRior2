#' Extract the clusters from a ComplexHeatmap
#'
#' @param heatmap_obj a Heatmap object
#' @param matrix_obj the matrix that was used to create the heatmap, needs col and or row names
#' @param which row or column cluster
#'
#' @return
#' @export
#'
#' @examples
#' mat = matrix(rnorm(80, 2), 8, 10)
#' mat = rbind(mat, matrix(rnorm(40, -2), 4, 10))
#' mat = matrix(rnorm(80, 2), 8, 10)
#' mat = rbind(mat, matrix(rnorm(40, -2), 4, 10))
#' rownames(mat) = letters[1:12]
#' colnames(mat) = letters[1:10]
#' HM <- ComplexHeatmap::Heatmap(mat, km=3)  #Make a heatmap, and have 3 clusters
#' HM <- ComplexHeatmap::draw(HM)  #Show the heatmap
#' heatmap_extract_cluster(HM, mat, which = "row")
heatmap_extract_cluster <- function(heatmap_obj, matrix_obj, which = "column"){
  if(which == "column") c <- ComplexHeatmap::column_order(heatmap_obj)
  if(which == "row") c <- ComplexHeatmap::row_order(heatmap_obj)
  n <- names(c)
  l <- seq(1, length(n), 1)
  clu <- purrr::map2(c, l, function(x, y){
    data.frame(x, y)
  })
  clu <- dplyr::bind_rows(clu)
  clu <- dplyr::arrange(clu, x)
  clu <- dplyr::mutate(clu, y = as.character(y))
  if(which == "column") r <- tibble::tibble(ID = colnames(matrix_obj), Cluster = clu$y)
  if(which == "row") r <- tibble::tibble(ID = rownames(matrix_obj), Cluster = clu$y)
  return(r)
}
