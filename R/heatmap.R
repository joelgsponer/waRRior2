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
  # Column
  if(which == "column"){
    l <- ComplexHeatmap::column_order(heatmap_obj)
    for (i in 1:length(l)){
      if (i == 1) {
        clu <- colnames(matrix_obj[,l[[i]]])
        out <- cbind(clu, i)
        colnames(out) <- c("ID", "Cluster")
      } else {
        clu <- colnames(matrix_obj[,l[[i]]])
        clu <- cbind(clu, i)
        out <- rbind(out, clu)
      }
    }
    out %>%
      as_tibble() ->
      out
    return(out)
  }
  # Row
  if(which == "row"){
    l <- ComplexHeatmap::row_order(heatmap_obj)
    for (i in 1:length(l)){
      if (i == 1) {
        clu <- rownames(matrix_obj[l[[i]],])
        out <- cbind(clu, i)
        colnames(out) <- c("ID", "Cluster")
      } else {
        clu <- rownames(matrix_obj[l[[i]],])
        clu <- cbind(clu, i)
        out <- rbind(out, clu)
      }
    }
    out %>%
      as_tibble() ->
      out
    return(out)
  }
}
