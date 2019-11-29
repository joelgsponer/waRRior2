#' Title
#'
#' @param waRRior_heatmap
#'
#' @return
#' @export
#'
#' @examples
convert_to_tibble <- function(waRRior_heatmap){
  m <- t(waRRior_heatmap[['matrix']])
  #save IDs for later
  ids <- rownames(m)
  t <- tibble::as_tibble(m)
  t <- mutate(t, ID = ids)
  t <- dplyr::inner_join(t, waRRior_heatmap[['column_clusters']])
  t <- select(t, ID, Cluster, dplyr::everything())
  return(t)
}
# ----
#' Title
#'
#' @param waRRior_heatmap
#'
#' @return
#' @export
#'
#' @examples
convert_to_matrix <- function(waRRior_heatmap){
  t <- waRRior::convert_to_tibble(waRRior_heatmap)
  t <- dplyr::mutate(t, Cluster = as.numeric(Cluster))
  m <- as.matrix(dplyr::select(t, -ID))
  rownames(m) <- t$ID
  return(m)
}
# ---
#' Title
#'
#' @param waRRior_heatmap
#' @param path
#'
#' @return
#' @export
#'
#' @examples
distribution_among_clusters <- function(waRRior_heatmap, path = NULL){
  t <- waRRior::convert_to_tibble(waRRior_heatmap)
  t <- gather(t, key = "key", value = value, -ID, -Cluster)
  g <- ggplot2::ggplot(t, ggplot2::aes(x = as.factor(Cluster), y = value, fill = key)) +
    ggplot2::geom_violin() +
    ggplot2::facet_wrap(~key, scales = "free") +
    ggpubr::stat_compare_means()
  if(!is.null(path)){
    ggsave(filename = path, plot = g, width = 12, height=6)
  }
  return(g)
}
