#' Create
#'
#' @param d Tibble with an ID column, a key column and a value column
#' @param n number of clusters
#' @param show Print plot
#' @param colorscheme The Colorscheme for the heatmap
#' @param square Make the tiles square (TRUE or FALSE)
#' @return
#' @export
#' @example
#'
cluster_heatmap <- function(  d
                            , n
                            , colorscheme = c(rev(google_colors$Blue$accent[4]),"black", google_colors$Red$accent[4])
                            , square = F
                            , show = T
                            ){
  google_colors <- waRRior::google_colors_load(verbose = F)
  # Number of clusters
  nClusters = n
  nmarkers <- length(table(d$key))
  # Spread data
  d <- tidyr::spread(d, key = "key", value = "value")
  d_wide <- dplyr::mutate_at(.tbl = d,.vars = dplyr::vars(-!!"ID"), .funs = list(scales::rescale))
  # Impute
  # Return the unaltered tibble if nothing is left to impute (throwed me an error)
  d_wide_imputed <- tryCatch({
    tibble::as_tibble(mice::complete(mice::mice(data = d_wide,method = "pmm", printFlag = F),1))
  }, error = function(e){
    return(d_wide)
  })
  # Cluster
  clusters <- stats::kmeans(dplyr::select(d_wide_imputed, -!!"ID"), centers = nClusters)$cluster
  # Combine
  d_wide <- dplyr::mutate(d_wide, cluster = clusters)
  d_wide <- tidyr::gather(d_wide, key = "key", value = "value", -!!"cluster", -!!"ID")
  d_wide <- dplyr::arrange_at(d_wide, "cluster")
  #dd <- dplyr::mutate(d_wide_imputed, ID = factor(ID, unique(ID)))
  dd <- dplyr::mutate_at( .tbl = d_wide
                          , .vars = "ID"
                          , .funs = list(function(x){factor(x, unique(x))})
  )
  t <- ggplot2::theme(   axis.text.x = ggplot2::element_text(angle = 90, hjust = 1)
                         , plot.margin = ggplot2::unit(c(2,0,2,0), "cm")
                         , plot.background = ggplot2::element_blank()
                         , panel.spacing = ggplot2::unit(0, "cm")
                         , panel.grid = ggplot2::element_blank()
                         , axis.title.y = ggplot2::element_blank()
                         , axis.text.y = ggplot2::element_blank()
                         , axis.ticks.y = ggplot2::element_blank()
                         , axis.title.x = element_blank()
  )

  p1 <- ggplot2::ggplot(data = dd, ggplot2::aes(x = as.factor(dd$key), y = dd$ID)) +
    ggplot2::geom_raster(ggplot2::aes(fill = dd$value)) +
    ggplot2::scale_fill_gradientn(
      colours = colorscheme) +
    ggplot2::labs(fill = "Scaled Z-Score") +
    ggplot2::theme(legend.position = "left") +
    t
  if(square) p1 <- p1 + ggplot2::coord_equal()

  p2 <- ggplot2::ggplot(data = dd, ggplot2::aes(y = dd$ID, x = "Cluster")) +
    ggplot2::geom_raster(ggplot2::aes(fill = as.factor(dd$cluster))) +
    ggplot2::labs(fill = "Cluster") +
    ggplot2::theme(legend.position = "right") +
    t
  if(square) p2 <- p2 + ggplot2::coord_equal()

  g1 <- ggplot2::ggplotGrob(p1)
  g2 <- ggplot2::ggplotGrob(p2)
  g2$heights <- g1$heights

  w <- ggplot2::unit(c(nmarkers+1,5), units = "cm")
  h <- ggplot2::unit(c(20), units = "cm")

  mat <- matrix(list(g1,g2), nrow = 1)
  g <- gtable::gtable_matrix("Heatmap"
                             , grobs = mat
                             , widths = w
                             , heights = h
  )
  if(show){
    grid::grid.newpage()
    grid::grid.draw(g)
  }
  return(g)
}
