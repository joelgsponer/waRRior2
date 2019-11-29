# Cluster Heatmap ----
#' Create clustered Heatmap
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
# Rtsne ----
#' Title
#'
#' @param m matrix
#' @param p perplexity
#' @param t theta
#' @param d dimensions
#' @param clusters A tibble or data frame with ID and Cluster
#' @param color_clusters Should the clusters be colored
#' @param which_clusters Column of row (for waRRior_heatmap)
#' @param path Provide a path to save the plot
#' @param ...
#'
#' @return Returns a ggplot
#' @export
#'
#' @examples
#' # Without clusters
#' a <- runif(100)
#' b <- runif(100)
#' c <- runif(100)
#' m <- as.matrix(cbind(a,b,c))
#' plot_tsne(m)
#'
#' # With clusters
#'
#' a <- runif(100)
#' b <- runif(100)
#' c <- runif(100)
#' m <- as.matrix(cbind(a,b,c))
#' rownames(m) <- seq(1,100)
#' clusters = data.frame(ID = rownames(m), Cluster = sample(seq(1:4), size = 1000, replace = T))
#' plot_tsne(m, color_clusters = T, clusters = clusters)

plot_tsne <- function(m
                      , p = 30
                      , t = 0.5
                      , d = 2
                      , clusters = NULL
                      , color_clusters = F
                      , which_clusters = "column"
                      , check_duplicates = F
                      , point_size = 0.25
                      , path = NULL
                      ,...
                      ){
  set.seed(42)
  if(class(m) == "waRRior_heatmap"){
    message("# WaRRior heatmap as input detected.")
    if(which_clusters == "column"){
      clusters <- m[['column_clusters']]
    }
    if(which_clusters == "row"){
      clusters <- m[['row_clusters']]
    }
    color_clusters = T
    m <- t(m[['matrix']])
  } else {
    if(color_clusters & is.null(clusters)) stop("No clusters provided. Please pass file with ID and Cluster")
  }

  tsne_model <- Rtsne::Rtsne(  m
                             , perplexity=p
                             , theta=t
                             , dims=d
                             , check_duplicates=check_duplicates
                             , ...
                             )
  tsne_df = tibble::as_tibble(tsne_model$Y)
  tsne_df <- dplyr::mutate(tsne_df, ID = rownames(m))
  if(color_clusters){
    tsne_df <- dplyr::inner_join(tsne_df, clusters)
    tsne_df <- dplyr::mutate(tsne_df, Cluster = as.factor(Cluster))
  }
  g <- ggplot2::ggplot(tsne_df, ggplot2::aes(x=V1, y=V2)) +
    ggplot2::geom_point(size=point_size) +
    ggplot2::guides(colour=ggplot2::guide_legend(override.aes=list(size=6))) +
    ggplot2::xlab("") + ggplot2::ylab("") +
    ggplot2::labs(title = "t-SNE", caption = sprintf("Perplexity: %s Theta: %s Dimensions: %s", p, t, d)) +
    ggplot2::theme_light(base_size=20) +
    ggplot2::theme(
        axis.text.x=ggplot2::element_blank()
      , axis.text.y=ggplot2::element_blank()
      , plot.caption = ggplot2::element_text(size = 10)
      )
    #ggplot2::scale_colour_brewer(palette = "Set2")
  if(color_clusters){
    g <- g + ggplot2::geom_point(ggplot2::aes(colour = Cluster), size = point_size)
  }
  if(!is.null(path)) pdf(file = path)
  plot(g)
  if(!is.null(path)) dev.off()
  r = list(
    d = tsne_df,
    p = g
  )
  class(r) <- "waRRior_tsne"
  return(r)
}
# Stacked Percentage Barplot ----
#' Stacked Percentage Barplot
#'
#' @param d
#' @param predictive_variable
#' @param dependent_variable
#' @param path
#' @param show_plot
#' @param dataset
#' @param generate_path
#'
#' @return
#' @export
#'
#' @examples
stacked_percentage_plot <- function(
  d
  , predictive_variable
  , dependent_variable
  , path = NULL
  , show_plot = T
  , dataset = NULL
  , generate_path = F
){
  # Symbols and names ----
  if(is.null(dataset)) dataset <- deparse(substitute(d))
  print(class(predictive_variable))
  print(class(dependent_variable))
  if(class(predictive_variable) == "character"){
    predvar = sym(predictive_variable)
    predname <- predictive_variable
  } else {
    predvar = dplyr::enquo(predictive_variable)
    predname <- as.character(predvar[2])
  }

  if(class(dependent_variable) == "character"){
    depvar = sym(dependent_variable)
    depname <- dependent_variable
  } else {
    depvar = dplyr::enquo(dependent_variable)
    depname <- as.character(depvar[2])
  }

  # Calcualting percentages and position
  p <- dplyr::select(d, !!predvar, !!depvar)
  p <- dplyr::group_by_all(p)
  p <- dplyr::tally(p)
  p <- dplyr::ungroup(p)
  p <- dplyr::group_by(p, !!predvar)
  p <- dplyr::group_split(p)
  p <- purrr::map(.x = p, .f = function(x){
    total = sum(x$n)
    x <- dplyr::mutate(x, p = n/total*100)
    x <- dplyr::mutate(x, t = cumsum(p))
    x <- dplyr::mutate(x, m = t - (p/2))
    return(x)
  })
  p <- bind_rows(p)

  # Inference ----
  message("Calculate Chi-square test")
  t <- infer::chisq_test(d, expr(!!ensym(dependent_variable) ~ !!ensym(predictive_variable)))

  # Plot ----
  message("Plotting")
  g <- ggplot2::ggplot(p, ggplot2::aes(x = !!predvar, y = p, fill = as.factor(!!depvar))) +
    ggplot2::geom_col() +
    ggplot2::geom_text(ggplot2::aes(label = paste0(round(p), "%(", n,")"), y = 100 - m)) +
    ggplot2::ylab("%") +
    ggplot2::xlab(predname) +
    ggplot2::labs(fill = depname, caption = paste("Permutation chi-squared test\np = ", waRRior::waRRior_p(t$p_value))) +
    ggpubr::theme_pubclean()

  # Output ----
  message("Generating output")
  if(show_plot)message("Plotting")
  if(generate_path) path = sprintf("./img/Stacked-percentage-%s-%s-%s.pdf", depname, predname, dataset)
  if(!is.null(path)){
    message("Saving at ", path)
    ggplot2::ggsave(filename = path,plot = g, device = "pdf")
  }
  r <- structure(list(
    "table" = p
    , "plot"  = g
  ), class = "waRRior_stacked_barplot")
  return(r)
}




