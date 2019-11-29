# ----
#' Survival ROC
#'
#' @param time vector with the time
#' @param event vector with the events
#' @param predvar the vector with the predictive variables
#' @param plot_best should the best one be plotted
#' @param plot_mean should the median one be plotted
#' @param plot_diagnostics should a diagnostic plot be plotted
#' @param verbose vebose printin?
#' @param minN minimum n per group
#' @param sig significance level (default <= 0.05)
#' @param max_cutoffs maximum number of cutoffs
#'
#' @return
#' @export
#'
#' @examples
survival_warrior <- function(time
                             , event
                             , predvar
                             , plot_best = T
                             , plot_mean = T
                             , plot_median = T
                             , plot_diagnostics = T
                             , verbose = F
                             , minN = 5
                             , sig = 0.05
                             , max_cutoffs = 20){
  message("# waRRior: calculating logrank test")
  res <- list()
  # calculate cutoff
  cutoffs <- sort(unique(predvar))
  #if(length(cutoffs) > max_cutoffs){
  #  message("Mamimum number of cutoffs reached reducing to ", max_cutoffs)
  #  mincut <- min(cutoffs)
  #  maxcut <- max(cutoffs)
  #  cutoffs <- seq(mincut, maxcut, length(cutoffs)/max_cutoffs)
  #}
  if(verbose) message("# Number of cutoffs: ", length(cutoffs))
  # loop through cutoffs
  first <- T
  for(cut in cutoffs){
    try({
      message(".", appendLF = F)
      # create groups
      groupvar <- as.factor(predvar >= cut)
      pos <- length(groupvar[groupvar == T])
      neg <- length(groupvar[groupvar == F])
      # calculate logrank for each
      if(length(table(groupvar)) > 1){
        s <- survival::Surv(time, event) ~ groupvar
        f <- survival::survfit(s)
        l <- coin::logrank_test(s)
        p <- coin::pvalue(l)
        i <- tibble::tibble(
          cutoff = cut
          , pvalue = p
          , Npos = pos
          , Nneg = neg
        )
        if(first){
          r <- i
          first <- F
        } else {
          r <- rbind(r, i)
        }
      }
    })
  }
  # plot diagnostics ----
  if(plot_diagnostics){
    message("\n# Ploting diagnostics")
    tmp <- dplyr::arrange(r, cutoff)
    g1 <- ggplot2::ggplot(tmp, ggplot2::aes(x = cutoff, y = pvalue)) +
      ggplot2::geom_line() +
      ggplot2::geom_abline(ggplot2::aes(intercept = 0.05, slope = 0), alpha = 0.5) +
      ggplot2::ylim(c(0,1))
    g2 <- ggplot2::ggplot(tmp) +
      ggplot2::geom_line(ggplot2::aes(y = Npos, x = cutoff, col = "")) +
      ggplot2::geom_line(ggplot2::aes(y = Nneg, x = cutoff)) +
      ggplot2::theme(legend.position = "none")
    g <- gridExtra::grid.arrange(g1, g2)
    print(g)
  }
  message("\n# Arrange table")
  # arrange for best on top ----
  r <- dplyr::arrange(r, pvalue)
  r <- dplyr::filter(r, Npos >= minN, Nneg >= minN, pvalue <= sig)
  res$result <- r

  # plot best
  if(plot_best){
    cut <- as.numeric(r[1,1])
    groupvar <- as.factor(predvar >= cut)
    if(length(table(groupvar)) > 1){
      s <- survival::Surv(time, event) ~ groupvar
      f <- survival::survfit(s)
      l <- coin::logrank_test(s)
      p <- coin::pvalue(l)
      p <- round(p, 5)
      plot(f
           , col = c("red", "blue")
           , main = paste0("Cutoff (best) = ", cut)
           , sub = paste0("pvalue = ", p)
           , xlab="Time"
           , ylab="Survival Probability"
      )
      legend(
        "topright",
        legend=c("<= cutoff", ">= cutoff"),
        fill=c("red", "blue"),
        horiz=T,
        bty='n')
      res$survfit$best <- f
    } else message("! only one factor level fo best avaiable. Aborting calculations.")

  }
  # plot mean ----
  if(plot_mean){
    cut <- waRRior::mean_noNA_noInf(predvar)
    groupvar <- as.factor(predvar >= cut)
    if(length(table(groupvar)) > 1){
      s <- survival::Surv(time, event) ~ groupvar
      f <- survival::survfit(s)
      l <- coin::logrank_test(s)
      p <- coin::pvalue(l)
      p <- round(p, 5)
      plot(f
           , col = c("red", "blue")
           , main = paste0("Cutoff (mean) = ", cut)
           , sub = paste0("pvalue = ", p)
           , xlab="Time"
           , ylab="Survival Probability"
      )
      legend(
        "topright",
        legend=c("<= cutoff", ">= cutoff"),
        fill=c("red", "blue"),
        horiz=T,
        bty='n')
      res$survfit$mean <- f
    } else message("! only one factor level fo mean avaiable. Aborting calculations.")
  }
  # plot median ----
  if(plot_median){
    cut <- waRRior::median_noNA_noInf(predvar)
    groupvar <- as.factor(predvar >= cut)
    if(length(table(groupvar)) > 1){
      s <- survival::Surv(time, event) ~ groupvar
      f <- survival::survfit(s)
      l <- coin::logrank_test(s)
      p <- coin::pvalue(l)
      p <- round(p, 5)
      plot(f
           , col = c("red", "blue")
           , main = paste0("Cutoff (median) = ", cut)
           , sub = paste0("pvalue = ", p)
           , xlab="Time"
           , ylab="Survival Probability"
      )
      legend(
        "topright",
        legend=c("<= cutoff", ">= cutoff"),
        fill=c("red", "blue"),
        horiz=T,
        bty='n')
      res$survfit$median <- f
    } else message("! only one factor level fo median avaiable. Aborting calculations.")
  }
  # Return
  return(res)
}
# ----
#' Tidy a survfit
#'
#' @param f a survfit or summary.survfit object
#'
#' @return
#' @export
#'
#' @examples
tidy_survfit <- function(f){
  if(class(f) == "survfit") f <- summary(f)
  if(class(f) == "summary.survfit"){
    cols <- lapply(c(2:6, 8:11, 16:17) , function(x) f[x])
    tbl <- do.call(data.frame, cols)
    return(tibble::as_tibble(tbl))
  }else{
    stop("Input has to be of class survfit or summary.survfit")
  }
}
# ----
#' Survival plot ggplot style
#'
#' @param f a survfit or summary.survfit object
#' @param ... other parameters
#'
#' @return
#' @export
#'
#' @examples
plot_survival <- function(f, ...){
  waRRior::errorWarrior({
    if(class(f) == "survfit") f <- summary(f)
    if(class(f) == "summary.survfit") f <- waRRior::tidy_survfit(f)
    g <- ggplot2::ggplot(f, ggplot2::aes(x = time, y = surv, color = strata)) +
      ggplot2::geom_step()
    plot(g)
  }, ...)
}

