#' plot avg rank base predictors
#'
#' @param x results from func base_predictors_avg_rank
base_predictors_avg_rank_plot <-
  function(base_res) {
    require(reshape2)
    require(ggplot2)

    rank_info <-
      sapply(base_res,
             function(x) {
               avg_mase <- rowMeans(sapply(x, unlist))
               rank(avg_mase)
             })

    avgrank_sort <- names(sort(apply(rank_info,1,median)))

    spl <-
      sapply(avgrank_sort,
             function(z) {
               split_by_(as.character(z))[1]
             }, USE.NAMES = FALSE)

    spl <- toupper(spl)

    spl <- gsub("NNET","MLP", spl)
    spl <- gsub("GBM","GBR", spl)
    spl <- gsub("CUB","RBR", spl)
    #spl[13] <- "PLS"
    #spl[15] <- "PLS"
    #spl[22] <- "PCR"

    rank_info <- rank_info[avgrank_sort,]

    x_melted <- melt(t(rank_info))

    p <- ggplot(x_melted, aes(Var2, value))

    p  +
      geom_boxplot() +
      theme_minimal() +
      labs(x="",
           y="Rank") +
      theme(axis.text.x  = element_text(size=10,
                                        angle=90)) +
      theme(axis.text.y  = element_text(size = 12),
            axis.title.y = element_text(size = 12)) +
      scale_x_discrete(labels=spl)
  }




#' avg rnak with plot
#'
#' @param x rank across ds (func analysis_across_datasets)
avg_rank_and_plot <-
  function(x) {
    require(ggplot2)

    avg_rank <- colMeans(x)
    nms_ord <- names(sort(avg_rank))
    sd_rank <- apply(x,2,sd)

    ord <- order(avg_rank)

    avg_rank <- avg_rank[ord]
    sd_rank <- sd_rank[ord]
    nms <- as.factor(names(sd_rank))

    ds <- data.frame(avg_rank=avg_rank,
                     sd_rank=sd_rank,
                     workflow = nms)

    ds$workflow <-
      factor(ds$workflow, levels = nms_ord)


    ggplot(data = ds,
           aes(x = workflow,
               y = avg_rank)) +
      geom_bar(stat="identity",
               fill="#33CCCC") +
      theme_minimal() +
      theme(axis.text.x  = element_text(angle = 35,
                                        size = 14,
                                        hjust = 1)) +
      theme(axis.text.y  = element_text(size = 12),
            axis.title.y = element_text(size = 12)) +
      #geom_hline(yintercept = 1) +
      geom_errorbar(aes(ymin = avg_rank - sd_rank,
                        ymax = avg_rank + sd_rank),
                    width = .5,
                    position = position_dodge(.9)) +
      labs(x="",
           y="Avg. rank & Std dev.",
           title = "")
  }

#' perc diff data
#'
#' @param res res from func results_analysis_by_ds
#' @param baseline baseline
perc_diff_plot <-
  function(res,baseline="ADE",
           scales_type="free",
           nrows=2) {
    require(reshape2)
    require(ggplot2)

    AVG_LOSS <-
      sapply(res,
             function(x) {
               colMeans(x$loss_vals)
             })

    AVG_LOSS <- as.data.frame(t(AVG_LOSS))
    col_baseline <- grep(paste0("^",baseline,"$"), colnames(AVG_LOSS))

    PercDiff <-
      lapply(AVG_LOSS,
             function(x) {
               percentual_difference(AVG_LOSS[,col_baseline],x)
             })
    PercDiff <- as.data.frame(PercDiff[-col_baseline])

    PercDiff[] <-
      lapply(PercDiff,
             function(x) {
               x[x %in% boxplot.stats(x)$out] <- NA_real_
               x
             })

    PD_melted <- melt(PercDiff)

    PD_melted$variable <-  
        factor(PD_melted$variable, 
          levels = levels(PD_melted$variable)[order(levels(PD_melted$variable))])

    ggplot(PD_melted, aes(x=1,y= log_trans(value))) +
      facet_wrap( ~ variable,
                  nrow = nrows,
                  scales=scales_type) +
      geom_boxplot() +
      geom_hline(yintercept = 0,col="red") +
      theme_minimal() +
      labs(x="",
           y="ADE log perc. diff to:") +
      theme(axis.text.x  = element_blank()) +
      theme(axis.text.y  = element_text(size = 14),
            axis.title.y = element_text(size = 14),
            strip.text.x = element_text(angle = 90, size= 12))
  }


proportion_plot <-
  function(ds) {
    ds <- as.data.frame(t(ds))
    ds[ ] <- lapply(ds, unlist)
    ds$method <- rownames(ds)
    dsm <- melt(ds)
    colnames(dsm) <- c("method", "Result:","val")
    dsm$`Result:` <- gsub("probLeft|winLeft","ADE wins",dsm$`Result:`)
    dsm$`Result:` <- gsub("probRope|winRope","ADE draws",dsm$`Result:`)
    dsm$`Result:` <- gsub("probRight|winRight","ADE loses",dsm$`Result:`)
    dsm$`Result:` <-
      factor(dsm$`Result:`, levels = c("ADE wins",
                                       "ADE draws",
                                       "ADE loses"))

    ggplot(dsm, aes(x = method,
                    y = val,
                    fill = `Result:`)) +
      geom_col(position = "fill") +
      ylab("Proportion of probability") +
      xlab("") +
      theme_minimal() +
      theme(axis.text.x  = element_text(angle = 30,
                                        size = 11,
                                        hjust = 1),
            legend.position = "top") +
      theme(axis.text.y  = element_text(size = 11),
            axis.title.y = element_text(size = 11)) #+
    # scale_fill_hue(h = c(180, 300))
    # scale_fill_grey()
    # scale_fill_manual( values=c("red","green","blue"))
    # scale_fill_brewer(palette = "Set1")
  }

exec_time_plot_solo <-
  function(x) {
    x <- as.data.frame(x)

    ggplot(x, aes(x=1,y=x)) +
      geom_boxplot() +
      theme_minimal() +
      labs(x="",
           y="Run time difference (secs)") +
      theme(axis.text.x  = element_blank()) +
      theme(axis.text.y  = element_text(size = 14),
            axis.title.y = element_text(size = 14))
  }


multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
   library(grid)

   # Make a list from the ... arguments and plotlist
   plots <- c(list(...), plotlist)

   numPlots = length(plots)

   # If layout is NULL, then use 'cols' to determine layout
   if (is.null(layout)) {
     # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
#
  if (numPlots==1) {
    print(plots[[1]])
#
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
#
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
#
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}
