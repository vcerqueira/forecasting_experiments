library(tsensembler)
library(reshape2)
require(ggplot2)

nms <- names(embedded_time_series)


correlation_results <- rm.null(correlation_results)

nms_vars <-
  c(
    "loss_ADE",
    "loss_ADE_corr",
    "loss_ADE_PCA",
    "loss_ADE_noSR",
    "loss_ADE_noSR_corr",
    "loss_ADE_noSR_PCA")

nms_clean <- c("ADE-corr", "ADE-pca",
               "ADE-noreweight",
               "ADE-noreweight-corr",
               "ADE-noreweight-pca")

# nreps <- 15
# rho<-1/nreps
# rope_max<-.01
# rope_min<- -.01
# baseline <- "rmse_ADE"
# BayesCorrTtest <-
#   lapply(correlation_results,
#          function(x) {
#            agg_x <- lapply(x, function(z)
#              unlist(z))
#            agg_x <- rbind_l(agg_x)
#            col_baseline <-
#              grep(paste0("^", baseline, "$"), colnames(agg_x))
#            c_iters <- seq_len(ncol(agg_x))[-col_baseline]
#
#            BayesCorrTT <-
#              sapply(c_iters,
#                     function(j) {
#                       x_base <- agg_x[, col_baseline]
#                       x_comp <- agg_x[, j]
#
#                       diff_vec <- x_base - x_comp
#
#                       correlatedBayesianTtest(diff_vec, rho, rope_min, rope_max)
#                     })
#            colnames(BayesCorrTT) <- colnames(agg_x)[-col_baseline]
#
#            BayesCorrTT <- as.data.frame(BayesCorrTT)
#            BayesCorrTT[] <- lapply(BayesCorrTT, unlist)
#
#            BayesCorrTT
#          })

avg_loss <-
  lapply(correlation_results,
         function(x) {
           x <- rbind_l(lapply(x, unlist))
           colMeans(x, na.rm = T)
         })

avg_loss <- as.data.frame(rbind_l(avg_loss))

# Bayes_Sign_corr <-
#   lapply(avg_loss,
#          function(x) {
#            diff_vec <- avg_loss$rmse_ADE - x
#
#            if (sum(diff_vec) == 0) {
#              NULL
#            } else {
#              BS <- BayesianSignTest(diff_vec,-.01, .01)
#              round(unlist(BS), 2)
#            }
#          })
#
# Bayes_Sign_corr <- rm.null(Bayes_Sign_corr)
# Bayes_Sign_corr <- rbind_l(Bayes_Sign_corr)
# rownames(Bayes_Sign_corr) <- nms_clean
#
# ds <- Bayes_Sign_corr
# ds <- as.data.frame(ds)
# ds$method <- rownames(ds)
# dsm <- melt(ds)
# colnames(dsm) <- c("method", "Result:","val")
# dsm$`Result:` <- gsub("probLeft|winLeft","ADE wins",dsm$`Result:`)
# dsm$`Result:` <- gsub("probRope|winRope","ADE draws",dsm$`Result:`)
# dsm$`Result:` <- gsub("probRight|winRight","ADE loses",dsm$`Result:`)
# dsm$`Result:` <-
#   factor(dsm$`Result:`, levels = c("ADE wins",
#                                    "ADE draws",
#                                    "ADE loses"))
#
# ggplot(dsm, aes(x = method,
#                 y = val,
#                 fill = `Result:`)) +
#   geom_col(position = "fill") +
#   ylab("Proportion of probability") +
#   xlab("") +
#   theme_minimal() +
#   theme(axis.text.x  = element_text(angle = 30,
#                                     size = 11,
#                                     hjust = 1),
#         legend.position = "top") +
#   theme(axis.text.y  = element_text(size = 11),
#         axis.title.y = element_text(size = 11))
# #

##
ranks <- apply(avg_loss,1,rank)

ranks <- t(ranks)
colnames(ranks) <-
  c("ADE","ADE-corr","ADE-pca",
    "ADE-noreweight",
    "ADE-corr-noreweight",
    "ADE-pca-noreweight")

avg_rank_and_plot(ranks) +
  theme(axis.text.x  = element_text(angle = 25,
                                    size = 14,
                                    hjust = 1))
