library(tsensembler)
library(tsbox)

nms <- names(embedded_time_series)


seqrew_results <- rm.null(seqrew_results)

nms_vars <-
  c(
    "RMSE_WindowLoss_1",
    "RMSE_MLpol_1",
    "RMSE_AEC_1",
    "RMSE_EWA_1",
    "RMSE_FixedShare_1",
    "RMSE_OGD_1")


nreps <- 15
SR_impact_BCTT <-
  lapply(nms_vars,
         function(nm) {
           sr_method <-
             lapply(seqrew_results,
                    function(x) {
                      x1<<-x
                      sr_impact_bayesian_analysis(x = x,
                                                  baseline = nm,
                                                  1 / nreps, -.01, .01)
                    })

           sr_method <- lapply(sr_method, unlist)
           sr_method <- round(rbind_l(sr_method), 2)

           thresh <- .95

           W <- sum(apply(sr_method, 1, function(x)
             x["left"] > .5))

           SW <- sum(apply(sr_method, 1, function(x) {
             x["left"] > .95
           }))

           D <- sum(apply(sr_method, 1, function(x) {
             (x["left"] < .5 & x["right"] < .5) | x["rope"] >= .5
           }))

           SD <- sum(apply(sr_method, 1, function(x) {
             x["rope"] > .95
           }))

           L <- sum(apply(sr_method, 1, function(x) {
             x["right"] > .5
           }))

           SL <- sum(apply(sr_method, 1, function(x) {
             x["right"] > .95
           }))

           c(W = W, SW = SW, D = D, SD = SD, L = L, SL = SL)
         })

nms_vars2 <- gsub("LOSS_|_1","",nms_vars)
names(SR_impact_BCTT)<- nms_vars2
SR_impact_BCTT <- rbind_l(SR_impact_BCTT)
rownames(SR_impact_BCTT) <- nms_vars2
#
#
# SR_impact_BayesSign <-
#   lapply(nms_vars,
#          function(nm) {
#
#            nm_simple <- gsub("LOSS_|_1","",nm)
#            SR_nm <-
#              lapply(SR,
#                     function(x) {
#                       lapply(x,
#                              function(z) {
#                                z[grepl(nm_simple,names(z))]
#                              })
#                     })
#
#            avg_loss <-
#              lapply(SR_nm,
#                   function(x) {
#                     rowMeans(sapply(x, unlist))
#                   })
#
#            avg_loss <- as.data.frame(rbind_l(avg_loss))
#            id1 <- grep("_1", colnames(avg_loss))
#            id0 <- grep("_0", colnames(avg_loss))
#
#            diff_vec <- avg_loss[,id1] - avg_loss[,id0]
#
#            BS <- BayesianSignTest(diff_vec, -.01, .01)
#            BS <- round(unlist(BS),2)
#            #BR <- BayesianSignedRank(diff_vec, -.01, .01)
#            #BR <- round(unlist(BR),2)
#
#            BS
#            #list(BS=BS,BR=BR)
#          })
#
# SR_impact_BayesSign <- rbind_l(SR_impact_BayesSign)
# rownames(SR_impact_BayesSign) <- nms_vars2
#
#
# SR_impact_BayesRank <-
#   lapply(nms_vars,
#          function(nm) {
#
#            nm_simple <- gsub("LOSS_|_1","",nm)
#            SR_nm <-
#              lapply(SR,
#                     function(x) {
#                       lapply(x,
#                              function(z) {
#                                z[grepl(nm_simple,names(z))]
#                              })
#                     })
#
#            avg_loss <-
#              lapply(SR_nm,
#                     function(x) {
#                       rowMeans(sapply(x, unlist))
#                     })
#
#            avg_loss <- as.data.frame(rbind_l(avg_loss))
#            id1 <- grep("_1", colnames(avg_loss))
#            id0 <- grep("_0", colnames(avg_loss))
#
#            diff_vec <- avg_loss[,id1] - avg_loss[,id0]
#
#            BR <- BayesianSignedRank(diff_vec, -.01, .01)
#            BR <- round(unlist(BR),2)
#
#            BR
#          })
#
# SR_impact_BayesRank <- rbind_l(SR_impact_BayesRank)
# rownames(SR_impact_BayesRank) <- nms_vars2
#
# ds <- SR_impact_BayesSign#SR_impact_BayesRank
# ds <- as.data.frame(ds)
# ds$method <- rownames(ds)
# dsm <- melt(ds)
# colnames(dsm) <- c("method", "Result:","val")
# dsm$`Result:` <- gsub("probLeft|winLeft","Using SR wins",dsm$`Result:`)
# dsm$`Result:` <- gsub("probRope|winRope","Using SR draws",dsm$`Result:`)
# dsm$`Result:` <- gsub("probRight|winRight","Using SR loses",dsm$`Result:`)
# dsm$`Result:` <-
#   factor(dsm$`Result:`, levels = c("Using SR wins",
#                                    "Using SR draws",
#                                    "Using SR loses"))
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

