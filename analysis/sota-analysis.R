library(tsensembler)
library(reshape2)
library(ggplot2)


nms <- names(embedded_time_series)

main_results <- rm.null(main_results)
#
names(main_results[[1]][[1]]$AGG_RMSE)

nms_methods <-
  c("ADE","ADE-vanilla","Stacking",
    "AEC","WindowLoss",
    "ERP","Simple","BestTrain",
    "Blast","SimpleTrim", "LossTrain",
    "MLpol","Arbitrating","EWA",
    "FixedShare","OGD",
    "ARIMA","ExpSmooth",
    "Naive","SeasonalNaive",
    "ADE-selectbest","ADE-allmodels","ADE-v0",
    "ADE-noreweight")

nms_variants <-
  c("ADE","ADE-vanilla",
    "ADE-selectbest","ADE-allmodels",
    "ADE-v0","ADE-noreweight")

nms_sota <-
  c("ADE","AEC","WindowLoss",
    "ERP","Simple","BestTrain","Arbitrating","Stacking",
    "Blast","SimpleTrim", "LossTrain",
    "MLpol","EWA",
    "FixedShare","OGD",
    "ARIMA","ExpSmooth","Naive","SeasonalNaive")


main_all <-
  lapply(main_results,
         function(X) {
           X <- X[!is.na(X)]
           lapply(X,
                  function(Z) {
                    names(Z$AGG_RMSE) <- nms_methods

                    Z
                  })
         })


main_sota <-
  lapply(main_results,
         function(X) {
           X <- X[!is.na(X)]
           lapply(X,
                  function(Z) {
                    names(Z$AGG_RMSE) <- nms_methods
                    lv <- nms_methods %in% nms_sota
                    Z$AGG_RMSE <- Z$AGG_RMSE[lv]
                    Z
                  })
         })

main_variants <-
  lapply(main_results,
         function(X) {
           X <- X[!is.na(X)]
           lapply(X,
                  function(Z) {
                    names(Z$AGG_RMSE) <- nms_methods
                    lv <- nms_methods %in% nms_variants
                    Z$AGG_RMSE <- Z$AGG_RMSE[lv]
                    Z
                  })
         })


# Bayesian correlated t-test
nreps <- 15
analysis_by_ds_sota <-
  lapply(main_sota,
         function(x) {
           results_analysis_in_dataset(x = x,
                                       baseline = "ADE",
                                       rho = 1 / nreps,
                                       rope_min = -.01,
                                       rope_max = .01,
                                       nm = "AGG_RMSE",
                                       scale = T)
         })


analysis_by_ds_vars <-
  lapply(main_variants,
         function(x) {
           results_analysis_in_dataset(x = x,
                                       baseline = "ADE",
                                       rho = 1 / nreps,
                                       rope_min = -.01,
                                       rope_max = .01,
                                       nm = "AGG_RMSE",
                                       scale = F)
         })


analysis_by_ds_all <-
  lapply(main_all,
         function(x) {
           results_analysis_in_dataset(x = x,
                                       baseline = "ADE",
                                       rho = 1 / nreps,
                                       rope_min = -.01,
                                       rope_max = .01,
                                       nm = "AGG_RMSE",
                                       scale = T)
         })


res_across_ds_all_bs <-
  analysis_across_datasets_bsign(
    results  = analysis_by_ds_all,
    baseline = "ADE",
    rope_min = -.01,
    rope_max = .01,
    samples = 3000
  )

proportion_plot(res_across_ds_all_bs)

rank_across_ds <-
  t(sapply(analysis_by_ds_sota,
           function(x) x$agg_rank))

rank_across_ds_vars <-
  t(sapply(analysis_by_ds_vars,
           function(x) x$agg_rank))


avg_rank_and_plot(rank_across_ds)
avg_rank_and_plot(rank_across_ds_vars)

# Percentual diff in mase
perc_diff_plot(res=analysis_by_ds_all,
               baseline = "ADE",
               scales_type = "fixed",
               nrows = 1)
#


##
# bayesian correlated ttest matrix
thresh <- .95
l <-length(analysis_by_ds_all$aep$avg_loss)-1
m <- matrix(0, ncol = 6, nrow=l)
rownames(m) <- names(analysis_by_ds_all$air_company_stock2$avg_loss[-1])
colnames(m) <- c("L","SL","D","SD","W","SW")
for (i in seq_along(analysis_by_ds_all)) {
  ds <- analysis_by_ds_all[[i]]$BayesCorrTT

  m[,"W"] <- m[,"W"] + as.integer(ds["left",] > .5)
  m[,"SW"] <- m[,"SW"] + as.integer(ds["left",] > thresh)
  m[,"L"] <- m[,"L"] + as.integer(ds["right",] > .5)
  m[,"SL"] <- m[,"SL"] + as.integer(ds["right",] > thresh)

  m[,"D"] <- m[,"D"] + as.integer((ds["left",] < .5 & ds["right",] < .5) | ds["rope",] > .5)
  m[,"SD"] <- m[,"SD"] + as.integer(ds["rope",] > thresh)
}
m

