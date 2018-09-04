load("data/embedded_data.rdata")

library(tsensembler)
library(forecast)

source("sources/src-analysis-plots.R")
source("sources/src-analysis.R")
source("sources/src-base-models-specification.R")
source("sources/src-methods.R")
source("sources/src-summarystats.R")
source("sources/src-workflows.r")
source("sources/utils.R")


embedded_time_series <-
  embedded_time_series[sapply(embedded_time_series, nrow) > 2900]

L <- length(embedded_time_series)
ids <- 1:L
form <- target ~.

specs <- MODELSPECS

rsize <- 1
tr_size <- .7
ts_size <- .29

retrain_analysis <- vector("list", L)
for (i in seq_along(retrain_analysis)[ids]) {
  cat(i, "\n\n\n")
  cat(as.character(Sys.time()),"\n")

  set.seed(1234)
  resamples_timeseries <-
    resample_timeseries(embedded_time_series[[i]],
                        rsize,
                        tr_size,
                        ts_size)

  train <- resamples_timeseries[[1]]$train
  test <- resamples_timeseries[[1]]$test

  retrain_res <-
    RETRAIN_ADE_PIPELINE(train,
                         test,
                         form,
                         specs,
                         step = 100)

  retrain_analysis[[i]]  <- retrain_res

  save(retrain_analysis, file = paste0("ADE_R1_retrain_", ids[1], ".rdata"))
}
names(retrain_analysis) <- names(embedded_time_series)

retrain_analysis <- sapply(retrain_analysis, unlist)

ranks <- apply(retrain_analysis,2,rank)
rownames(ranks) <-
  c("M0_Z0(ADE)",
    "M1_Z1",
    "M1_Z0",
    "M0_Z1",
    "ADE-runtime")

avg_rank_and_plot(t(ranks))

