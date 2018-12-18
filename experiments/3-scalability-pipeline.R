load("./data/embedded_data.rdata")
library(tsensembler)

source("sources/src-methods.R")
source("sources/src-workflows.r")
source("sources/utils.R")
source("sources/src-summarystats.R")
source("sources/src-base-models-specification.R")
source("sources/src-analysis.R")
source("sources/src-analysis-plots.R")

form <- target ~.
L <- length(embedded_time_series)
ids <- 1:L

nreps <- 15
tr_size <- .5
ts_size <- .25

specs <- MODELSPECS

scale_results <- vector("list", L)
for (i in seq_along(scale_results)[ids]) {
  cat(i, "\n\n\n")
  X <- embedded_time_series[[i]]

  cat("Getting mc samples\n")
  set.seed(1234)
  MC_samples <-
    resample_timeseries(X, nreps, tr_size, ts_size)

  ###### Running pipelines
  cat("Running pipeline for scalability analysis\n")
  scale_res <- vector("list", nreps)
  for (j in 1:nreps) {
    train <- MC_samples[[j]]$train
    test  <- MC_samples[[j]]$test
    scale_res[[j]] <-
      tryCatch(SCALABILITY_PIPELINE(train, test, form, specs),
               error = function(e) NA)
  }

  scale_results[[i]] <- scale_res

  save(scale_results, file = paste0("ADE_R1_SCALE_", ids[1], ".rdata"))
}

