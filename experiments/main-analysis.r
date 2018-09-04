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

seqrew_results <- vector("list", L)
correlation_results <- vector("list", L)
main_results <- vector("list", L)
scale_results <- vector("list", L)
addexperts_results <- vector("list", L)
for (i in seq_along(correlation_results)[ids]) {
  cat(i, "\n\n\n")
  X <- embedded_time_series[[i]]

  cat("Getting mc samples\n")
  set.seed(1234)
  MC_samples <-
    resample_timeseries(X, nreps, tr_size, ts_size)

  ###### Running pipelines
  cat("Running main pipeline\n")
  main_h <- vector("list", nreps)
  for (j in 1:nreps) {
    train <- MC_samples[[j]]$train
    test  <- MC_samples[[j]]$test
    main_h[[j]] <-
      tryCatch(MAIN_PIPELINE(train, test, form, specs),
               error = function(e) NA)
  }

  cat("Running pipeline for SR impact analysis\n")
  seq_rew_res <- vector("list", nreps)
  for (j in 1:nreps) {
    train <- MC_samples[[j]]$train
    test  <- MC_samples[[j]]$test
    seq_rew_res[[j]] <-
      tryCatch(SeqRew_IMPACT_PIPELINE(train, test, form, specs),
               error = function(e) NA)
  }

  cat("Running pipeline for scalability analysis\n")
  scale_res <- vector("list", nreps)
  for (j in 1:nreps) {
    train <- MC_samples[[j]]$train
    test  <- MC_samples[[j]]$test
    scale_res[[j]] <-
      tryCatch(SCALABILITY_PIPELINE(train, test, form, specs),
               error = function(e) NA)
  }

  cat("Running pipeline for correlation analysis\n")
  correlation_in_feats <- vector("list", nreps)
  for (j in 1:nreps) {
    train <- MC_samples[[j]]$train
    test  <- MC_samples[[j]]$test
    correlation_in_feats[[j]] <-
      tryCatch(CORRELATION_PIPELINE(train, test, form, specs),
               error = function(e) NA)
  }

  cat("Adding experts analysis\n")
  add_experts_res <- vector("list", nreps)
  for (j in 1:nreps) {
    train <- MC_samples[[j]]$train
    test  <- MC_samples[[j]]$test
    add_experts_res[[j]] <-
      tryCatch(ADDING_EXPERTS_PIPELINE(train, test, form, MODELSPECS_plus, 100),
               error = function(e) NA)
  }

  main_results[[i]] <- main_h
  seqrew_results[[i]] <- seq_rew_res
  correlation_results[[i]] <- correlation_in_feats
  scale_results[[i]] <- scale_res
  addexperts_results[[i]] <- add_experts_res

  save(main_results, file = paste0("ADE_R1_MAIN_H_", ids[1], ".rdata"))
  save(seqrew_results, file = paste0("ADE_R1_SR_", ids[1], ".rdata"))
  save(correlation_results, file = paste0("ADE_R1_CORRELATION_", ids[1], ".rdata"))
  save(scale_results, file = paste0("ADE_R1_SCALE_", ids[1], ".rdata"))
  save(addexperts_results, file = paste0("ADE_R1_AE_", ids[1], ".rdata"))
}

