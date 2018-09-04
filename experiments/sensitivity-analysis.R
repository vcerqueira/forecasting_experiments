library(tsensembler)

load("./data/embedded_data.rdata")

source("sources/src-workflows.r")
source("sources/utils.r")
source("sources/src-base-models-specification.R")

embedded_time_series <-
  embedded_time_series[sapply(embedded_time_series, nrow) > 2900]

L <- length(embedded_time_series)
ids <- c(1,20)#1:L

form <- target ~.

rsize <- 2
tr_size <- .5
ts_size <- .25

specs <- MODELSPECS

sens_analysis <- vector("list", L)
for (i in seq_along(sens_analysis)[ids]) {
  cat(i, "\n\n\n")

  set.seed(123)
  resamples_timeseries <-
    resample_timeseries(embedded_time_series[[i]],
                        rsize,
                        tr_size,
                        ts_size)

  MC_ESTIMATES <-
    lapply(resamples_timeseries,
           function(o) {
             set.seed(1234)
             ade <-
               ADE(
                 form = form,
                 data = o$train,
                 specs = specs,
                 lambda = 700,
                 omega = .5,
                 select_best = FALSE,
                 all_models = FALSE,
                 aggregation = "linear",
                 sequential_reweight = TRUE
               )

             LAMBDA <-
               c(3, 5, 10, 15, 25, 50, 60,
                 75, 100, 150, 300, 450, 600)
             OMEGA <- c(.1, .2, .3, .4,
                        .5, .6, .7, .8, .9)

             gsearch <- expand.grid(LAMBDA, OMEGA)
             colnames(gsearch) <- c("lambda", "omega")

             n <- nrow(gsearch)
             par_loss <- vector("list", n)
             par_rmse <- vector("list", n)
             for (j in 1:n) {
               cat(j, "\n")
               ade@lambda <- gsearch[j, "lambda"]
               ade@omega  <- gsearch[j, "omega"]

               yh <- predict(ade, o$test)@y_hat

               par_rmse[[j]] <- rmse(o$test$target, yh)
             }
             list(par_rmse=par_rmse)
           })

  sens_analysis[[i]]  <- MC_ESTIMATES

  cat("\n\n")
  save(sens_analysis,
       file = paste0("ADE_SENS_", ids[1], ".rdata"))
}
names(sens_analysis)  <- names(embedded_time_series)

