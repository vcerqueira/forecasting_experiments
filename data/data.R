#devtools::install_github("vcerqueira/tsensembler") # please install dev version
library(tsensembler)#ADE
library(forecast)#forecast
library(softImpute)#NAs
library(tseriesChaos)#embedding dimension
library(ranger)#random forests
library(reshape2)#munging data
library(ggplot2)#plots
library(opera)#forecast combinations
library(caret)#pca
library(moments)#statistics
library(DMwR)#statistics
library(nonlinearTseries)#statistics
library(Rwave)#statistics

source("sources/utils.R")
source("sources/src-summarystats.R")
source("sources/src-workflows.r")

load("./data/timeseries.rdata")

# estimating embedding dimension
# according to false nearest neighbors
max_k <- 30
min_k <- 8
k_hat <-
  sapply(ts_list,
         function(x) {
           tryCatch(estimate_k(x = x,m.max = max_k,tol = .01),
                    error = function(e) max_k)
         })

k_hat[k_hat < min_k] <- min_k


# trend inclusion
n_diffs <- sapply(ts_list, ndiffs)

# Embedding time series
# and computing dynamics
embedded_time_series <- vector("list", length(ts_list))
for (i in seq_along(ts_list)) {
  K <- k_hat[[i]]
  X <- ts_list[[i]]
  if (n_diffs[i] > 0 ) {
    X <- diff(X)[-1]
  }

  X_embed <-
    embed_timeseries(timeseries = X,
                     embedding.dimension = K)

  target_col <- grep("target", colnames(X_embed))
  X_embed_attrs <- subset(X_embed, select = - target_col)

  dynamics_x <- embed_dynamics(X_embed_attrs)

  ds <- cbind.data.frame(X_embed, dynamics_x)

  embedded_time_series[[i]] <- ds
}


names(embedded_time_series) <- names(ts_list)
ord <- order(sapply(embedded_time_series,nrow))
embedded_time_series <- embedded_time_series[ord]
           
embedded_time_series <-
  lapply(embedded_time_series,
         function(x) {
           rownames(x) <-
             seq.Date(from = Sys.Date(),
                      length.out = nrow(x),
                      by = 1)

           x
         })

save(embedded_time_series, file = "./data/embedded_data.rdata")
#
