library(tsensembler)#install.packages("tsensembler")

setwd("../ArbitrageForecastingExperts/R1.Experiments/forecast_combination_experiments/")
source("sources/utils.R")
source("sources/src-summarystats.R")
source("sources/src-workflows.r")

load("./data/timeseries.rdata")

# estimating embedding dimension
# according to false nearest neighbors
max_k <- 30
min_k <- 6
k_hat <-
  sapply(ts_list,
         function(x) {
           tryCatch(estimate_k(x = x,m.max = max_k,tol = .01),
                    error = function(e) max_k)
         })

k_hat[k_hat < min_k] <- min_k

# Embedding time series
# and computing dynamics
embedded_time_series <- vector("list", length(ts_list))
for (i in seq_along(ts_list)) {
  K <- k_hat[[i]]
  X <- ts_list[[i]]

  X_embed <-
    embed_timeseries(timeseries = X,
                     embedding.dimension = K)

  target_col <- grep("target", colnames(X_embed))
  X_embed_attrs <- subset(X_embed, select = - target_col)

  dynamics_x <- embed_dynamics(X_embed_attrs)

  ds <- cbind.data.frame(X_embed, dynamics_x)

  embedded_time_series[[i]] <- ds
}

embedded_time_series_diff <- vector("list", length(ts_list))
for (i in seq_along(ts_list)) {
  K <- k_hat[[i]]
  X <- ts_list[[i]]
  X <- diff(X)[-1]

  X_embed <-
    embed_timeseries(timeseries = X,
                     embedding.dimension = K)

  target_col <- grep("target", colnames(X_embed))
  X_embed_attrs <- subset(X_embed, select = - target_col)

  dynamics_x <- embed_dynamics(X_embed_attrs)

  ds <- cbind.data.frame(X_embed, dynamics_x)

  embedded_time_series_diff[[i]] <- ds
}


names(embedded_time_series) <-
  names(embedded_time_series_diff) <-
  names(ts_list)

ord <- order(sapply(embedded_time_series,nrow))
embedded_time_series <- embedded_time_series[ord]
embedded_time_series_diff <- embedded_time_series_diff[ord]

save(embedded_time_series,
     embedded_time_series_diff,
     file = "./data/embedded_data.rdata")

#
