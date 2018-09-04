load("data/embedded_data.rdata")

library(tsensembler)
library(forecast)
library(ranger)
library(opera)

ids <- 24

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
#ids <- 1:L
form <- target ~.
specs <- MODELSPECS

adddata_analysis <- vector("list", L)
for (i in seq_along(adddata_analysis)[ids]) {
  cat(i, "\n\n\n")
  cat(as.character(Sys.time()),"\n")

  set.seed(1234)
  id_res <-
    ADDING_DATA_PIPELINE(ds = embedded_time_series[[i]],
                         form = form,
                         specs = specs)

  adddata_analysis[[i]]  <- id_res

  names(adddata_analysis) <- names(embedded_time_series)
  save(adddata_analysis, file = paste0("ADE_R1_ID_", ids[1], ".rdata"))
}
