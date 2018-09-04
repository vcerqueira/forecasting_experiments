#' Computing time series dynamics
#'
#' @param x embedded numeric time series with removed target
embed_dynamics <-
  function(x) {
    require(caret)
    require(moments)
    require(DMwR)

    dim_ts <- dim(x)

    cat("Computing trend ...\n")
    ts_trend <- apply(x, 1, trend)

    cat("Computing skewness ...\n")
    ts_skew <- apply(x, 1, moments::skewness)

    #cat("Computing kurtosis ...\n")
    #ts_kts <- apply(x, 1, moments::kurtosis)

    cat("Computing mean ...\n")
    ts_mean <- rowMeans(x)

    cat("Computing standard deviation ...\n")
    ts_stddev <- apply(x, 1, sd)

    cat("Computing maximum lyapunov exponent ...\n")
    ts_mle <- apply(x, 1, function(o) {
      tryCatch(max_lyapunov_exp(o), error = function(e) NA)
    })

    cat("Computing hurst ...\n")
    ts_hurst <- apply(x, 1,
                      function(o) {
                        tryCatch(HURST(o), error = function(e) NA)
                      })

    cat("Computing serial correlation ...\n")
    ts_serialcorr <- apply(x, 1,
                           function(j) {
                             tryCatch(Box.test(j)$p.val,
                                      error = function(e) NA)
                           })

    #cat("Computing ratio sma ema ...\\n")
    #ts_r_sema <- tryCatch(apply(x, 1, tsbox::r_sma_ema),
    #                      error = function(e) rep(1, times=nrow(x)))


    ts_dyns <-
      data.frame(trend = ts_trend,
                 skew = ts_skew,
                 #kts = ts_kts,
                 mean = ts_mean,
                 stddev = ts_stddev,
                 mle = ts_mle,
                 hurst = ts_hurst,
                 #ts_r_sema=ts_r_sema,
                 serialcorr = ts_serialcorr)
    ts_dyns1<<-ts_dyns

    ts_dyns <- replace_inf(ts_dyns)

    has_na <- DMwR::manyNAs(t(ts_dyns), .4)

    if (length(has_na) > 0) {
      ts_dyns <- subset(ts_dyns, select = -has_na)
    }

    ts_dyns <- soft_completion(ts_dyns)

    nzv_cols <- caret::nearZeroVar(ts_dyns)
    if (length(nzv_cols) > 0L) {
      ts_dyns <- subset(ts_dyns, select = -nzv_cols)
    }

    rownames(ts_dyns) <- NULL

    ts_dyns
  }

#' Trend/acceleration of a vector
#'
#' std / diffed std
#'
#' @param x numeric vector
trend <-
  function(x) {
    sd(x) / sd(diff(x)[-1])
  }


max_lyapunov_exp <-
  function(x) {
    require(nonlinearTseries)

    len <- length(x)
    Reduce(max,
           nonlinearTseries::divergence(
             nonlinearTseries::maxLyapunov(
               time.series = x,
               min.embedding.dim = ceiling(len / 4),
               max.embedding.dim = ceiling(len / 2),
               radius = ceiling(len / 6),
               do.plot = FALSE
             )
           ))
  }

#' Hurst exponent
#'
#' @param x numeric vector
HURST <-
  function(x) {
    require(Rwave)

    cwtwnoise <- Rwave::DOG(x, 10, 3, 1, plot = FALSE)
    mcwtwnoise <- Mod(cwtwnoise)
    mcwtwnoise <- mcwtwnoise * mcwtwnoise
    wspwnoise <- Rwave::tfmean(mcwtwnoise, plot = FALSE)

    Rwave::hurst.est(wspwnoise, 1:7, 3, plot = FALSE)[[2]]
  }





