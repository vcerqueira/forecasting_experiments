#' Soft Imputation
#'
#' @param x embedded time series
soft_completion <- function(x) {
  require(softImpute)

  if ("data.frame" %in% class(x))
    x <- as.matrix(x)

  as.data.frame(complete(x, softImpute(x)))
}


#' pointwise metalearn with random forest
#'
#' @param metadata metadata from \code{setup.metadata}
#' @param timeseq time sequence of test set
#' @param steps metamodels are retrained every \code{steps}
#'
#' @import ranger
#' @export
point.metalearn.rf <- function(metadata, timeseq, steps) {
  len <- length(timeseq)
  oM_hat <- lapply(metadata, function(metaset) {
    retrain.time <- seq(from = timeseq[1], to = timeseq[len], by = steps)
    om_yhat <- numeric(len)
    for (j in seq_along(timeseq)) {
      N_j <- timeseq[j]
      if (N_j %in% retrain.time) {
        if (any(is.na(metaset))) {
          metaset <- soft_completion(metaset)
        }

        capture.output(
          metamodel <-
            ranger(
              score ~ .,
              metaset[seq_len(N_j),],
              num.trees = 500,
              mtry = ncol(metaset) / 3,
              write.forest = TRUE,
              importance = 'impurity'
            )
        )
      }
      om_yhat[j] <- predict(metamodel, metaset[N_j + 1L, ])$predictions
    }
    om_yhat
  })
  as.data.frame(oM_hat)
}

#' Estimating Embedding Dimension using False Nearest Neighbors
#'
#' @param x time series
#' @param max_k maximum embedding dimension
#'
#' @export
estimate_k <-
  function(x, m.max=20,tol=.15) {
    require(tseriesChaos)

    fn.out <- false.nearest(x, m.max, d=1, t=1)
    fn.out <- round(fn.out,4)
    fn.out[is.na(fn.out)] <- 0
    #plot(fn.out)

    fnp.tol <- fn.out["fraction",] > tol
    fnp.tol.sum <- sum(fnp.tol)

    m <- ifelse(fnp.tol.sum < m.max,fnp.tol.sum + 1, m.max)

    m
  }


extended_weights <-
  function(W, C) {
    stopifnot(NROW(W) == length(C))

    seq. <- seq_len(NROW(W))

    weightsf <-
      vapply(seq.,
             function(j) {
               W[j, C[[j]]] <- proportion(W[j, C[[j]]])
               W[j, -C[[j]]] <- 0.
               W[j,]
             }, double(NCOL(W)))

    t(weightsf)
  }


#' R squared
#'
#' Also known as the coefficient of determination
r_squared <- function(y, y_hat) {
  1 - (sum((y - y_hat)^2, na.rm=TRUE) / sum((y - mean(y, na.rm=TRUE))^2, na.rm=TRUE))
}

err <- function(y, y_hat, ...) y - y_hat


cv.folds <- function(x, nfolds) {
  cut(seq_len(NROW(x)), breaks = nfolds, labels = FALSE)
}

prequential_in_blocks_samples <-
  function(x, nfolds=10) {
    f <- cv.folds(x, nfolds)

    cv.res <- vector("list", nfolds-1)
    seq. <- seq_len(nfolds)
    for (i in seq.[-length(seq.)]) {
      tr.id <- which(f %in% seq_len(i))
      ts.id <- which(f == i + 1L)

      cv.res[[i]]$train <- x[tr.id,]
      cv.res[[i]]$test  <- x[ts.id,]
    }
    cv.res
  }

#' monte carlo apprx.
#'
#' @param x time series dataset
#' @param resamples no of simulations
#' @param size_estimation size of training set
#' @param size_validation size of test set
resample_timeseries <-
  function (x,
            resamples,
            size_estimation,
            size_validation) {
    n <- nrow(x)
    tr_size <- as.integer(n * size_estimation)
    ts_size <- as.integer(n * size_validation)
    selection_range <- (tr_size + 1):(n - ts_size + 1)
    origins <- sample(selection_range, resamples)
    lapply(origins, function(o) {
      list(train = x[(o - tr_size):(o - 1),], test = x[o:(o + ts_size - 1),])
    })
  }


calc_time <-
  function(x) {
    if (attr(x,"units") == "mins") {
      r <- as.vector(x) * 60
    } else {
      r <- as.vector(x)
    }
    r
  }

 
replace_inf <- 
  function (df)  {
    do.call(data.frame, lapply(df, function(j) {
        replace(j, is.infinite(j), NA)
    }))
}


log_trans <- function(x) sign(x) * log(abs(x) + 1)

percentual_difference <-
  function(x,y) {
    ((x - y) / abs(y)) * 100
  }

