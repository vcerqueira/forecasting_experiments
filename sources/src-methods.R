#' ARIMA
#'
#' @param train train data
#' @param test test data
arima_workflow <-
  function(train, test) {
    require(forecast)
    tm_ids <- c(1,grep("Tm", colnames(train)))
    train <- subset(train, select = tm_ids)
    test <- subset(test, select = tm_ids)


    train <- train$target
    test  <- test$target

    model <- auto.arima(train)

    preds <- Arima(test, model=model)
    preds <- as.vector(fitted(preds))#as.vector(preds$fitted)

    preds
  }

arima_workflow_xreg <-
  function(train, test) {
    require(forecast)
    tm_ids <- c(1, grep("Tm", colnames(train)))
    train_xreg <- as.matrix(subset(train, select = -tm_ids))
    test_xreg <- as.matrix(subset(test, select = -tm_ids))

    train <- subset(train, select = tm_ids)
    test <- subset(test, select = tm_ids)

    train <- train$target
    test <- test$target

    model <- auto.arima(train, xreg = train_xreg)

    preds <- Arima(test, model = model, xreg = test_xreg)
    preds <- as.vector(fitted(preds))#as.vector(preds$fitted)

    preds
  }


#' Exponential smoothing
#'
#' @param train train data
#' @param test test data
expsmooth_workflow <-
  function(train, test) {
    require(forecast)
    tm_ids <- c(1,grep("Tm", colnames(train)))
    train <- subset(train, select = tm_ids)
    test <- subset(test, select = tm_ids)

    train <- train$target
    test <- test$target

    model <- ets(train)

    preds <- ets(test, model=model)
    preds <- as.vector(preds$fitted)

    preds
  }

#' Windowing - Selecting best model
#'
#' @param base_models predictive models. Object of class base_ensemble
#' @param newdata test data
#' @param lambda window size for computing recent performance
GEN.BLAST <-
  function(base_models, newdata, lambda = 50) {
    Y_hat <- predict(base_models, newdata)
    Y <- get_y(newdata, base_models@form)

    if (any(is.na(Y_hat))) {
      Y_hat <- soft_completion(Y_hat)
    }

    E <- base_models_loss(Y_hat, Y, se)

    moving_loss <- roll_mean_matrix(E, lambda = lambda)
    pre_W <- rep(1./base_models@N, times = base_models@N)

    W <-
      apply(moving_loss, 1,
            function(j) {
              proportion(normalize(-j, na.rm = TRUE), na.rm = TRUE)
            })

    W <- as.data.frame(t(W))
    W <- rbind.data.frame(pre_W, W[-NROW(W), ])
    W <- select_best(W)

    y_hat <- combine_predictions(Y_hat, W, committee = NULL)

    y_hat
  }


#' Elusive return predictability pipeline
#'
#' @param base_models predictive models. Object of class base_ensemble
#' @param newdata test data
#' @param lambda window size for computing recent performance
#' @param min_r2 minimum R squared required for combination
GEN.ERP <-
  function(base_models, newdata, lambda = 50, min_r2 = .1) {
    Y_hat <- predict(base_models, newdata)
    Y <- get_y(newdata, base_models@form)
    seq. <- seq_along(Y)

    R2 <-
      lapply(seq., function(j) {
        if (j <= lambda)
          in_seq <- seq_len(j)
        else
          in_seq <- (j-lambda-1):j

        as.data.frame(
          lapply(Y_hat[in_seq, ],
                 function(y_hat)
                   r_squared(Y[in_seq], y_hat)
          )
        )
      })

    R2 <- rbind_l(R2)
    R2[1, ] <- 1
    R2[is.na(R2)] <- 0.

    C <- apply(R2, 1, function(j) any(j > min_r2))

    prevailing_mean <-
      vnapply(seq.[-1], function(j) {
        if (j <= lambda + 1)
          in_seq <- seq_len(j) - 1
        else
          in_seq <- (j - lambda - 1):j - 1

        mean(Y[in_seq], na.rm = T)
      })

    prevailing_mean <- c(0, prevailing_mean)

    y_hat <- vnapply(seq., function(j) {
      if (C[[j]])
        yh <- mean(unlistn(Y_hat[j, ]), na.rm = TRUE)
      else
        yh <- prevailing_mean[j]

      yh
    })

    y_hat
  }


#' Simple average with trimming
#'
#' @param base_models predictive models. Object of class base_ensemble
#' @param newdata test data
GEN.SimpleTrim <-
  function(base_models, newdata, omega=.5) {
    Y <- get_y(newdata, base_models@form)
    Y_hat <- predict(base_models, newdata)
    seq. <- seq_along(Y)

    W <- vector("list", nrow(Y_hat) - 1)
    for (i in seq.[-1]) {
      mseq <- seq_len(i-1)
      W[[i]] <- apply(Y_hat, 2, function(j) rmse(j[mseq], Y[mseq]))
    }

    W <- rbind_l(W)
    W <- rbind(rep(1., time = ncol(Y_hat)), W)
    colnames(W) <- colnames(Y_hat)

    W <- t(apply(W, 1, model_weighting, trans="linear", na.rm=T))

    W <- as.data.frame(W)
    C <- get_top_models(W, omega = omega)

    y_hat <-
      vnapply(seq.,
              function(j) {
                 mean(unlist(Y_hat[j, C[[j]]]))
              })

    y_hat
  }


#' Combining according to loss in training set
#'
#' @param base_models predictive models. Object of class base_ensemble
#' @param newdata test data
GEN.LossTrain <-
  function(base_models, newdata) {
    Y_hat <- predict(base_models, newdata)

    W_0 <- base_models@pre_weights

    W <- rep(list(W_0), times = nrow(newdata))
    W <- rbind_l(W)
    colnames(W) <- colnames(Y_hat)

    y_hat <- combine_predictions(Y_hat, W, committee = NULL)

    y_hat
  }


#' Random weights for sanity check
#'
#' @param base_models predictive models. Object of class base_ensemble
#' @param newdata test data
GEN.RandomWeights <-
  function(base_models, newdata) {
    Y_hat <- predict(base_models, newdata)

    dim_hats <- dim(Y_hat)
    size <- Reduce('*', dim_hats)

    W <- matrix(runif(size), nrow = dim_hats[1], ncol = dim_hats[2])
    colnames(W) <- colnames(Y_hat)
    W <- t(apply(W, 1, proportion))

    y_hat <- combine_predictions(Y_hat, W, committee = NULL)

    y_hat
  }



#' Simple average
#'
#' @param base_models predictive models. Object of class base_ensemble
#' @param newdata test data
#'
#' @export
GEN.Simple <-
  function(base_models, newdata) {
    Y_hat <- predict(base_models, newdata)

    rowMeans(Y_hat)
  }




#' ADE in runtime with updates in the meta-models
#'
#' @param base_models predictive models. Object of class base_ensemble
#' @param newdata test data
#' @param lambda window size for computing recent performance
#' @param omega ratio of predictive models picked at each point
#' @param warmup warmup for enough predictions from the base_models before meta-learning
#' @param step no. of periods for updating the meta-model, i.e. meta-models
#' are updated every step observations
GEN.ADE_runtime <-
  function(base_models,
           newdata,
           lambda = 50,
           omega = .5,
           warmup = 50,
           steps = 25) {

    Y_hat <- predict(base_models, newdata)
    Y <- get_y(newdata, base_models@form)
    target <- get_target(base_models@form)

    E <- base_models_loss(Y_hat, Y, ae)

    testcols <- setdiff(colnames(newdata), target)
    testset <- subset(newdata, select = testcols)

    metadata <-
      lapply(E, function(o) {
        cbind.data.frame(testset, score = o)
      })

    seq. <- seq_len(nrow(newdata) - 1L)
    seq._ <- seq.[-seq_len(warmup)]

    E_hat <- point.metalearn.rf(metadata, seq._, steps = steps)

    C <- build_committee(Y_hat, Y, lambda = lambda, omega = omega)
    C_pre_warm <- C[-seq_len(warmup + 1L)]

    W <- matrix(0., ncol = ncol(Y_hat), nrow = length(C_pre_warm))
    for (j in 1:nrow(W)) {
      W_j <- E_hat[j, C_pre_warm[[j]]]
      W_j <- model_weighting(W_j, "linear")

      W[j,  C_pre_warm[[j]]] <- W_j
      W[j, -C_pre_warm[[j]]] <- 0.
    }
    colnames(W) <- colnames(Y_hat)

    ssimilarity <- sliding_similarity(Y_hat, lambda)
    W[-seq_len(lambda),] <- sequential_reweighting(ssimilarity, W[-seq_len(lambda),])

    W <- rbind(matrix(1 / ncol(Y_hat),
                      nrow = warmup + 1L,
                      ncol = ncol(Y_hat)), W)


    y_hat <- combine_predictions(Y_hat, W, C)

    y_hat
  }


#' Arbitrating - original
#'
#' @param base_models predictive models. Object of class base_ensemble
#' @param newdata test data
#' @param warmup warmup for enough predictions from the base_models before meta-learning
#' @param step no. of periods for updating the meta-model, i.e. meta-models
#' are updated every step observations
GEN.Arbitrating <-
  function(base_models, newdata, warmup = 5, steps = 25) {
    Y_hat <- predict(base_models, newdata)
    Y <- get_y(newdata, base_models@form)
    target <- get_target(base_models@form)

    E <- base_models_loss(Y_hat, Y, ae)

    testcols <- setdiff(colnames(newdata), target)
    testset <- subset(newdata, select = testcols)

    metadata <-
      lapply(E, function(o) {
        cbind.data.frame(testset, score = o)
      })

    seq. <- seq_len(nrow(newdata) - 1L)
    seq._ <- seq.[-seq_len(warmup)]

    E_hat <- point.metalearn.rf(metadata, seq._, steps = steps)

    W <- (t(apply(E_hat, 1, model_weighting,trans = "linear", na.rm = TRUE)))
    W <- rbind(matrix(1 / ncol(Y_hat),
                      nrow = warmup + 1L,
                      ncol = ncol(Y_hat)), W)

    W <- select_best(W)

    y_hat <- combine_predictions(Y_hat, W, NULL)

    y_hat
  }



#' Picking the best model in the training set
#'
#' @param base_models predictive models. Object of class base_ensemble
#' @param newdata test data
#' @param trainset train data
GEN.BestTrain <-
  function(base_models, newdata, trainset) {
    Y_hat_tr <- predict(base_models, trainset)
    Y_tr <- get_y(trainset, base_models@form)
    E_tr <- base_models_loss(Y_hat_tr, Y_tr, ae)

    best_id <- which.min(colMeans(E_tr))

    Y_hat <- predict(base_models, newdata)

    y_hat <- Y_hat[,best_id]

    y_hat
  }

#' Ridge model from opera
#'
#' @param base_models predictive models. Object of class base_ensemble
#' @param newdata test data
#'
#' @import opera
GEN.Ridge <-
  function(base_models, newdata) {
    Y_hat <- predict(base_models, newdata)
    Y <- get_y(newdata, base_models@form)

    if (any(is.na(Y_hat))) {
      Y_hat <- soft.completion(Y_hat)
    }

    Ridge_mix <- mixture(model = "Ridge", loss.type = "square")
    for (i in 1:length(Y)) {
      Ridge_mix <- predict(Ridge_mix, newexperts = Y_hat[i, ], newY = Y[i])
    }

    W <- Ridge_mix$weights

    combine_predictions(Y_hat, W, committee = NULL)
  }


#' MLpol model from opera
#'
#' @param base_models predictive models. Object of class base_ensemble
#' @param newdata test data
#' @param lambda window size for sequential reweighting
#' @param sequential_reweight logical. apply sequential reweighting to predictions
#'
#' @import opera
GEN.MLpol <-
  function(base_models,newdata,lambda=50, sequential_reweight = FALSE) {
    Y_hat <- predict(base_models, newdata)
    Y <- get_y(newdata, base_models@form)

    MLpol0 <- mixture(model = "MLpol", loss.type = "absolute")
    for (i in 1:length(Y)) {
      MLpol0 <- predict(MLpol0, newexperts = Y_hat[i,], newY = Y[i])
    }

    W <- as.matrix(MLpol0$weights)

    if (sequential_reweight) {
      ssimilarity <- sliding_similarity(Y_hat, lambda)
      ssimilarity <- c(as.list(rep(NA, times=lambda)), ssimilarity)
      W <- sequential_reweighting(ssimilarity, W)
    }

    combine_predictions(Y_hat, W, committee = NULL)
  }


#' Forecaster Combination
#' Model according to the guidelines
#' of Sanchez in adaptive combination of forecasts
#'
#' @param base_models predictive models. Object of class base_ensemble
#' @param newdata test data
#' @param ff forgetting factor
#' @param lambda window size for sequential reweighting
#' @param sequential_reweight logical. apply sequential reweighting to predictions
GEN.AEC <-
  function(base_models, newdata, ff = .95, lambda = 50,sequential_reweight=F) {
    Y_hat <- predict(base_models, newdata)
    Y <- get_y(newdata, base_models@form)
    E <- base_models_loss(Y_hat, Y)

    if (any(is.na(Y_hat))) {
      Y_hat <- soft.completion(Y_hat)
    }

    W <-
      lapply(seq_len(base_models@N),
             function(o) {
               y_hat <- Y_hat[, o]
               e_y <- E[, o]

               r <-
                 vnapply(seq_along(y_hat)[-(1:2)],
                         function(y_w) {
                           var_y <- var(y_hat[seq_len(y_w - 1)], na.rm = T)
                           v <- (1 / sqrt(var_y))

                           v * exp(-((e_y[y_w - 1] ^ 2) / (2 * var_y))) * ff
                         })
               c(1., 1., r)
             })

    W <- as.data.frame(W)
    colnames(W) <- colnames(Y_hat)

    bad_m <- which(sapply(W, function(j) any(is.na(j))))

    if (length(bad_m) > 0) {
      W <- W[,-bad_m]
      Y_hat <- Y_hat[,-bad_m]
    }

    if (sequential_reweight) {
      ssimilarity <- sliding_similarity(Y_hat, lambda)
      ssimilarity <- c(as.list(rep(NA, times=lambda)), ssimilarity)
      W <- sequential_reweighting(ssimilarity, W)
    }

    y_hat <- combine_predictions(Y_hat, W, committee = NULL)

    y_hat
  }


#' Windowing - weight according to loss in recent observations
#'
#' @param base_models predictive models. Object of class base_ensemble
#' @param newdata test data
#' @param lambda window size for sequential reweighting
#' @param sequential_reweight logical. apply sequential reweighting to predictions
GEN.WindowLoss <-
  function(base_models,newdata,lambda = 50,sequential_reweight=F) {

    Y_hat <- predict(base_models, newdata)
    Y <- get_y(newdata, base_models@form)
    E <- base_models_loss(Y_hat, Y, se)

    moving_loss <- roll_mean_matrix(E, lambda = lambda)
    pre_W <- rep(1. / base_models@N, times = base_models@N)

    W <-
      apply(moving_loss, 1,
            function(j) {
              proportion(normalize(-j, na.rm = TRUE), na.rm = TRUE)
            })

    W <- as.data.frame(t(W))
    W <- rbind.data.frame(pre_W, W[-NROW(W),])
    W <- as.matrix(W)

    if (sequential_reweight) {
      ssimilarity <- sliding_similarity(Y_hat, lambda)
      ssimilarity <- c(as.list(rep(NA, times=lambda)), ssimilarity)
      W <- sequential_reweighting(ssimilarity, W)
    }

    combine_predictions(Y_hat, W, committee = NULL)
  }


#' EWA model from opera
#'
#' @param base_models predictive models. Object of class base_ensemble
#' @param newdata test data
#' @param lambda window size for sequential reweighting
#' @param sequential_reweight logical. apply sequential reweighting to predictions
#'
#' @import opera
GEN.EWA <-
  function(base_models, newdata, lambda=50,sequential_reweight=F) {
    Y_hat <- predict(base_models, newdata)
    Y <- get_y(newdata, base_models@form)

    if (any(is.na(Y_hat))) {
      Y_hat <- soft.completion(Y_hat)
    }

    EWA_mix <- mixture(model = "EWA", loss.type = "absolute")
    for (i in 1:length(Y)) {
      EWA_mix <- predict(EWA_mix, newexperts = Y_hat[i, ], newY = Y[i])
    }

    W <- as.matrix(EWA_mix$weights)

    if (sequential_reweight) {
      ssimilarity <- sliding_similarity(Y_hat, lambda)
      ssimilarity <- c(as.list(rep(NA, times=lambda)), ssimilarity)
      W <- sequential_reweighting(ssimilarity, W)
    }

    combine_predictions(Y_hat, W, committee = NULL)
  }


#' Fixed share model from opera
#'
#' @param base_models predictive models. Object of class base_ensemble
#' @param newdata test data
#' @param lambda window size for sequential reweighting
#' @param sequential_reweight logical. apply sequential reweighting to predictions
#'
#' @import opera
GEN.FixedShare <-
  function(base_models, newdata,lambda=50,sequential_reweight=F) {
    Y_hat <- predict(base_models, newdata)
    Y <- get_y(newdata, base_models@form)

    if (any(is.na(Y_hat))) {
      Y_hat <- soft.completion(Y_hat)
    }


    FS_mix <- mixture(model = "FS", loss.type = "absolute")
    for (i in 1:length(Y)) {
      FS_mix <- predict(FS_mix, newexperts = Y_hat[i, ], newY = Y[i])
    }

    W <- as.matrix(FS_mix$weights)

    if (sequential_reweight) {
      ssimilarity <- sliding_similarity(Y_hat, lambda)
      ssimilarity <- c(as.list(rep(NA, times=lambda)), ssimilarity)
      W <- sequential_reweighting(ssimilarity, W)
    }

    combine_predictions(Y_hat, W, committee = NULL)
  }



#' OGD model from opera
#'
#' @param base_models predictive models. Object of class base_ensemble
#' @param newdata test data
#' @param lambda window size for sequential reweighting
#' @param sequential_reweight logical. apply sequential reweighting to predictions
#'
#' @import opera
GEN.OGD <-
  function(base_models, newdata, lambda=50, sequential_reweight=F) {
    Y_hat <- predict(base_models, newdata)
    Y <- get_y(newdata, base_models@form)

    if (any(is.na(Y_hat))) {
      Y_hat <- soft.completion(Y_hat)
    }

    OGD_mix <- mixture(model = "OGD", loss.type = "absolute")
    for (i in 1:length(Y)) {
      OGD_mix <- predict(OGD_mix, newexperts = Y_hat[i, ], newY = Y[i])
    }

    W <- as.matrix(OGD_mix$weights)

    if (sequential_reweight) {
      ssimilarity <- sliding_similarity(Y_hat, lambda)
      ssimilarity <- c(as.list(rep(NA, times=lambda)), ssimilarity)
      W <- sequential_reweighting(ssimilarity, W)
    }

    combine_predictions(Y_hat, W, committee = NULL)
  }


#' Predict method for "ADE-vanilla"
#'
#' @param object object of class ADE
#' @param newdata test data
predict_vanilla <-
  function(object, newdata) {
    data_size <- nrow(newdata)
    seq. <- seq_len(data_size)
    N <- object@base_ensemble@N

    Y_hat <- predict(object@base_ensemble, newdata)
    Y_hat_recent <- predict(object@base_ensemble, object@recent_series)
    Y_hat_recent <- utils::tail(Y_hat_recent, object@lambda)

    Y_hat_extended <- rbind.data.frame(Y_hat_recent, Y_hat)

    Y <- get_y(newdata, object@form)

    E_hat <-
      lapply(object@meta_model,
             function(o) {
               meta_predict(o, newdata, "randomforest")
             })
    names(E_hat) <- colnames(Y_hat)

    E_hat <- as.data.frame(E_hat)

    Y_rs <- get_y(object@recent_series, object@form)
    Y_rs <- utils::tail(Y_rs, object@lambda)

    if (!object@all_models) {
      Y_rs <- get_y(object@recent_series, object@form)
      Y_rs <- utils::tail(Y_rs, object@lambda)

      C <-
        build_committee(
          rbind.data.frame(Y_hat_recent, Y_hat),
          c(Y_rs, Y),
          lambda = object@lambda,
          omega = object@omega)

      C <- C[-seq_len(object@lambda)]
      C <- lapply(C, unname)
    } else
      C <- NULL

    YH_shifted <- Y_hat + E_hat

    y_hat <-
      vnapply(seq.,
              function(j) {
                rowMeans(YH_shifted[j, C[[j]]])
              })

    y_hat
  }

#' @export
generalizer_ade <-
  function(base_models,
           traindata,
           newdata,
           specs,
           lfun = ae,
           lambda = 50,
           omega = .5,
           nfolds = 10) {
    form <- base_models@form
    Y <- get_y(newdata, form)
    tgt <- get_target(form)

    cat("Setting up meta data\n")
    OOB_data <-
      blocked_prequential(
        x = traindata,
        nfolds = nfolds,
        intraining_estimations,
        .rbind = FALSE,
        form = form,
        specs = specs,
        lfun = lfun
      )

    OOB <- rbind_l(lapply(OOB_data, function(x) x$oob))
    train_loss <- rbind_l(lapply(OOB_data, function(x) x$mloss))
    recent_series <- recent_lambda_observations(OOB, lambda)

    OOB <- subset(OOB, select = -which(colnames(OOB) %in% tgt))
    train_metadata <-
      lapply(train_loss,
             function(l_hat) cbind.data.frame(OOB,
                                              score = l_hat))

    meta_models <-
      lapply(train_metadata,
             function(meta_set) {
               if (any(is.na(meta_set))) {
                 meta_set <- soft.completion(meta_set)
               }
               loss_meta_learn(score ~ ., meta_set, "randomforest")
             })


    ############# preds

    data_size <- nrow(newdata)
    seq. <- seq_len(data_size)
    N <- base_models@N

    Y_hat <- predict(base_models, newdata)
    Y_hat_recent <- predict(base_models, recent_series)
    Y_hat_recent <- tail(Y_hat_recent, lambda)

    Y_hat_extended <- rbind.data.frame(Y_hat_recent, Y_hat)

    E_hat <- lapply(meta_models,
                    function(o) {
                      meta_predict(o, newdata, "randomforest")
                    })
    names(E_hat) <- colnames(Y_hat)
    E_hat <- abs(as.data.frame(E_hat))

    Y_rs <- get_y(recent_series, form)

    C <-
      build_committee(
        rbind.data.frame(Y_hat_recent, Y_hat),
        c(Y_rs, Y),
        lambda = lambda,
        omega = omega
      )

    C <- C[-seq_len(lambda)]
    C <- lapply(C, unname)

    W <- matrix(0., ncol = N, nrow = data_size)
    for (j in seq.) {
      W_j <- E_hat[j, C[[j]]]
      W_j <- model_weighting(W_j, "linear")

      W[j,  C[[j]]] <- W_j
      W[j, -C[[j]]] <- 0.
    }
    colnames(W) <- colnames(Y_hat)

    ssimilarity <- sliding_similarity(Y_hat_extended, lambda)
    W <- sequential_reweighting(ssimilarity, W)

    y_hat <- combine_predictions(Y_hat, W, C)

    y_hat
  }

#' n pipe
#'
#' @param train tr
#' @param test ts
naive_workflow <-
  function(train, test) {
    require(forecast)

    train <- train$target
    test <- test$target

    seq. <- seq_along(test)
    trainset <- train
    y_hat <- vnapply(seq., function(j) {
      trainset <- c(trainset, test[seq_len(j) - 1])
      pforecast <- naive(trainset,1)$mean

      as.vector(pforecast)
    })
    y_hat
  }




#' Seasonal naive pipeline
#' if daily, use past week
#' if hourly, use past day
#'
#' @param train train data
#' @param test test data
man_snaive_workflow <-
  function(train, test) {
    tm_ids <- c(1,grep("Tm", colnames(train)))
    train <- subset(train, select = tm_ids)
    test <- subset(test, select = tm_ids)

    x <- head(train)
    x <- as.POSIXct(rownames(x))
    xdiff <- abs(difftime(x[1],x[2],units="hours"))[[1]]

    if (xdiff > 3) {
      p <- 7
    } else {
      p <- 24
    }

    n <- nrow(train)
    seq. <- n + seq_len(nrow(test))

    ds <- rbind.data.frame(train, test)

    preds <- numeric(nrow(test))
    for (i in seq.) {
      preds[i] <- ds[i - p,"target"]
    }
    tail(preds, nrow(test))
  }



#' stacking
#'
#' stacked generaliza.
#'
#' @param form form
#' @param train train data
#' @param specs specs
#'
#' @import ranger
#'
#' @export
stacking_train <-
  function(form, train, specs) {
    M <- build_base_ensemble(form, train, specs)

    OOB_hat <-
      blocked_prequential(
        x = train,
        nfolds = 10,
        intraining_predictions,
        .rbind = FALSE,
        form = form,
        specs = specs)
    
    OOB_hat <-
      rbind_l(lapply(OOB_hat, function(o) {
        o$test_ids <- NULL
        o
      })) 

    colnames(OOB_hat)[-1] <- names(M@base_models)

    if (any(is.na(OOB_hat))) {
      cat("imputing nas\n")
      OOB_hat <- soft.completion(OOB_hat)
    }

    generalizer <-
      ranger(
        target ~ .,
        OOB_hat,
        num.trees = 500,
        mtry = ncol(OOB_hat) / 3,
        write.forest = TRUE)

    list(base_ensemble = M, generalizer = generalizer)
  }

#' predicting with stacking
#'
#' @param base_ensemble bas en
#' @param generalizer metamodel
#' @param newdata new data
#'
#' @export
stacking_predicting <-
  function(base_ensemble, generalizer, newdata) {
    Y_hat <- predict(base_ensemble, newdata)

    Y_hat <- cbind.data.frame(target = -1, Y_hat)

    if (any(is.na(Y_hat))) {
      Y_hat <- soft.completion(Y_hat)
    }

    predict(generalizer, Y_hat)$predictions
  }

