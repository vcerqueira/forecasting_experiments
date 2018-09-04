#' Main pipeline
#'
#' @param train training set
#' @param test test set
#' @param form formula
#' @param specs model specs
MAIN_PIPELINE <-
  function(train, test, form, specs) {
    require(ranger)
    require(forecast)
    require(tsensembler)
    require(opera)

    Y <- get_y(test, form)
    Y_tr <- get_y(train, form)

    cat("Fitting ADE\n")
    set.seed(1234)
    ade <-
      ADE(
        form = form,
        data = train,
        specs = specs,
        lambda = 50,
        omega = .5,
        select_best = FALSE,
        all_models = FALSE,
        aggregation = "linear",
        sequential_reweight = TRUE
      )

    cat("Fit ADE vanilla\n")
    set.seed(1234)
    ade_van <-
      ADE(
        form = form,
        data = train,
        specs = specs,
        lambda = 50,
        omega = .5,
        select_best = FALSE,
        all_models = FALSE,
        aggregation = "linear",
        sequential_reweight = TRUE,
        meta_loss_fun = err
      )

    preds_ADE_van <- predict_vanilla(ade_van, test)

    # Base predictors
    M <- ade@base_ensemble
    base_Y_hat <- predict(M, test)
    loss_predictors <-
      sapply(base_Y_hat,
             function(x) {
               rmse(Y,x)
             })

    preds_ADE  <- predict(ade, test)@y_hat

    ###
    # Select best
    ade@select_best <- TRUE
    preds_ade_SB <- predict(ade, test)@y_hat
    # Select all
    ade@select_best <- FALSE
    ade@all_models <- TRUE
    preds_ade_ALLM <- predict(ade, test)@y_hat

    # ECML
    ade@select_best <- FALSE
    ade@all_models <- FALSE
    ade@aggregation <- "softmax"
    ade@sequential_reweight <- FALSE
    preds_ade_V0 <- predict(ade, test)@y_hat

    # NO SR
    ade@select_best <- FALSE
    ade@all_models <- FALSE
    ade@aggregation <- "linear"
    ade@sequential_reweight <- FALSE
    preds_ade_noSR <- predict(ade, test)@y_hat

    cat("State of the Art:\n")
    cat("Analysing Stacking...\n")
    mstacking_feats <- stacking_train(form, train, specs)
    preds_stacking <- stacking_predicting(mstacking_feats[[1]],
                                          mstacking_feats[[2]], test)

    #cat("Analysing ADE runtime...\n")
    ##preds_ADE_runtime <- GEN.ADE_runtime(M, test)

    cat("Analysing AEC...\n")
    preds_AEC <-
      GEN.AEC(base_models = M,
              newdata = test,
              ff = .9,
              lambda=ade@lambda)

    cat("Analysing Windowing...\n")
    preds_WindowLoss <-
      GEN.WindowLoss(base_models = M,
                     newdata = test,
                     lambda=ade@lambda)

    cat("Analysing ERP...\n")
    preds_ERP <-
      GEN.ERP(base_models = M,
              newdata = test,
              lambda = ade@lambda,
              min_r2 = .1)

    cat("Analysing Simple...\n")
    preds_Simple <-
      GEN.Simple(base_models = M, newdata = test)

    cat("Analysing Arbitrating...\n")
    preds_Arbitrating <- GEN.Arbitrating(M, test)

    cat("Analysing best in train\n")
    preds_BestTR <-
      GEN.BestTrain(base_models = M,
                    newdata = test,
                    trainset = train)

    cat("Analysing blast\n")
    preds_Blast <-
      GEN.BLAST(base_models = M,
                newdata = test,
                lambda = ade@lambda)

    cat("Analysing Simple trim\n")
    preds_trimmedSimple <-
      GEN.SimpleTrim(base_models = M,
                     newdata = test,
                     omega = ade@omega)

    cat("Analysing TrainLoss \n")
    preds_LossTrain <-
      GEN.LossTrain(base_models = M, newdata = test)

    cat("Analysing MLpol\n")
    preds_MLpol <-
      GEN.MLpol(base_models = M, newdata = test)

    cat("Analysing EWA\n")
    preds_EWA <-
      GEN.EWA(base_models = M, newdata = test)

    cat("Analysing FS\n")
    preds_FixedShare <-
      tryCatch(
        GEN.FixedShare(base_models = M,
                       newdata = test),
        error = function(e) preds_Simple)

    cat("Analysing OGD\n")
    preds_OGD <-
      GEN.OGD(base_models = M,
              newdata = test)

    cat("Analysing SNAIVE...\n")
    preds_SNAIVE <- man_snaive_workflow(train, test)

    cat("Analysing ARIMA...\n")
    preds_ARIMA <-
      tryCatch(arima_workflow(train, test),
               error = function(e) preds_SNAIVE)


    cat("Analysing ETS...\n")
    preds_ETS <- expsmooth_workflow(train, test)

    cat("Analysing NAIVE...\n")
    preds_NAIVE <- naive_workflow(train, test)

    ############################################################
    cat("RMSE loss\n")
    rmse_ADE         <- rmse(Y, preds_ADE)
    rmse_ADE_van     <- rmse(Y, preds_ADE_van)
    rmse_stacking    <- rmse(Y, preds_stacking)
    rmse_AEC         <- rmse(Y, preds_AEC)
    #rmse_ADE_runtime <- rmse(Y, preds_ADE_runtime)
    rmse_WindowLoss  <- rmse(Y, preds_WindowLoss)
    rmse_ERP         <- rmse(Y, preds_ERP)
    rmse_Simple      <- rmse(Y, preds_Simple)
    rmse_BestTR      <- rmse(Y, preds_BestTR)
    rmse_Blast       <- rmse(Y, preds_Blast)
    rmse_trimmedSimple  <- rmse(Y, preds_trimmedSimple)
    rmse_LossTrain      <- rmse(Y, preds_LossTrain)
    rmse_MLpol          <- rmse(Y, preds_MLpol)
    rmse_Arbit          <- rmse(Y, preds_Arbitrating)
    rmse_EWA            <- rmse(Y, preds_EWA)
    rmse_FixedShare     <- rmse(Y, preds_FixedShare)
    rmse_OGD            <- rmse(Y, preds_OGD)
    rmse_ARIMA          <- rmse(Y, preds_ARIMA)
    rmse_ETS            <- rmse(Y, preds_ETS)
    rmse_NAIVE          <- rmse(Y, preds_NAIVE)
    rmse_SNAIVE         <- rmse(Y, preds_SNAIVE)
    rmse_ade_SB         <- rmse(Y, preds_ade_SB)
    rmse_ade_ALLM       <- rmse(Y, preds_ade_ALLM)
    rmse_ade_V0         <- rmse(Y, preds_ade_V0)
    rmse_ade_noSR       <- rmse(Y, preds_ade_noSR)

    results <-
      list(
        rmse_ADE = rmse_ADE,
        rmse_ADE_van = rmse_ADE_van,
        rmse_stacking = rmse_stacking,
        rmse_AEC = rmse_AEC,
        #rmse_ADE_runtime = rmse_ADE_runtime,
        rmse_WindowLoss = rmse_WindowLoss,
        rmse_ERP = rmse_ERP,
        rmse_Simple = rmse_Simple,
        rmse_BestTR = rmse_BestTR,
        rmse_Blast = rmse_Blast,
        rmse_trimmedSimple = rmse_trimmedSimple,
        rmse_LossTrain = rmse_LossTrain,
        rmse_MLpol = rmse_MLpol,
        rmse_Arbit = rmse_Arbit,
        rmse_EWA = rmse_EWA,
        rmse_FixedShare = rmse_FixedShare,
        rmse_OGD = rmse_OGD,
        rmse_ARIMA = rmse_ARIMA,
        rmse_ETS = rmse_ETS,
        rmse_NAIVE = rmse_NAIVE,
        rmse_SNAIVE = rmse_SNAIVE,
        rmse_ade_SB = rmse_ade_SB,
        rmse_ade_ALLM = rmse_ade_ALLM,
        rmse_ade_V0 = rmse_ade_V0,
        rmse_ade_noSR = rmse_ade_noSR)


    results_M <- loss_predictors

    list(AGG_RMSE=results, BASE = results_M)
  }

#' Experiments ADE vs Handling correlation in X
#'
#' @param train train set
#' @param test test set
#' @param form formula
#' @param specs model specs
CORRELATION_PIPELINE <-
  function(train, test, form, specs) {
    require(ranger)
    require(forecast)
    require(tsensembler)
    require(opera)
    require(caret)

    Y <- get_y(test, form)

    # PCA
    tr <- train
    ts <- test
    y_tr <- tr$target; tr$target <- NULL
    y_ts <- ts$target; ts$target <- NULL
    train_pca <- preProcess(tr, method = "pca", thresh=.95)
    trf <- cbind.data.frame(target = y_tr, predict(train_pca, tr))
    tsf <- cbind.data.frame(target = y_ts, predict(train_pca, ts))

    # Correlated features
    deprec_variables <- findCorrelation(cor(train[,-1]), .95) + 1
    if (length(deprec_variables) > 0) {
      train_without_feat_corr <-
        subset(train, select = -deprec_variables)
    } else {
      train_without_feat_corr <- train
    }

    cat("Fit ADE \n")
    set.seed(1234)
    ade <-
      ADE(
        form = form,
        data = train,
        specs = specs,
        lambda = 50,
        omega = .5,
        select_best = FALSE,
        all_models = FALSE,
        aggregation = "linear",
        sequential_reweight = TRUE
      )

    cat("Fit ADE with PCAd feat set\n")
    set.seed(1234)
    ade_pca <-
      ADE(
        form = form,
        data = trf,
        specs = specs,
        lambda = 50,
        omega = .5,
        select_best = FALSE,
        all_models = FALSE,
        aggregation = "linear",
        sequential_reweight = TRUE
      )

    cat("Fit ADE without correlated feats\n")
    set.seed(1234)
    ade_correlation <-
      ADE(
        form = form,
        data = train_without_feat_corr,
        specs = specs,
        lambda = 50,
        omega = .5,
        select_best = FALSE,
        all_models = FALSE,
        aggregation = "linear",
        sequential_reweight = TRUE
      )

    ADE_yh  <- predict(ade, test)@y_hat
    ADE_corr_yh  <- predict(ade_correlation, test)@y_hat
    ADE_PCA_yh  <- predict(ade_pca, tsf)@y_hat

    ade@sequential_reweight <- FALSE
    ade_correlation@sequential_reweight <- FALSE
    ade_pca@sequential_reweight <- FALSE

    ADE_noSR_yh  <- predict(ade, test)@y_hat
    ADE_noSR_corr_yh  <- predict(ade_correlation, test)@y_hat
    ADE_noSR_PCA_yh  <- predict(ade_pca, tsf)@y_hat

    cat("RMSE\n")
    rmse_ADE <- rmse(Y, ADE_yh)
    rmse_ADE_corr <- rmse(Y, ADE_corr_yh)
    rmse_ADE_PCA <- rmse(Y, ADE_PCA_yh)
    rmse_ADE_noSR <- rmse(Y, ADE_noSR_yh)
    rmse_ADE_noSR_corr <- rmse(Y, ADE_noSR_corr_yh)
    rmse_ADE_noSR_PCA <- rmse(Y, ADE_noSR_PCA_yh)

    rm(ade, ade_pca, ade_correlation)

    results <-
      list(
        rmse_ADE    = rmse_ADE,
        rmse_ADE_corr = rmse_ADE_corr,
        rmse_ADE_PCA = rmse_ADE_PCA,
        rmse_ADE_noSR = rmse_ADE_noSR,
        rmse_ADE_noSR_corr=rmse_ADE_noSR_corr,
        rmse_ADE_noSR_PCA=rmse_ADE_noSR_PCA)

    results
  }



#' Pipeline for analysing different deployment strategies
#'
#' "Off" -> models trained in the training set and not updated
#' throughout test
#' "On" -> mdoels trained in the training set and updated
#' during test every "step" observations
#'
#' 1 - Base-level off, Meta-level off -- strategy in the main experiments
#' 2 - Base-level off, Meta-level on
#' 3 - Base-level on, meta-level off
#' 4 - Base-level on, meta-level on
#' 5 - ADE run-time. base-level off, meta runtime on
#'
#' @param train training set
#' @param test test set
#' @param form formula
#' @param specs model specs
#' @param step updating step
#'
#' @export
RETRAIN_ADE_PIPELINE <-
  function(train, test, form, specs, step = 100) {
    require(ranger)

    Y <- get_y(test, form)
    Y_tr <- get_y(train, form)

    set.seed(1234)
    ade <-
      ADE(
        form = form,
        data = train,
        specs = specs,
        lambda = 50,
        omega = .5,
        select_best = FALSE,
        all_models = FALSE,
        aggregation = "linear",
        sequential_reweight = TRUE)

    M <- ade@base_ensemble

    preds_ADE_run <-
      GEN.ADE_runtime(
        base_models = M,
        newdata = test,
        lambda = 50,
        omega = .5,
        warmup = 50,
        steps = step)

    ade0 <- ade
    preds <- predict(ade, test)

    cat("M0 Z0\n")
    M0_Z0 <- preds@y_hat

    ade <- ade0
    cat("M0 Z1\n")
    predictions <- numeric(nrow(test))
    for (i in seq_along(predictions)) {
      predictions[i] <- predict(ade, test[i, ])@y_hat

      if (i %% step == 0) {
        ade <- update_ade_meta(ade, rbind.data.frame(train, test[seq_len(i), ]))
      } else {
        ade <- update_weights(ade, test[i, ])
      }
    }
    M0_Z1 <- predictions

    ade <- ade0
    cat("M1 Z0\n")
    predictions <- numeric(nrow(test))
    for (i in seq_along(predictions)) {
      predictions[i] <- predict(ade, test[i, ])@y_hat

      if (i %% step == 0) {
        ade <-
          update_base_models(ade,
                             rbind.data.frame(train, test[seq_len(i), ]))
      } else {
        ade <- update_weights(ade, test[i, ])
      }
    }
    M1_Z0 <- predictions

    ade <- ade0
    cat("M1 Z1\n")
    predictions <- numeric(nrow(test))
    for (i in seq_along(predictions)) {
      predictions[i] <- predict(ade, test[i, ])@y_hat

      if (i %% step == 0) {
        ade <- update_ade(ade, rbind.data.frame(train, test[seq_len(i), ]))
      } else {
        ade <- update_weights(ade, test[i, ])
      }
    }
    M1_Z1 <- predictions


    ############################################################
    cat("RMSE loss\n")
    LOSS_M0_Z0_rmse <- rmse(Y, M0_Z0)
    LOSS_M1_Z1_rmse <- rmse(Y, M1_Z1)
    LOSS_M1_Z0_rmse <- rmse(Y, M1_Z0)
    LOSS_M0_Z1_rmse <- rmse(Y, M0_Z1)
    LOSS_M0_Zs_rmse <- rmse(Y, preds_ADE_run)
    


    rmse_loss <- list(
      LOSS_M0_Z0_rmse=LOSS_M0_Z0_rmse,
      LOSS_M1_Z1_rmse=LOSS_M1_Z1_rmse,
      LOSS_M1_Z0_rmse=LOSS_M1_Z0_rmse,
      LOSS_M0_Z1_rmse=LOSS_M0_Z1_rmse,
      LOSS_M0_Zs_rmse=LOSS_M0_Zs_rmse)

    rmse_loss
  }


#' Pipeline for analysing the value of additional experts
#'
#' @param train training data
#' @param test test data
#' @param specs complete model specs
#' @param max_models max no of models
#'
#'
ADDING_EXPERTS_PIPELINE <-
  function(train,test,form, specs, max_models=100) {
    lambda <- 50#values used in main experimens
    omega <- .5#values used in main experimens

    Y <- get_y(test, form)
    tgt <- get_target(form)

    base_models <- build_base_ensemble(form, train, specs)

    OOB_data <-
      blocked_prequential(
        x = train,
        nfolds = 10,
        intraining_estimations,
        .rbind = FALSE,
        form = form,
        specs = specs,
        lfun = ae)

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

    data_size <- nrow(test)
    seq. <- seq_len(data_size)
    N <- base_models@N

    Y_hat <- predict(base_models, test)
    Y_hat_recent <- predict(base_models, recent_series)
    Y_hat_recent <- tail(Y_hat_recent, lambda)

    Y_hat_extended <- rbind.data.frame(Y_hat_recent, Y_hat)

    E_hat <- lapply(meta_models,
                    function(o) {
                      meta_predict(o, test, "randomforest")
                    })
    names(E_hat) <- colnames(Y_hat)
    E_hat <- abs(as.data.frame(E_hat))

    Y_rs <- get_y(recent_series, form)

    reps <- 30#monte carlo approx.
    mc_reps <-
      lapply(1:reps,
             function(j) {
               size_vec <- seq(from = 5, to = max_models, by = 5)
               predictions_list <- vector("list", length(size_vec))
               names(predictions_list) <- as.character(size_vec)
               for (i in seq_along(size_vec)) {
                 sample_size <- size_vec[i]

                 #cat(i, "\n")
                 sample_models <- sample(1:N, sample_size, replace = FALSE)

                 Y_hat_recent_sub <- Y_hat_recent[, sample_models]
                 Y_hat_sub <- Y_hat[, sample_models]
                 E_hat_sub <- E_hat[, sample_models]
                 Y_hat_extended_sub <- Y_hat_extended[, sample_models]

                 C <-
                   build_committee(
                     rbind.data.frame(Y_hat_recent_sub, Y_hat_sub),
                     c(Y_rs, Y),
                     lambda = lambda,
                     omega = omega
                   )

                 C <- C[-seq_len(lambda)]
                 C <- lapply(C, unname)

                 W <- matrix(0., ncol = sample_size, nrow = data_size)
                 for (j in seq.) {
                   W_j <- E_hat_sub[j, C[[j]]]
                   W_j <- model_weighting(W_j, "linear")

                   W[j,  C[[j]]] <- W_j
                   W[j,-C[[j]]] <- 0.
                 }
                 colnames(W) <- colnames(Y_hat_sub)

                 ssimilarity <-
                   sliding_similarity(Y_hat_extended_sub, lambda)
                 W <- sequential_reweighting(ssimilarity, W)

                 predictions_list[[i]] <-
                   combine_predictions(Y_hat_sub, W, C)
               }
               predictions_list
             })

    rmse_predictors <-
      lapply(mc_reps,
             function(preds) {
               sapply(preds,
                      function(x) {
                        rmse(Y, x)
                      })
             })

    list(preds=mc_reps,
         rmse_predictors=rmse_predictors,
         Y=Y,
         Y_tr=Y_tr)
  }


ADDING_EXPERTS_PIPELINE <-
  function(train,test,form, specs, max_models=100) {
    lambda <- 50
    omega <- .5

    tgt <- get_target(form)
    
    Ytr <- get_y(train, form)
    Y <- get_y(test, form)

    base_models <- build_base_ensemble(form, train, specs)

    OOB_data <-
      blocked_prequential(
        x = train,
        nfolds = 10,
        intraining_estimations,
        .rbind = FALSE,
        form = form,
        specs = specs,
        lfun = ae)

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

    data_size <- nrow(test)
    seq. <- seq_len(data_size)
    N <- base_models@N

    Y_hat <- predict(base_models, test)
    Y_hat_recent <- predict(base_models, recent_series)
    Y_hat_recent <- tail(Y_hat_recent, lambda)

    Y_hat_extended <- rbind.data.frame(Y_hat_recent, Y_hat)

    E_hat <- lapply(meta_models,
                    function(o) {
                      meta_predict(o, test, "randomforest")
                    })
    names(E_hat) <- colnames(Y_hat)
    E_hat <- abs(as.data.frame(E_hat))

    Y_rs <- get_y(recent_series, form)

    reps <- 30#monte carlo approx.
    mc_reps <-
      lapply(1:reps,
             function(j) {
               size_vec <- seq(from = 5, to = max_models, by = 5)
               predictions_list <- vector("list", length(size_vec))
               names(predictions_list) <- as.character(size_vec)
               for (i in seq_along(size_vec)) {
                 sample_size <- size_vec[i]

                 #cat(i, "\n")
                 sample_models <- sample(1:N, sample_size, replace = FALSE)

                 Y_hat_recent_sub <- Y_hat_recent[, sample_models]
                 Y_hat_sub <- Y_hat[, sample_models]
                 E_hat_sub <- E_hat[, sample_models]
                 Y_hat_extended_sub <- Y_hat_extended[, sample_models]

                 C <-
                   build_committee(
                     rbind.data.frame(Y_hat_recent_sub, Y_hat_sub),
                     c(Y_rs, Y),
                     lambda = lambda,
                     omega = omega
                   )

                 C <- C[-seq_len(lambda)]
                 C <- lapply(C, unname)

                 W <- matrix(0., ncol = sample_size, nrow = data_size)
                 for (j in seq.) {
                   W_j <- E_hat_sub[j, C[[j]]]
                   W_j <- model_weighting(W_j, "linear")

                   W[j,  C[[j]]] <- W_j
                   W[j,-C[[j]]] <- 0.
                 }
                 colnames(W) <- colnames(Y_hat_sub)

                 ssimilarity <-
                   sliding_similarity(Y_hat_extended_sub, lambda)
                 W <- sequential_reweighting(ssimilarity, W)

                 predictions_list[[i]] <-
                   combine_predictions(Y_hat_sub, W, C)
               }
               predictions_list
             })

    rmse_predictors <-
      lapply(mc_reps,
             function(preds) {
               sapply(preds,
                      function(x) {
                        rmse(Y, x)
                      })
             })

    list(preds=mc_reps,
         rmse_predictors=rmse_predictors,
         Y=Y,
         Y_tr=Ytr)
  }


#' value of adding data
#' analysis in a prequential in blocks
#'
#' @param ds dataset
#' @param form formula
#' @param specs model specs
#'
#' NEEDS REFACTOR
ADDING_DATA_PIPELINE <-
  function(ds,form,specs) {
    dataSplits <- prequential_in_blocks_samples(ds, 100)
    
    res <- vector("list", length(dataSplits))
    for (i in seq_along(dataSplits)) {#
      cat(i, " ")
      ds <- dataSplits[[i]]
      tgt <- get_target(form)
      
      specs@learner <- 
        specs@learner[!specs@learner %in% c("bm_gbm","bm_ppr")]
      
      train <- ds$train
      test  <- ds$test
      
      M <- build_base_ensemble(form, train, specs)
      
      if (i == 1) {
        lambda<-min_lambda <- min(nrow(train)-1, 50)
        ade_preds <-
          GEN.ADE_runtime(
            base_models = M,
            newdata = test,
            lambda = min_lambda,
            omega = .5,
            warmup = 3,
            steps = 3)
        
      } else {
        if (i < 15) {
          lambda <- 5
        } else {
          lambda <- 50
        }
        
        ade_preds <- generalizer_ade(base_models = M,
                                     traindata = train,
                                     newdata = test,
                                     specs = specs,
                                     lfun = ae,
                                     lambda = lambda,
                                     omega = .5, 
                                     nfolds = min(i, 5))
        
      }
      
      if (i < 15) {
        lambda <- 5
      } else {
        lambda <- 50
      }
      
      lambda <- min(50,nrow(test))
      
      wl_preds <- 
        GEN.WindowLoss(base_models = M,
                       newdata = test,
                       lambda = lambda,
                       sequential_reweight = FALSE)
      
      mlpol_preds <- GEN.MLpol(base_models = M,
                               newdata = test,
                               lambda = lambda, 
                               sequential_reweight = FALSE)
      
      arima_preds <- arima_workflow(train,test)
      ets_preds <- suppressMessages(expsmooth_workflow(train,test))
      naive_preds <- naive_workflow(train,test)
      simple_preds <- GEN.SimpleTrim(base_models = M, 
                                     newdata = test, 
                                     omega = .5)
      
      
      res[[i]] <- list(ade=ade_preds,
                       arima=arima_preds,
                       naive=naive_preds,
                       simpletrim=simple_preds,
                       wl=wl_preds,
                       mlpol=mlpol_preds,
                       train=train,
                       test=test)
    }
    
    loss_info <-
      lapply(res,
             function(x) {
               ade_loss <- se(x$test$target,x$ade)
               arima_loss <- se(x$test$target,x$arima)
               
               naive_loss <- se(x$test$target,x$naive)
               
               wl_loss <- se(x$test$target,x$wl)
               
               simpletrim_loss <- se(x$test$target, x$simpletrim)
               
               mlpol_loss <- se(x$test$target,x$mlpol)
               
               
               list(ade=ade_loss,
                    arima=arima_loss,
                    naive=naive_loss,
                    wl=wl_loss,
                    simpletrim=simpletrim_loss,
                    mlpol=mlpol_loss)
             })
    
    loss_info
  }


#' Impact analysis of sequential reweighting
#' to SOTA aggregation approaches
#'
#' @export
SeqRew_IMPACT_PIPELINE <-
  function(train, test, form, specs) {
    require(ranger)

    Y <- get_y(test, form)

    set.seed(1234)
    M <- build_base_ensemble(form, train, specs)
    lambda<-50

    cat("Analysing AEC...\n")
    AEC_0 <- GEN.AEC(base_models = M, newdata =  test,
                     ff = .9, lambda=lambda,
                     sequential_reweight = FALSE)
    AEC_1 <- GEN.AEC(base_models = M, newdata =  test,
                     ff = .9, lambda=lambda,
                     sequential_reweight = TRUE)

    cat("Analysing WINDOWLOSS...\n")
    WindowLoss_0 <- GEN.WindowLoss(base_models = M, newdata = test,
                                   lambda=lambda,
                                   sequential_reweight = FALSE)

    WindowLoss_1 <- tryCatch(GEN.WindowLoss(base_models = M, newdata = test,
                                            lambda=lambda,
                                            sequential_reweight = TRUE),
                             error=function(e) WindowLoss_0)

    cat("Analysing MLpol\n")
    MLpol_0 <-  GEN.MLpol(base_models = M, newdata = test,
                          lambda = lambda,
                          sequential_reweight = FALSE)
    MLpol_1 <-  GEN.MLpol(base_models = M, newdata = test,
                          lambda = lambda,
                          sequential_reweight = TRUE)

    cat("Analysing EWA\n")
    EWA_0 <- GEN.EWA(base_models = M, newdata = test,
                     lambda = lambda,
                     sequential_reweight = FALSE)
    EWA_1 <- GEN.EWA(base_models = M, newdata = test,
                     lambda = lambda,
                     sequential_reweight = TRUE)


    cat("Analysing FS\n")
    FixedShare_0 <- GEN.FixedShare(base_models = M, newdata = test,
                                   lambda = lambda,
                                   sequential_reweight = FALSE)

    FixedShare_1 <- GEN.FixedShare(base_models = M, newdata = test,
                                   lambda = lambda,
                                   sequential_reweight = TRUE)

    cat("Analysing OGD\n")
    OGD_0 <- GEN.OGD(base_models = M, newdata = test,
                     lambda = lambda,
                     sequential_reweight = FALSE)

    OGD_1 <- GEN.OGD(base_models = M, newdata = test,
                     lambda = lambda,
                     sequential_reweight = TRUE)


    ############################################################
    cat("RMSE loss\n")
    RMSE_AEC_0        <- rmse(Y, AEC_0)
    RMSE_AEC_1        <- rmse(Y, AEC_1)
    RMSE_WindowLoss_0 <- rmse(Y, WindowLoss_0)
    RMSE_WindowLoss_1 <- rmse(Y, WindowLoss_1)
    RMSE_MLpol_0      <- rmse(Y, MLpol_0)
    RMSE_MLpol_1      <- rmse(Y, MLpol_1)
    RMSE_EWA_0        <- rmse(Y, EWA_0)
    RMSE_EWA_1        <- rmse(Y, EWA_1)
    RMSE_FixedShare_0 <- rmse(Y, FixedShare_0)
    RMSE_FixedShare_1 <- rmse(Y, FixedShare_1)
    RMSE_OGD_0        <- rmse(Y, OGD_0)
    RMSE_OGD_1        <- rmse(Y, OGD_1)

    rmse_l <- list(
      RMSE_AEC_0        = RMSE_AEC_0,
      RMSE_AEC_1        = RMSE_AEC_1,
      RMSE_WindowLoss_0 = RMSE_WindowLoss_0,
      RMSE_WindowLoss_1 = RMSE_WindowLoss_1,
      RMSE_MLpol_0      = RMSE_MLpol_0,
      RMSE_MLpol_1      = RMSE_MLpol_1,
      RMSE_EWA_0        = RMSE_EWA_0,
      RMSE_EWA_1        = RMSE_EWA_1,
      RMSE_FixedShare_0 = RMSE_FixedShare_0,
      RMSE_FixedShare_1 = RMSE_FixedShare_1,
      RMSE_OGD_0        = RMSE_OGD_0,
      RMSE_OGD_1        = RMSE_OGD_1)

    rmse_l
  }


#' pipeline for scalability
#'
#' @param train training set
#' @param test test set
#' @param form formula
#' @param specs model specs
#'
SCALABILITY_PIPELINE <-
  function(train, test, form, specs) {
    require(ranger)
    require(forecast)
    require(tsensembler)
    require(opera)

    Y <- get_y(test, form)

    t0 <- Sys.time()
    M <- build_base_ensemble(form, train, specs)
    Base_Models_clock <- Sys.time() - t0

    OOB <-
      train_ade(
        form = form,
        train = train,
        specs = specs,
        lambda = 50,
        lfun = ae, "randomforest")$OOB


    t0 <- Sys.time()
    ADE_preds <- generalizer_ade(M,train,test,specs)
    ADE_clock <- Sys.time() - t0

    t0 <- Sys.time()
    preds_trimmedSimple <-
      GEN.SimpleTrim(base_models = M,
                     newdata = test,
                     omega = .5)
    SimpleTrim_clock <- Sys.time() - t0

    t0 <- Sys.time()
    MLpol_preds <-
      GEN.MLpol(
        base_models = M,
        newdata = test,
        lambda = 50,
        sequential_reweight = FALSE
      )
    MLpol_clock <- Sys.time() - t0

    t0 <- Sys.time()
    WindowLoss_preds <-
      GEN.WindowLoss(
        base_models = M,
        newdata = test,
        lambda = 50,
        sequential_reweight = FALSE
      )
    WindowLoss_clock <- Sys.time() - t0

    preds_NAIVE <- naive_workflow(train, test)

    t0 <- Sys.time()
    preds_ARIMA <-
      tryCatch(arima_workflow_xreg(train, test),
               error = function(e) preds_SNAIVE)
    ARIMA_clock <- Sys.time() - t0

    list(
      Base_Models_clock = Base_Models_clock,
      ADE_clock = ADE_clock,
      SimpleTrim_clock=SimpleTrim_clock,
      MLpol_clock = MLpol_clock,
      WindowLoss_clock = WindowLoss_clock,
      ARIMA_clock = ARIMA_clock)
  }
