#' FROM https://github.com/BayesianTestsML/tutorial
#' correlated Bayesian t-test
#'
#' diff_a_b is a vector of differences between the two classifiers,
#' on each fold of cross-validation.
#' If you have done 10 runs of 10-folds cross-validation,
#' you have 100 results for each classifier.
#' You should have run cross-validation on the same folds for the two classifiers.
#' @param diff_a_b is the difference fold-by-fold.
#' @param rho is the correlation of the cross-validation results: 1/(number of folds)
#' @param rope_min and rope_max are the lower and the upper bound of the rope
#' @param rope_max ub
#'
#' @export
correlatedBayesianTtest <-
  function(diff_a_b, rho, rope_min, rope_max) {
    if (rope_max < rope_min) {
      stop("rope_max should be larger than rope_min")
    }

    delta <- mean(diff_a_b)
    n <- length(diff_a_b)
    df <- n - 1
    stdX <- sd(diff_a_b)
    sp <- sd(diff_a_b) * sqrt(1 / n + rho / (1 - rho))
    p.left <- pt((rope_min - delta) / sp, df)
    p.rope <- pt((rope_max - delta) / sp, df) - p.left
    results <-
      list(
        'left' = p.left,
        'rope' = p.rope,
        'right' = 1 - p.left - p.rope
      )
    return (results)
  }


#' Bayesian signed rank test
#' FROM https://github.com/BayesianTestsML/tutorial
BayesianSignTest <- function(diffVector, rope_min, rope_max, samples=3000) {
  library(MCMCpack)

  #samples <- 3000

  #build the vector 0.5 1 1 ....... 1
  weights <- c(0.5, rep(1, length(diffVector)))

  #add the fake first observation in 0
  diffVector <- c (0, diffVector)


  #for the moment we implement the sign test. Signedrank will follows
  probLeft <- mean (diffVector < rope_min)
  probRope <- mean (diffVector > rope_min & diffVector < rope_max)
  probRight <- mean (diffVector > rope_max)



  results = list ("probLeft" = probLeft,
                  "probRope" = probRope,
                  "probRight" = probRight)

  return (results)

}


#' Bayesian signed rank test
#' FROM https://github.com/BayesianTestsML/tutorial
BayesianSignedRank <-
  function(diffVector, rope_min, rope_max, samples=3000) {
    library(MCMCpack)

    #samples <- 30000

    #build the vector 0.5 1 1 ....... 1
    weights <- c(0.5, rep(1, length(diffVector)))

    #add the fake first observation in 0
    diffVector <- c (0, diffVector)

    sampledWeights <- rdirichlet(samples, weights)

    winLeft <- vector(length = samples)
    winRope <- vector(length = samples)
    winRight <- vector(length = samples)

    for (rep in 1:samples) {
      currentWeights <- sampledWeights[rep, ]
      for (i in 1:length(currentWeights)) {
        for (j in 1:length(currentWeights)) {
          product = currentWeights[i] * currentWeights[j]
          if (diffVector[i] + diffVector[j] > (2 * rope_max)) {
            winRight[rep] <- winRight[rep] + product
          }
          else if (diffVector[i] + diffVector[j] > (2 * rope_min)) {
            winRope[rep] <- winRope[rep] + product
          }
          else {
            winLeft[rep] <- winLeft[rep] + product
          }
        }
      }
      maxWins = max(winRight[rep], winRope[rep], winLeft[rep])
      winners = (winRight[rep] == maxWins) * 1 + (winRope[rep] == maxWins) *
        1 + (winLeft[rep] == maxWins) * 1
      winRight[rep] <- (winRight[rep] == maxWins) * 1 / winners
      winRope[rep] <- (winRope[rep] == maxWins) * 1 / winners
      winLeft[rep] <- (winLeft[rep] == maxWins) * 1 / winners
    }


    results = list(
      "winLeft" = mean(winLeft),
      "winRope" = mean(winRope),
      "winRight" = mean(winRight)
    )

    return (results)

  }


#' results analysis for a dataset
#'
#' @param x results from pipeline for a dataset
#' @param rho correlation factor from cross val results
#' @param rope_min min rope val
#' @param rope_max max rope val
#' @param nm name of the list of the results
#' @param scale scale results according to Simple
results_analysis_in_dataset <-
  function(x,
           baseline="ade", 
           rho, 
           rope_min=-.01,
           rope_max=.01, 
           nm = "AGG",
           scale=T) {

    agg_x <- lapply(x, function(z) unlist(z[[nm]]))
    agg_x <- rbind_l(agg_x)
    col_baseline <- grep(paste0("^",baseline,"$"), colnames(agg_x))
    c_iters <- seq_len(ncol(agg_x))[-col_baseline]

    ## average loss across mc repetitions
    avg_loss <- colMeans(agg_x, na.rm = T)

    ## rank across mc repetitions
    rank_ds <- rank(avg_loss)

    if (scale) {
      agg_x <- as.data.frame(agg_x)
      agg_x[] <- lapply(agg_x, function(x) {
        x /agg_x[,"Simple"]
      })

      agg_x <- as.matrix(agg_x)
    }

    BayesCorrTT <-
      sapply(c_iters,
             function(j) {
               x_base <- agg_x[, col_baseline]
               x_comp <- agg_x[, j]

               diff_vec <- x_base - x_comp

               #---
               diff_vec <- diff_vec[!is.na(diff_vec)]
               rho_ <- 1/length(diff_vec)

               correlatedBayesianTtest(diff_vec, rho_, rope_min, rope_max)
             })
    colnames(BayesCorrTT) <- colnames(agg_x)[-col_baseline]

    list(loss_vals = agg_x,
         avg_loss = avg_loss,
         agg_rank = rank_ds,
         BayesCorrTT = BayesCorrTT)
  }


sr_impact_bayesian_analysis <-
  function(x,
           baseline="ADE", rho, rope_min=-.01,rope_max=.01) {

    agg_x <- lapply(x, function(z) unlist(z))
    agg_x <- rbind_l(agg_x)
    col_baseline <- grep(paste0("^",baseline,"$"), colnames(agg_x))
    c_iters <- seq_len(ncol(agg_x))[-col_baseline]

    BayesCorrTT <-
      sapply(c_iters,
             function(j) {
               x_base <- agg_x[, col_baseline]
               x_comp <- agg_x[, j]

               diff_vec <- x_base - x_comp

               correlatedBayesianTtest(diff_vec, rho, rope_min, rope_max)
             })
    colnames(BayesCorrTT) <- colnames(agg_x)[-col_baseline]

    counter_baseline <- gsub("1","0",baseline)
    col_c_baseline <- grep(paste0("^",counter_baseline,"$"),
                           colnames(BayesCorrTT))

    BayesCorrTT[,col_c_baseline]
  }


analysis_across_datasets <-
  function(results,
           baseline = "ADE",
           rope_min =-.01,
           rope_max =.01, samples=3000) {

    avg_loss <- t(sapply(results,
                         function(x) x$avg_loss))

    avg_loss <- avg_loss[complete.cases(avg_loss),]

    col_baseline <- grep(paste0("^",baseline,"$"), colnames(avg_loss))
    c_iters <- seq_len(ncol(avg_loss))[-col_baseline]


    BayesSign <-
      sapply(c_iters,
             function(j) {
               x_base <- avg_loss[, col_baseline]
               x_comp <- avg_loss[, j]

               diff_vec <- x_base - x_comp

               BayesianSignTest(diff_vec, rope_min, rope_max, samples)
             })
    colnames(BayesSign) <- colnames(avg_loss)[-col_baseline]


    BayesRank <-
      sapply(c_iters,
             function(j) {
               x_base <- avg_loss[, col_baseline]
               x_comp <- avg_loss[, j]

               diff_vec <- x_base - x_comp

               BayesianSignedRank(diff_vec, rope_min, rope_max, samples)
             })
    colnames(BayesRank) <- colnames(avg_loss)[-col_baseline]


    rank_across_ds <-
      t(sapply(results,
               function(x) x$agg_rank))


    list(BayesSign=BayesSign,
         BayesRank=BayesRank,
         rank_across_ds= rank_across_ds)
  }


analysis_across_datasets_bsign <-
  function(results,
           baseline = "ADE",
           rope_min =-.01,
           rope_max =.01, samples=3000) {

    avg_loss <- t(sapply(results,
                         function(x) x$avg_loss))

    avg_loss <- avg_loss[complete.cases(avg_loss),]

    col_baseline <- grep(paste0("^",baseline,"$"), colnames(avg_loss))
    c_iters <- seq_len(ncol(avg_loss))[-col_baseline]


    BayesSign <-
      sapply(c_iters,
             function(j) {
               x_base <- avg_loss[, col_baseline]
               x_comp <- avg_loss[, j]

               diff_vec <- x_base - x_comp

               BayesianSignTest(diff_vec, rope_min, rope_max, samples)
             })
    colnames(BayesSign) <- colnames(avg_loss)[-col_baseline]


    BayesSign
  }

