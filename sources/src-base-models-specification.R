library(tsensembler)

all_kernels <- c("rbfdot","vanilladot","polydot","laplacedot")
base_predictors <- c("bm_mars","bm_ppr","bm_ffnn","bm_svr","bm_glm",
                     "bm_gaussianprocess", "bm_randomforest","bm_cubist",
                     "bm_pls_pcr","bm_gbm", "bm_timeseries")
pars_predictors <- list(bm_gaussianprocess = list(kernel = all_kernels,tol = c(.001,.01)),
                        bm_svr = list(kernel = all_kernels),
                        bm_ppr = list(nterms = c(2,5),
                                      sm.method = c("supsmu","gcvspline")),
                        bm_ffnn = list(size = c(25, 10, 15, 5, 3),
                                       decay = c(0.01), maxit=c(1500)),
                        bm_mars = list(degree = c(1, 3), nk = c(7,15), thresh=c(0.001)),
                        bm_glm = list(alpha = c(0,.25,.5,.75,1),
                                      family = c("gaussian")),
                        bm_timeseries = list(model = c("bm_arima","bm_ets","bm_tbats")),
                        bm_gbm = list(interaction.depth = c(5,10),
                                      dist = c("gaussian","laplace"),
                                      shrinkage = c(.1),
                                      n.trees = c(500, 1000)),
                        bm_randomforest = list(num.trees = c(250, 500)),
                        bm_pls_pcr = list(method = c("kernelpls","simpls","svdpc")),
                        bm_cubist = list(committees = c(10, 25, 50, 100)))
MODELSPECS <- model_specs(base_predictors,pars_predictors)

all_kernels <- c("rbfdot","vanilladot","polydot","laplacedot")
base_predictors <- c("bm_mars","bm_ppr","bm_ffnn","bm_svr","bm_glm",
                     "bm_gaussianprocess", "bm_randomforest","bm_cubist",
                     "bm_pls_pcr","bm_gbm","bm_timeseries")
pars_predictors <- list(bm_gaussianprocess = list(kernel = all_kernels,
                                                  tol = c(.001,.01, .1)),
                        bm_svr = list(kernel = all_kernels,
                                      C = c(1, 10, 5),
                                      epsilon= c(0.001,0.01)),
                        bm_timeseries = list(model = c("bm_arima","bm_ets","bm_tbats")),
                        bm_ppr = list(nterms = c(2,5, 10),
                                      sm.method = c("supsmu","gcvspline")),
                        bm_ffnn = list(size = c(10, 15, 5, 30),
                                       decay = c(0.05,0.01), maxit=c(1500)),
                        bm_mars = list(degree = c(1, 3, 5), nk = c(7,15, 30), thresh=c(0.001)),
                        bm_glm = list(alpha = c(0,.25,.5,.75,1),
                                      family = c("gaussian")),
                        bm_gbm = list(interaction.depth = c(5,10, 15),
                                      dist = c("gaussian","laplace"),
                                      shrinkage = c(.1, 0.01),
                                      n.trees = c(500, 1000)),
                        bm_randomforest = list(num.trees = c(250, 500, 1000)),
                        bm_pls_pcr = list(method = c("kernelpls","simpls","svdpc")),#, "cppls"
                        bm_cubist = list(committees = c(25, 50, 100)))
MODELSPECS_plus <- model_specs(base_predictors,pars_predictors)


