library(tsensembler)
library(ggplot2)

source("sources/src-analysis-plots.R")

LAMBDA <-
  c(3, 5, 10, 15, 25, 50, 60,
    75, 100, 150, 300, 450, 600)
OMEGA <- c(.1, .2, .3, .4,
           .5, .6, .7, .8, .9)

gsearch <- expand.grid(LAMBDA, OMEGA)
colnames(gsearch) <- c("lambda", "omega")

form <- target ~.

sens_analysis<-rm.null(sens_analysis)

res_sens <-
  lapply(sens_analysis,
         function(x) {
           rowMeans(as.matrix(sapply(x,
                  function(z) {
                    unlist(z$par_rmse)
                  })))
         })

res_rank <- res_std <- res_sens <- as.data.frame(res_sens)

res_rank <- apply(res_rank, 2,rank)

avg_rank_res <- rowMeans(res_rank)
sd_rank_res <- apply(res_rank,1,sd)


df <- avg_rank_res

df <- cbind.data.frame(AvgRank = avg_rank_res,gsearch)
df2 <- cbind.data.frame(sdRank = sd_rank_res,gsearch)

p1 <- ggplot(df, aes(x = factor(lambda), y = factor(omega))) +
  geom_tile(aes(fill = AvgRank), colour = "white") +
  scale_fill_gradient(low = "white", high = "black") +
  labs(x = "lambda",
       y = "Omega",
       title = "") +
  theme(legend.position = "top") +
  guides(fill = guide_colorbar(
    title = "Average Rank",
    barwidth = 10,
    barheight = .5
  ))

p2 <- ggplot(df2, aes(x = factor(lambda), y = factor(omega))) +
  geom_tile(aes(fill = sdRank), colour = "white") +
  scale_fill_gradient(low = "white", high = "black") +
  labs(x = "lambda",
       y = "Omega",
       title = "") +
  theme(legend.position = "top") +
  guides(fill = guide_colorbar(
    title = "Std. Dev. Rank",
    barwidth = 10,
    barheight = .5
  ))


multiplot(p1, p2, cols = 2)




