library(tsensembler)
library(reshape2)

load("./data/embedded_data.rdata")
nms <- names(embedded_time_series)

addexperts_results <- rm.null(addexperts_results)

add_exp <-
  sapply(addexperts_results,
         function(x) {
           avg_loss_across <-
             rowMeans(sapply(x, function(z) {
               colMeans(rbind_l(z$rmse_predictors))
             }))

           bas <- avg_loss_across[length(avg_loss_across)]

           sapply(avg_loss_across,
                  function(z) {
                    percentual_difference(z, bas)
                  })
         })

ds <-
  sapply(c(median, IQR),
         function(f)
           apply(add_exp, 1, f))

ds <- as.data.frame(round(ds, 2))
colnames(ds) <- c("avg","sdev")
ds$N <- seq(from=5,to=100,by=5)
rownames(ds) <- NULL

ggplot(data = ds,
       aes(x = N,
           y = avg)) +
  geom_bar(stat="identity",
           fill="#00CCCC") +
  theme_minimal() +
  theme(axis.text.x  = element_text(size = 14)) +
  theme(axis.text.y  = element_text(size = 12),
        axis.title.y = element_text(size = 12)) +
  geom_errorbar(aes(ymin = avg - sdev,
                    ymax = avg + sdev),
                width = .7,
                position = position_dodge(.9)) +
  labs(x="No. of experts in the ensemble",
       y="Avg. perc. diff. & std dev.",
       title = "")


#
