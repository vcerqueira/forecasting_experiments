library(tsensembler)
library(reshape2)
library(ggplot2)


load("./data/embedded_data_mlj.rdata")
nms <- names(embedded_time_series)

main_results <- rm.null(main_results)

rank_info <- sapply(main_results,
       function(x) {
         x <- x[!is.na(x)]

         rank(rowMeans(sapply(x, function(z) z$BASE)))
       })

avgrank_sort <- names(sort(apply(rank_info,1,median)))

spl <-
  sapply(avgrank_sort,
         function(z) {
           split_by_(as.character(z))[1]
         }, USE.NAMES = FALSE)

spl <- toupper(spl)

spl <- gsub("NNET","MLP", spl)
spl <- gsub("GBM","GBR", spl)
spl <- gsub("CUB","RBR", spl)
spl[28] <- "PLS"
spl[27] <- "PLS"
spl[31] <- "PCR"

rank_info <- rank_info[avgrank_sort,]

x_melted <- melt(t(rank_info))

p <- ggplot(x_melted, aes(Var2, value))

p  +
  geom_boxplot() +
  theme_minimal() +
  labs(x="",
       y="Rank") +
  theme(axis.text.x  = element_text(size=10,
                                    angle=90)) +
  theme(axis.text.y  = element_text(size = 12),
        axis.title.y = element_text(size = 12)) +
  scale_x_discrete(labels=spl)

