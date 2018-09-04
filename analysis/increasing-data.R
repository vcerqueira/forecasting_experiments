library(tsensembler)
library(reshape2)
library(ggplot2)

setwd("/Users/vcerqueira/Desktop/ADETK2/ID/")

x <- lapply(list.files(),
            function(x) {
              load(x)
              rm.null(adddata_analysis)
            })

adddata_analysis <- sapply(x, rm.null)
length(adddata_analysis)
names(adddata_analysis) <- paste0("ts_",1:14)


res <-
  lapply(adddata_analysis,
         function(x) {
           ds <- Reduce(rbind.data.frame,
                        lapply(x, as.data.frame))


           ds_by_arima <- as.data.frame(ds)

           ds_rank <- as.data.frame(t(apply(ds_by_arima[,c(1,2,3,5)],1,rank)))

           ds_rank <- roll_mean_matrix(ds_rank,100)
           ds_rank <- round(ds_rank,3)

           as.matrix(head(ds_rank,2800))
         })

resf<-apply(simplify2array(res), 1:2, mean)

colnames(resf) <-
  c("ADE","ARIMA","Naive","SimpleTrim")

df <- melt(resf)

colnames(df) <- c("Time","Model","AvgRank")
ggplot(df, aes(x=Time,
               y=AvgRank,
               color=Model)) +
  geom_line(lwd=.8) +
  theme_minimal() +
  geom_vline(xintercept = 150) +
  theme(legend.position = "top")

