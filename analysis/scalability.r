scale_results <- rm.null(scale_results)
scale_results <- scale_results[!sapply(scale_results,
                                       function(z) any(is.na(z)))]

scale_ds <-
  sapply(scale_results,
         function(x) {
           clock <- sapply(x, function(z)
             sapply(z, calc_time))
           avg_clock <- rowMeans(clock, na.rm = TRUE)

           ADE <-
             avg_clock[["Base_Models_clock"]] + avg_clock[["ADE_clock"]]
           MLpol <-
             avg_clock[["Base_Models_clock"]] + avg_clock[["MLpol_clock"]]

           SimpleTrim <-
             avg_clock[["Base_Models_clock"]] + avg_clock[["SimpleTrim_clock"]]

           WindowLoss <-
             avg_clock[["Base_Models_clock"]] + avg_clock[["WindowLoss_clock"]]
           ARIMA <- avg_clock[["ARIMA_clock"]]

           c(
             ADE = ADE,
             MLpol = MLpol,
             SimpleTrim=SimpleTrim,
             WindowLoss = WindowLoss,
             ARIMA = ARIMA
           )
         })

scale_ds <- as.data.frame(t(scale_ds))


scale_ds[] <-
  lapply(scale_ds,
         function(x) {
           scale_ds$ADE / x
         })

scale_ds <- scale_ds[,c(3,5)]
colnames(scale_ds) <-
  c("ADE/SimpleTrim", "ADE/ARIMA")


PD_melted <- melt(scale_ds)
ggplot(PD_melted, aes(x=1,y= log_trans(value))) +
  facet_wrap( ~ variable,
              nrow = 1,
              scales="free") +
  geom_boxplot() +
  #geom_hline(yintercept = 0,col="red") +
  theme_minimal() +
  labs(x="",
       y="Relative runtime") +
  theme(axis.text.x  = element_blank()) +
  theme(axis.text.y  = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        strip.text.x = element_text(size= 12))


