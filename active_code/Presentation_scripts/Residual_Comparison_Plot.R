require(here)
require(ggplot2)
require(ggthemes)

modelRes <- readRDS(file = here("data/workingModelFit.RDS"))

finalModel <- modelRes$mWorking # use the Snell transformation
origModel <- modelRes$mOrig # use the untransformed left-right ideology self-reported scale

qqnorm_Final <- qqnorm(residuals(finalModel))
qqnorm_Orig <- qqnorm(residuals(origModel))

plotDF <- data.frame(x = c(qqnorm_Orig$x,qqnorm_Final$x),
                     y = c(qqnorm_Orig$y,qqnorm_Final$y),
                     Model = c(rep("Untransformed Response",length(qqnorm_Orig$x)),
                               rep("Snell Transformed Response",length(qqnorm_Final$x))))
plotDF$Model <- factor(plotDF$Model,
                       levels = c("Untransformed Response",
                                  "Snell Transformed Response"))

p1 <- ggplot(data = plotDF,aes(x=x,y=y)) + facet_wrap(~ Model,nrow=1)
p1 <- p1 + geom_point()
p1 <- p1 + geom_abline(slope=1,intercept=0,color="red",lty=2)
p1 <- p1 + theme_tufte()
p1 <- p1 + theme(axis.text = element_text(size=14),
                 axis.title = element_text(size=16),
                 plot.title = element_text(size=16,hjust=0.5),
                 plot.subtitle = element_text(size=14,hjust=0.5),
                 strip.text = element_text(size=16),
                 legend.title = element_text(size=16),
                 legend.text = element_text(size=16),
                 panel.background = element_rect(fill = "#f8f8f8"),
                 strip.background = element_rect(fill = "gray"))
p1 <- p1 + labs(x = "Theoretical Quantiles",
                y = "Sample Quantiles",
                title = "Q-Q Plots for the Working Model with Original and Transformed Response")
ggsave(p1,file = here("images/QQ_Comparison.png"),
       units = "in",width=12,height=8)