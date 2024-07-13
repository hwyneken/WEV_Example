require(here)
require(ggplot2)
require(ggthemes)

modelDF <- readRDS(file = here("data/WEV_Full_Model_Data.RDS"))

plotDF <- modelDF[,c("LR_SELF_Snell","AGE_NUM","UNION","GENDER")]

p1 <- ggplot(data = plotDF,aes(x=AGE_NUM,y=LR_SELF_Snell,color=UNION,group=UNION))
p1 <- p1 + facet_wrap(~ GENDER,nrow=1)
p1 <- p1 + geom_smooth()
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
p1 <- p1 + scale_color_manual(values = c("#223B3C","#9FCEA7"))
p1 <- p1 + labs(x = "Respondent Age in Years",
                y = "Transformed Self-Reported Ideology Score",
                color = "Union Status",
                title = "Smoother Estimates of the Relationship Between Ideology and Age",
                subtitle = "Union Membership and Gender Interact with the Age/Ideology Trajectory")
ggsave(p1,file = here("images/Age_Union_Gender_Interaction.png"),
       units = "in",width=12,height=8)