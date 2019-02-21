library(ggplot2)
library(ggpubr)

e <- read.csv("results.csv")

e$feature <- factor(e$feature, levels=c("crps", "rmse", "bias", "corr", "mae"))

g1 <- ggplot(subset(e, feature == "rmse" | feature == "bias" |
                       feature == "crps"),
             aes(x=horizon, y=value, color=method)) + geom_line() + 
    facet_grid(feature~., scales = "free") +
    xlim(1,59) + xlab("horizon (hours)") +
    ylab("") + theme_bw() + theme(legend.position = "none",
                                  plot.margin = unit(c(1,0,1,1), "cm"),
                                  strip.text.y = element_blank(),
                                  strip.background = element_blank())

g2 <- ggplot(subset(e, feature == "rmse" | feature == "bias" |
                       feature == "crps"),
             aes(x=method, y=value, color=method)) + 
  geom_boxplot()+ #(position = position_dodge(preserve = "total")) + 
  facet_grid(feature~., scales = "free") +
    ylab("")  + xlab("") + theme(legend.position = "right") +
    theme_bw() + 
    theme(axis.text.x = element_text(angle = 30, hjust = 1, color="black"),
          plot.margin = unit(c(1,1,0.7,-0.5), "cm"),
          axis.text.y=element_blank(), axis.ticks.y=element_blank(),
          legend.position = "none")

ggarrange(g1,g2, ncol=2, widths=c(0.8, 0.2))

ggsave("errorGraph.png", device=png(), width=9.5*1.15, height=5*1.2)

system("convert -trim errorGraph.png errorGraph.png")
