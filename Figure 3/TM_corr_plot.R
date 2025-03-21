library(ggplot2)
library(tidyverse)
library(ggpubr)
library(xlsx)
library(ggthemes)
library(ggrepel)

tm.cor<- read.csv("TM_NC_corr.csv", header = T)
color <- c("#FB8072","#80B2D5","#B3DE69","#F3D567")

ggplot(tm.cor, aes(x=lognc, y=logtm))+
  geom_point(aes(colour = Subspecies),alpha=0.5)+
  geom_smooth(method = lm, se = TRUE)+ 
  theme_minimal()+
  xlab("Log10(Recombination Rate *1e8)")+
  ylab("Log(Twitching motility fringe width (μm)) ")+
  stat_cor(method = "pearson", label.x = 2, label.y = max(tm.cor$logtm)+.5)+
  geom_text_repel(aes(label=Strain, color=Subspecies),box.padding = 0.1, 
                  point.padding = 0.5, max.overlaps = 60, size =2, segment.size=.1)+
  scale_color_manual(values=color, name="Subspecies",
                    labels = c(expression(paste("subsp.",italic("fastidiosa"))),
                               expression(paste("subsp.",italic("morus"))),
                               expression(paste("subsp.",italic("multiplex"))),
                               expression(paste("subsp.",italic("pauca"))),
                               expression(paste("subsp.",italic("sandyi")))))

ggsave("TM_cor.pdf", height = 4, width = 6, units = "in", dpi = 700)

