library(ggplot2)
library(tidyverse)
library(ggpubr)
library(xlsx)
library(ggthemes)
library(ggrepel)

df<- read.csv("TM_summary_35strains.csv", header = T)

level_order1 <- c("TemeculaL", "WM1-1","16M7","Je63","SLO","M23","3SVG","Alpha3",
                  "AlmaEM3","GILGRA274Ext","LLAFAL718A", "RH1", "L95-1","Oak95.1",
                  "Oak92.10","ALS6","M12","VALVAL072Ext", "RBCF119","4rd+1",
                  "BBI64","ESVL","IVIA5901","L95-2","Alma311","UVA519.1B","BBI80",
                  "BBI10","SRBB","Fillmore","Dixon", "LM10","Ann-1","MEDPRI047",
                  "XYL1961")

df$Subspecies <- factor(df$Subspecies, levels = c("subsp.fastidiosa","subsp.multiplex","subsp.pauca","subsp.sandyi"))
color <- c("#FB8072","#80B2D5","#B3DE69","#F3D567")

#boxplot
ggplot(df, aes(x=factor(Strain,level=level_order1), y=Fringe, color=Subspecies))+
  geom_boxplot()+
  stat_summary(fun = mean, geom = "point", shape = 18, size = 2, color = "grey", 
               aes(group = Strain)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
  xlab("Strain") + 
  ylab("Fringe width (μm)")+
  stat_compare_means( ref.group = "TemeculaL",aes(label = ..p.signif..), 
                      hide.ns = TRUE,size = 3, label.y = 750)+
  theme(legend.position = "bottom")+
  scale_color_manual(values=color, name="Subspecies",
                    labels = c(expression(paste("subsp.",italic("fastidiosa"))),
                               expression(paste("subsp.",italic("multiplex"))),
                               expression(paste("subsp.",italic("pauca"))),
                               expression(paste("subsp.",italic("sandyi")))))


ggsave("TM_boxplot.pdf", width=10, height=8, units=c("in"), dpi=700)


