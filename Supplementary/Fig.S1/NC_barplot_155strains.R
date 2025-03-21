
library(ggplot2)
library(ggtext)
library(ggpubr)
library(dplyr)
ncsum<-read.csv("../NC_summary.csv", header=TRUE)
colnames(ncsum)[1]<- "Strain"
#log transform the recombination rate
ncsum$logRR_mean <- ifelse(ncsum$Mean > 0, ncsum$Mean*10^8, 1)
ncsum$logRR_se <- ifelse(ncsum$SE > 0, ncsum$SE*10^8, 0)


#order data by decreasing within each subsp
ncsum<-ncsum[order(ncsum$Subspecies, -(ncsum$logRR_mean)), ]
ncsum$logRR_mean

ncsum <- ncsum %>%
  arrange(Subspecies, desc(logRR_mean))

ncsum$Subspecies<-factor(ncsum$Subspecies,levels = c("subsp.fastidiosa","subsp.multiplex","subsp.pauca","subsp.morus","subsp.sandyi"))

color <- c("#FB8072","#80B2D5","#B3DE69","#EE9B43","#F3D567")

unique(ncsum$Subspecies)
fas <- subset(ncsum, Subspecies=="subsp.fastidiosa")
mul <- subset(ncsum, Subspecies=="subsp.multiplex")
other <- subset(ncsum, !Subspecies %in% c("subsp.fastidiosa","subsp.multiplex"))


p1 <- ggplot(fas, aes(x=reorder(Strain, -logRR_mean), y=logRR_mean))+
  geom_bar(stat="identity", width=.5,aes(fill = Subspecies))+
  geom_errorbar(aes(ymin=logRR_mean-logRR_se, ymax=logRR_mean+logRR_se), width=.2, position = position_dodge(.9))+
  scale_y_continuous(trans = 'log10', breaks=c(1e1, 1e2, 1e3, 1e4,1e5, 1e6, 1e7, 1e8))+
  theme_minimal()+ 
  theme(axis.text.x = element_text(angle=45, hjust=1, vjust=1.2, size = 5))+
  theme(legend.position = "bottom",
        panel.border = element_rect(color = "black", fill = NA, size = 1), 
        strip.background = element_rect(color = "black", fill = "gray90"),  
        axis.title.y = element_text(size=10),
        legend.text = element_text(size=8),
        legend.key.size = unit(5, 'mm'),
        legend.title = element_text(size=10),
        strip.text = element_text(size=10) )+
  labs(x = "Strain",
       y = expression("Log10 Recombination frequency"~(x*10^{-8})))+
  geom_hline(yintercept = 535866.1874, color="black",linetype="solid", size=0.25)+
  scale_fill_manual(values=color[1], name="Subspecies",
                    labels = c(expression(paste("subsp.",italic("fastidiosa")))))+
  stat_compare_means(label="p.signif", ref.group = "TemeculaL", hide.ns = TRUE, label.y = 8)+
  facet_wrap(~Subspecies)
p1
  
p2 <- ggplot(mul, aes(x=reorder(Strain, -logRR_mean), y=logRR_mean))+
  geom_bar(stat="identity", width=.5,aes(fill = Subspecies))+
  geom_errorbar(aes(ymin=logRR_mean-logRR_se, ymax=logRR_mean+logRR_se), width=.2, position = position_dodge(.9))+
  scale_y_continuous(trans = 'log10', breaks=c(1e1, 1e2, 1e3, 1e4,1e5, 1e6, 1e7, 1e8), limits = c(1,1e8))+
  theme_minimal()+ 
  theme(axis.text.x = element_text(angle=45, hjust=1, vjust=1.2, size = 5))+
  theme(legend.position = "bottom",
        panel.border = element_rect(color = "black", fill = NA, size = 1), 
        strip.background = element_rect(color = "black", fill = "gray90"),  
        axis.title.y = element_text(size=10),
        legend.text = element_text(size=8),
        legend.key.size = unit(5, 'mm'),
        legend.title = element_text(size=10),
        strip.text = element_text(size=10))+
  labs(x = "Strain",
       y = expression("Log10 Recombination frequency"~(x*10^{-8})))+
  geom_hline(yintercept = 535866.1874, color="black",linetype="solid", size=0.25)+
  scale_fill_manual(values=color[2], name="Subspecies",
                    labels = c(expression(paste("subsp.",italic("multiplex")))))+
  stat_compare_means(label="p.signif", ref.group = "AlmaEM3", hide.ns = TRUE, label.y = 7)+
  facet_wrap(~Subspecies)


p3<-ggplot(other, aes(x=reorder(Strain, -logRR_mean), y=logRR_mean))+
  geom_bar(stat="identity", width=.5,aes(fill = Subspecies))+
  geom_errorbar(aes(ymin=logRR_mean-logRR_se, ymax=logRR_mean+logRR_se), width=.2, position = position_dodge(.9))+
  scale_y_continuous(trans = 'log10', breaks=c(1e1, 1e2, 1e3, 1e4,1e5, 1e6, 1e7, 1e8), limits = c(1,1e8))+
  theme_minimal()+ 
  theme(axis.text.x = element_text(angle=45, hjust=1, vjust=1.2, size = 5))+
  theme(legend.position = "bottom",
        panel.border = element_rect(color = "black", fill = NA, size = 1), 
        strip.background = element_rect(color = "black", fill = "gray90"),  
        axis.title.y = element_text(size=10),
        legend.text = element_text(size=8),
        legend.key.size = unit(5, 'mm'),
        legend.title = element_text(size=10),
        strip.text = element_text(size=10))+
  labs(x = "Strain",
       y = expression("Log10 Recombination frequency"~(x*10^{-8})))+
  #geom_hline(yintercept = 535866.1874, color="black",linetype="solid", size=0.25)+
  scale_fill_manual(values=color[3:5], name="Subspecies",
                    labels = c(expression(paste("subsp.",italic("pauca"))),
                    expression(paste("subsp.",italic("morus"))),
                    expression(paste("subsp.",italic("sandyi")))))+
  facet_grid(~Subspecies, space = "free", scales = "free_x")

                               
# Load the patchwork package
library(patchwork)

# Combine plots with controlled widths
combined_plot <- (p1 | p2) / p3 + 
  plot_layout(widths = c(1, 1), heights = c(1, 0.5))  # Adjust widths and heights as needed

# Display the combined plot
combined_plot

ggsave("NC_barplot_v2.pdf",width = 12, height = 7, units = "in", dpi = 700)

