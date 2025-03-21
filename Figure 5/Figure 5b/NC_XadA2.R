library(ggplot2)
library(xlsx)
library(ggpubr)
library(scales)
library(rstatix)
library(dplyr)

df <- read.csv("NC_XadA2.xlsx", header = T)

summary_df <- df %>%
  group_by(Strain) %>%
  summarise(mean=mean(Rate),
            se=sd(Rate)/sqrt(n()))

ggplot(df, aes(x = Strain, y = Rate, fill = Strain)) +
  stat_summary(
    fun = mean, geom = "bar", 
    color = "black", width = 0.6, position = position_dodge()) +
  geom_jitter(
    shape = 16, size = 1.5, width = 0.2, height = 0) +
  geom_errorbar(
    data = summary_df, aes(x = Strain, ymin = mean - se, ymax = mean + se),
    width = 0.2, color = "black", inherit.aes = FALSE) +
  labs(
    x = "Strain",
    y = "Recombination rate (CFU/mL)") +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.title = element_text(size = 10),
    axis.text = element_text(size = 8)
  )+
  stat_compare_means(label.y = 0.015, size=4)+
  scale_x_discrete(
    labels = c(
      "TemeculaL" = "TemeculaL",
      "XadA2" = expression(italic(Delta) * italic("XadA2")))
  )+
  scale_y_continuous(labels = scales::scientific, breaks = seq(0, 0.015, by = 0.002) ) 



ggsave("NC_XadA2.pdf", width = 6, height = 4, units = "in", dpi = 700)