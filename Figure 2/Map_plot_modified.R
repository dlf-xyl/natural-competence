
library(magrittr)
library(terra)
library(ggplot2)
library(ggrepel)

data <- read.csv("GPS_coordinates.csv", header = T)


# Plot combined data, with facet_wrap for each Subspecies
df.fm <- subset(data, Subspecies %in% c("fastidiosa", "multiplex"))

subcol <- c("#E41A1C", "#377EB8", "#4DAF4A", "#FF7F00", "#FFFF33")
df.fm$Subspecies <- factor(df.fm$Subspecies, levels = c("fastidiosa", "multiplex"))

#subsp. fastidiosa and multiplex
ggplot() +
  borders("world", colour = "gray85", fill = "gray80") +
  geom_point(data = df.fm, aes(x = Longitude, y = Latitude, color = Subspecies, size = rr), alpha = 0.7) +
  scale_size(range = c(1, 10)) +  # Adjust point size range for better visualization
  scale_color_manual(values = subcol[1:2]) +
  theme_minimal() +
  theme(legend.position = "right") +
  labs(title = "",
       x = "Longitude", y = "Latitude", color = "Strain Subspecies", size = "Recombination Rate") + 
  coord_cartesian(xlim = c(-125, 45), ylim = c(20, 50)) +  # Adjust latitude and longitude to cover both regions
  theme(
    legend.position = "right",
    legend.title = element_text(size = 8),
    legend.text = element_text(size = 6, face = "italic"),
    legend.key.size = unit(3, "mm"),
    axis.title = element_text(size = 8),
    axis.text = element_text(size=6)
  ) +
  guides(
    color = guide_legend(override.aes = list(size = .5)),# Increase point size in color legend
    size = guide_legend(override.aes = list(size = c(1,2,3,4))) # Increase point size in color legend
  ) +
  facet_wrap(~Subspecies, ncol = 1) 

ggsave( "combined.fm.pdf", width = 6, height = 3, units = "in", dpi=700)

save.image("122924.RData")

