
library(magrittr)
library(terra)
library(geodata)

# occ data
data <- read.csv("GPS_coordinates.csv", header = T)
data$rr %>% hist()

data$rr_levels <- ifelse(data$rr < 0.05, 1, ifelse(data$rr < 0.1, 2.5, ifelse(data$rr < 0.15, 4, 5.5)))

datav <- vect(data, geom = c("Longitude", "Latitude"))

# subset data
xff_occ <- datav[datav$Subspecies == "fastidiosa"]
xfm_occ <- datav[datav$Subspecies == "multiplex"]

# Download or load the world shapefile
wrld <- geodata::world(path = ".")

# Set up the plot limits
xlims <- c(-130, 45)
ylims <- c(10, 50)

wrld_cropped <- crop(wrld, ext(xlims, ylims))

# Set up the plot
png(filename = "Figure1.png", width = 250, height = 150, units = "mm", res = 300)

# Set up plot dimensions and layout
par(mfrow = c(2, 1)) # Two rows, one column
# Plot 1: fastidiosa
plot(wrld_cropped, 
     mar = c(3, 1, 2.2, 7), # Margins (bottom, left, top, right)
     xlim = xlims, ylim = ylims,
     col = "gray85", border = "gray80",
     #xlab = "Longitude", ylab = "Latitude",
     main = expression(italic("fastidiosa")), box = FALSE)

# Add points for fastidiosa
points(xff_occ,
       col = "#E41A1C",
       pch = 20, 
       cex = xff_occ$rr_levels,
       alpha = 0.6)

# Add legend
par(xpd = TRUE) # Allow plotting outside the plot area
legend("topright", inset = c(-0.1, -0.1), 
       y.intersp = 1, # Increase vertical spacing to prevent overlap
       x.intersp = 2.25, # Increase vertical spacing to prevent overlap
       legend = c("0", "0.05", "0.015", "> 0.015"), 
       pch = 19, col = "gray40", pt.cex = c(1, 2, 3, 4), cex = 0.8, bty = "n",
       title =expression(bold("Recombination rate")), 
       text.font = 1)


# Plot 2: multiplex
plot(wrld_cropped, 
     mar = c(3, 1, 1.9, 7), # Margins (bottom, left, top, right)
     xlim = xlims, ylim = ylims,
     col = "gray85", border = "gray80",
     #xlab = "Longitude", ylab = "Latitude",
     main = expression(italic("multiplex")),  box = FALSE)

# Add points for multiplex
points(xfm_occ,
       col = "#377EB8",
       pch = 20, 
       cex = xfm_occ$rr_levels,
       alpha = 0.6)
dev.off()

