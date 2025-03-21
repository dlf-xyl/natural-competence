
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
xfpa_occ <- datav[datav$Subspecies == "pauca"]
xfmo_occ <- datav[datav$Subspecies == "morus"]
xfsa_occ <- datav[datav$Subspecies == "sandyi"]

# Download or load the world shapefile
wrld <- geodata::world(path = ".")

# Set up the plot limits
xlims <- c(-130, 45)
ylims <- c(-10, 50)

wrld_cropped <- crop(wrld, ext(xlims, ylims))



################################################################################
###
### Plot paucca, morus, sandyi 

# Set up the plot
png(filename = "Figure_S1.png", width = 200, height = 130, units = "mm", res = 300)

# Set up plot dimensions and layout
par(mfrow = c(2, 2)) # Two rows, one column
# Plot 1: pauca
plot(wrld_cropped, 
     mar = c(2, 2, 2.2, 2.2), # Margins (bottom, left, top, right)
     xlim = xlims, ylim = ylims,
     col = "gray85", border = "gray80",
     #xlab = "Longitude", ylab = "Latitude",
     main = expression(italic("pauca")), 
     cex.main = 1.4, #size title
     pax = list(cex.axis= 1.4), #size labels
     box = FALSE)

# Add points for pauca
points(xfpa_occ,
       col = "#4DAF4A",
       pch = 20, 
       cex = xfpa_occ$rr_levels,
       alpha = 0.6)

# Plot 2: moruus
plot(wrld_cropped, 
     mar = c(2, 1, 2.2, 2.2), # Margins (bottom, left, top, right)
     xlim = xlims, ylim = ylims,
     col = "gray85", border = "gray80",
     #xlab = "Longitude", ylab = "Latitude",
     main = expression(italic("morus")),  , 
     cex.main = 1.4, #size title
     pax = list(cex.axis= 1.4), #size labels
     box = FALSE)


# Add points for morus
points(xfmo_occ,
       col = "#FF7F00",
       pch = 20, 
       cex = xfmo_occ$rr_levels,
       alpha = 0.9)

# Plot 3: sandyi
plot(wrld_cropped, 
     mar = c(2, 2, 2.2, 2.2), # Margins (bottom, left, top, right)
     xlim = xlims, ylim = ylims,
     col = "gray85", border = "gray80",
     #xlab = "Longitude", ylab = "Latitude",
     main = expression(italic("sandyi")),  , 
     cex.main = 1.4, #size title
     pax = list(cex.axis= 1.4), #size labels
     box = FALSE)

# Add points for sandyi
points(xfsa_occ,
       col = "purple",
       pch = 20, 
       cex = xfsa_occ$rr_levels,
       alpha = 0.6)

plot.new()
plot.window(xlim = c(0, 1), ylim = c(0, 1), asp = 1)
par(xpd = TRUE) # Allow plotting outside the plot area
legend("left", inset = c(-0.1, 0),
       y.intersp = 1.2, # Increase vertical spacing to prevent overlap
       x.intersp = 2.25, # Increase vertical spacing to prevent overlap
       legend = c("0", "0.05", "0.015", "> 0.015"),
       pch = 19, col = "gray40",
       pt.cex = c(1, 2, 3, 4), # size points
       cex = 1.2,  # size legend
       bty = "n",
       title =expression(bold("Recombination rate")),
       text.font = 1)

dev.off()



################################################################################
###
### Plot all in one


# Set up the plot
png(filename = "test_all-in-one.png", width = 200, height = 150, units = "mm", res = 300)

# Set up plot dimensions and layout
par(mfrow = c(3, 2)) # Two rows, one column

# Plot 1: fastidiosa
plot(wrld_cropped,
     mar = c(2, 2, 2.2, 2.2), # Margins (bottom, left, top, right)
     xlim = xlims, ylim = ylims,
     col = "gray85", border = "gray80",
     #xlab = "Longitude", ylab = "Latitude",
     main = expression(italic("fastidiosa")),
     cex.main = 1.4, #size title
     pax = list(cex.axis= 1.4), #size labels
     box = FALSE)

# Add points for fastidiosa
points(xff_occ,
       col = "#E41A1C",
       pch = 20,
       cex = xff_occ$rr_levels,
       alpha = 0.6)

# Plot 2: multiplex
plot(wrld_cropped,
     mar = c(2, 1, 2.2, 2.2), # Margins (bottom, left, top, right)
     xlim = xlims, ylim = ylims,
     col = "gray85", border = "gray80",
     #xlab = "Longitude", ylab = "Latitude",
     main = expression(italic("multiplex")),
     cex.main = 1.4, #size title
     pax = list(cex.axis= 1.4), #size labels
     box = FALSE)

# Add points for multiplex
points(xfm_occ,
       col = "#377EB8",
       pch = 20,
       cex = xfm_occ$rr_levels,
       alpha = 0.6)

# Plot 1: pauca
plot(wrld_cropped,
     mar = c(2, 2, 2.2, 2.2), # Margins (bottom, left, top, right)
     xlim = xlims, ylim = ylims,
     col = "gray85", border = "gray80",
     #xlab = "Longitude", ylab = "Latitude",
     main = expression(italic("pauca")),
     cex.main = 1.4, #size title
     pax = list(cex.axis= 1.4), #size labels
     box = FALSE)

# Add points for pauca
points(xfpa_occ,
       col = "#4DAF4A",
       pch = 20,
       cex = xfpa_occ$rr_levels,
       alpha = 0.6)

# Plot 2: moruus
plot(wrld_cropped,
     mar = c(2, 1, 2.2, 2.2), # Margins (bottom, left, top, right)
     xlim = xlims, ylim = ylims,
     col = "gray85", border = "gray80",
     #xlab = "Longitude", ylab = "Latitude",
     main = expression(italic("morus")),
     cex.main = 1.4, #size title
     pax = list(cex.axis= 1.4), #size labels
     box = FALSE)


# Add points for morus
points(xfmo_occ,
       col = "#FF7F00",
       pch = 20,
       cex = xfmo_occ$rr_levels,
       alpha = 0.9)

# Plot 3: sandyi
plot(wrld_cropped,
     mar = c(2, 2, 2.2, 2.2), # Margins (bottom, left, top, right)
     xlim = xlims, ylim = ylims,
     col = "gray85", border = "gray80",
     #xlab = "Longitude", ylab = "Latitude",
     main = expression(italic("sandyi")),
     cex.main = 1.4, #size title
     pax = list(cex.axis= 1.4), #size labels
     box = FALSE)



# Add points for sandyi
points(xfsa_occ,
       col = "purple",
       pch = 20,
       cex = xfsa_occ$rr_levels,
       alpha = 0.6)


# Add legend
plot.new()
plot.window(xlim = c(0, 1), ylim = c(0, 1), asp = 1)
par(xpd = TRUE) # Allow plotting outside the plot area
legend("left", inset = c(-0.1, 0),
       y.intersp = 1.15, # Increase vertical spacing to prevent overlap
       x.intersp = 2.25, # Increase vertical spacing to prevent overlap
       legend = c("0", "0.05", "0.015", "> 0.015"),
       pch = 19, col = "gray40",
       pt.cex = c(1, 2, 3, 4), # size points
       cex = 1.4,  # size legend
       bty = "n",
       title =expression(bold("Recombination rate")),
       text.font = 1)

dev.off()

