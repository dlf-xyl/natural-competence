library(ape)
library(brms)
library(dplyr)
library(geiger)
library(phytools)
library(sjPlot)
library(gridExtra)
library(ggbreak)
library(performance)
library(ggplot2)
library(factoextra)

#get the tree ready
t <- read.tree('RAxML_bestTree.tre')
rt <- midpoint_root(t) # re-root on midpoint
tbin <- multi2di(rt) #randomly resolve any multifurcations
tt <- chronoMPL(tbin) #ultrametricize
##calculate matrix of variances and covariances based on the phylogeny
#assuming Brownian motion
A <- ape::vcv.phylo(tt)

#get the feature data
d <- read.csv('XfRec_updated2025Jan-v2.csv')
d <- d %>% filter(Strain!="M1") #this one isn't in the phylogeny
#collapse dataset to one record per strain
d$mRate <- with(d, sapply(split(Rate, Strain), mean)[Strain]) #mean
d <- d %>% group_by(Strain) 
d <- d %>% filter(row_number()==1)

#histogram of the breaks
ggplot(d, aes(logRate)) + geom_histogram()  + scale_y_break(c(15, 60))

#make a host-genus factor
foo <- strsplit(d$Host_sn, ' ')
genera <- character()
for (i in 1:length(foo)){
    genus <- foo[[i]][1]
    genera <- c(genera, genus)
}
d$Host_genus <- genera
d$Host_sn <- as.factor(d$Host_sn)
d$Host_genus <- as.factor(d$Host_genus)

#ordinate the climate data
D <- d %>% dplyr::select(starts_with("BIO"))
D$Strain <- NULL
pcs <- prcomp(D , scale = T, center = T) ## add scale = T
d$cPC1 <- pcs$x[,1]
d$cPC2 <- pcs$x[,2]
d$cPC3 <- pcs$x[,3]

## PCA biplot of axes 1,2 and 2,3
fviz_pca_biplot(pcs, axe=c(1,2),
             repel = TRUE ,
             select.var = list(cos2 = 0.89),
             label = "var"
)
fviz_pca_biplot(pcs, axe=c(3,2),
                repel = TRUE ,
                select.var = list(cos2 = 0.68),
                label = "var"
)

#cut down to just the variables that we'll use
dd <- data.frame(d)
keepers = c('Strain', 'Host_genus', 'Location1', 'Location0', 'mRate', 'cPC1', 'cPC2', 'cPC3', 'Latitude', 'Longitude')
dd <- dd[,keepers]
#rescale
dd$cPC1 <- scale(dd$cPC1)
dd$cPC2 <- scale(dd$cPC2)
dd$cPC3 <- scale(dd$cPC3)
dd$Latitude <- scale(dd$Latitude)
dd$Logitude <- scale(dd$Longitude)


#fit a phylogenetic regression model with Location0 as random effect
m1 <- brm(
  mRate ~ cPC1 + cPC2 + cPC3 + (1|gr(Strain, cov = A)) + (1|Host_genus) + (1|Location0),
  data = dd,
  family=hurdle_lognormal(),
  data2 = list(A = A),
  #setting some stronger priors seems to help a bit with the search
  prior = c(
    prior(normal(0, 5), "b"),
    prior(normal(0, 10), "Intercept"),
    prior(student_t(3, 0, 20), "sd"),
    prior(student_t(3, 0, 20), "sigma")
  )
)

summary(m1)
icc(m1, by_group = T) 


#make some plots
set_theme(base=theme_538())
plot_model(m1, vline.color='gold', bpe.style='dot', bpe.color='pink', dot.size=2)


