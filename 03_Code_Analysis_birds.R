###################################################################################################
# Analysis of the dependence of the abundance of breeding birds in different kinds of shrub cover #
###################################################################################################

# Analyzing of abundance of breeding bird species in the 
# cells of a grid on the natural park "Ebenberg"

# Install Packages
library(latticeExtra) # Additional functions for plot
require(ggplot2)    
require(vegan)        # needed for `decostand`, `specnumber`
require(permute)      
require(lattice)      # needed for  `xyplot`
require(mvabund)      # needed for  `mvabund()`, `manyglm()`

# Working directory
setwd("L:/leon/Studium/Bachelor/Bachelorarbeit/Statistik")

# Read in data 
env19=read.table("env2019_subset.txt",header=T,sep="\t")
species19=read.table("spec2019_subset.txt",header=T,sep="\t")

attach(env19)
attach(species19)

# Processing and analyzing of the data #
species1 <- species19[-1]
species2 <- decostand(species19[-1],"pa") # skip first column and presence-absence-Transformation

# Number of cells, in which the analyzed species is abundant
specsum <- apply(species2,2,sum)
sort(specsum)

# removal of rare species: remove species that occur in less than 5 cells
subset19 <- species2[ , ! specsum<5]

colSums(subset19)

Artenzahl <- specnumber(species2) # number of species per cell

# Summarize of different types of shrub cover 
env19$shrub=(env19$Gebuesch+env19$shrub_high+env19$shrub_medium)/env19$Area1
env19$open=(env19$herb_layer+env19$Mahd+env19$shrub_low)/env19$Area1

# Statistical tests #

# mvabund (multivariat analysis) 
wdat <- mvabund(subset19)

quadshrub=env19$shrub^2  # square shrub to get a unimodal distribution in the model

glm.1 <- manyglm(wdat ~ shrub+quadshrub, family="binomial", data=env19)
glm.2 <- manyglm(wdat ~ shrub, family="binomial", data=env19)

plot(glm.1)
plot(glm.2)

drop1(glm.1,glm.2) #AIC of models better with squared term 

anova(glm.1) # final result

anova(glm.1, p.uni="adjusted") # tests all species one by one


# Plotting #

f <- seq(0, 1, length.out =229)^2
s <- seq(0, 1, length.out = 229)

newdata <- expand.grid(shrub=s) #open=t , low=l, forest1=r, 
newdata2=cbind(newdata,quadshrub=f)
names(newdata2)

pred.w.plim <- predict(glm.1, newdata2, type = "response",interval="prediction")
pred.w.clim <- predict(glm.1, newdata2, type = "response",interval="confidence")
newdatpred =cbind(newdata2,pred.w.plim )

# lty=1 (straight line): significant result of shrub cover and abundance of bird specie
# lty=2 (pointed line): no significant result of shrub cover and abundance of bird specie

# Species with no significant depency on shrub cover
png("not_significant_species.png", width=2480, height=3508, res=300) 

xyplot(Bluthaenfling+Feldlerche+Gartengrasmuecke+
         Gruenspecht+Neuntoeter+Ringeltaube+Schwarzkehlchen
       +Wendehals
       ~shrub, 
       col=c("blue","red","chartreuse3","darkblue","darkorange2","purple",
             "darkgreen","magenta","cornflowerblue","darkred","cyan4","lightgreen","black"),
       ylab="Probability of occurence", xlab="Shrub cover",
       auto.key=list(columns = 3, points = FALSE, lines = FALSE,
                     col=c("blue","red","chartreuse3","darkblue","darkorange2","purple",
                           "darkgreen","magenta","cornflowerblue","darkred",
                           "cyan4","lightgreen","black")),
       type = "l", lty=c(2,2,2,2,2,2,2,2), lwd=2, data =  newdatpred)
dev.off()

# Species with significant depency on shrub cover
png("significant_species.png", width=2480, height=3508, res=300)  

xyplot(Dorngrasmuecke+Fitis+Heckenbraunelle+Moenchsgrasmuecke+ 
         Nachtigall+Ringeltaube+Turteltaube+Zilpzalp
       ~shrub, 
       col=c("blue","red","chartreuse3","darkorange2","darkblue",
             "darkgreen","magenta","cornflowerblue"), 
       ylab="Probability of occurence", xlab="Shrub cover",
       auto.key=list(columns = 3, points = FALSE, lines = FALSE,
                     col=c("blue","red","chartreuse3","darkorange2","darkblue",
                           "darkgreen","magenta","cornflowerblue")),
       type = "l", lwd=2,data =  newdatpred, ylim=c(-0.03,0.63))
dev.off()

