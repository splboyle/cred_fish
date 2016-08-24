rm(list=ls())
setwd("G:/Fish/SiteProject")
load("TMPwsd.Rdata")
load("FG_GF.Rdata")

#########
require(reshape2)
require(gradientForest)
require(ggplot2)
require(extendedForest)
require(grid)

source("gradForestFunc_v002.R")

dat <- read.csv("dat.csv")
dat$X <- NULL
dat$SITE.1 <- NULL


## Fish Indicators 

## Habitat Drivers
#"DEPTH"		"HARD_CORAL"	"MA"			"CCA"		"ISLAND"
#"SD_SH_DIFF"	"meanWV"		"REEF_ZONE"		"WV_SD"

ind.name<-c("Benthic omnivore", "Browser", "Bumphead", "Coastal Pelagic Piscivore", "Corallivore", "Diurnal piscivore", "Diurnal planktivore", "Grazer", "Grazer/detritivore", "Large excavators/bioeroders", "Macro-invertivore", "Micro-invertivore", "Nocturnal piscivore", "Nocturnal planktivore", "OTHER", "Pisci-invertivore", "Roving Mid Water Herbivore", "Roving Mid Water Planktivore", "Roving Nocturnal Planktivore", "Roving Pisci-invertivore", "Schooling Micro-invertivore", "Scrapers/small excavators", "Small Grazers")
dri.name<-c("DEPTH", "HARD_CORAL", "MA", "CCA", "SD_SH_DIFF", "meanWV", "WV_SD")


PIenv <- data.frame(FG$SITE, FG$ISLAND, FG$DEPTH, FG$meanWV, FG$WV_SD, FG$HARD_CORAL, FG$SAND, FG$MA, FG$CCA, FG$SD_SH_DIFF)
colnames(PIenv) <- c("SITE",  "ISLAND", "DEPTH", "meanWV", "WV_SD", "HARD_CORAL", "SAND", "MA", "CCA", "SD_SH_DIFF") 

PIbio <- data.frame(FG$SITE, FG$'Benthic omnivore', FG$Browser, FG$Bumphead, FG$'Coastal Pelagic Piscivore', FG$Corallivore, FG$'Diurnal piscivore', FG$'Diurnal planktivore', FG$Grazer, FG$'Grazer/detritivore', FG$'Large excavators/bioeroders', FG$'Macro-invertivore', FG$'Micro-invertivore', FG$'Nocturnal piscivore', FG$'Nocturnal planktivore', FG$OTHER, FG$'Pisci-invertivore', FG$'Roving Mid Water Herbivore', FG$'Roving Mid Water Planktivore', FG$'Roving Nocturnal Planktivore', FG$'Roving Pisci-invertivore', FG$'Schooling Micro-invertivore', FG$'Scrapers/small excavators', FG$'Small Grazers')
colnames(PIbio) <- c("SITE", "Benthic omnivore", "Browser", "Bumphead", "Coastal Pelagic Piscivore", "Corallivore", "Diurnal piscivore", "Diurnal planktivore", "Grazer", "Grazer/detritivore", "Large excavators/bioeroders", "Macro-invertivore", "Micro-invertivore", "Nocturnal piscivore", "Nocturnal planktivore", "OTHER", "Pisci-invertivore", "Roving Mid Water Herbivore", "Roving Mid Water Planktivore", "Roving Nocturnal Planktivore", "Roving Pisci-invertivore", "Schooling Micro-invertivore", "Scrapers/small excavators", "Small Grazers")

PIenv[ is.na(PIenv) ] <- NA

dim(PIenv)
dim(PIbio)

dat<-merge(PIenv, PIbio, by = "SITE")

write.csv(dat, "dat2.csv")

dat_line <- subset(dat, ISLAND != "Baker" & ISLAND != "Howland" & ISLAND != "Johnston" & ISLAND != "Wake")
 

# Many of the pressure variables do not have full data (some NA):

## Option 1- Impute:
dat <- na.roughfix(dat)

# Maximum level of splits
lev <- floor(log2(nrow(FG) * 0.368/2))

colnames(dat)[!colnames(dat) %in% c(dri.name, ind.name)]
#dat<-data.frame(dat)
#
## GF analysis ##

all <-   gradientForest(data = FG, 
                           predictor.vars = dri.name, 
                           response.vars = ind.name,
                           ntree = 1000, 
                           transform = NULL,
                           maxLevel = lev,
                           corr.threshold = 0.5, 
                           compact = F,
                           trace = T)

lev <- floor(log2(nrow(subset(FG, ISLAND != "Baker" & ISLAND != "Howland" & ISLAND != "Johnston" & ISLAND != "Wake")) * 0.368/2))
line <-   gradientForest(data = subset(FG, ISLAND != "Baker" & ISLAND != "Howland" & ISLAND != "Johnston" & ISLAND != "Wake"), 
                           predictor.vars = dri.name, 
                           response.vars = ind.name,
                           ntree = 1000, 
                           transform = NULL,
                           maxLevel = lev,
                           corr.threshold = 0.5, 
                           compact = F,
                           trace = T)

lev <- floor(log2(nrow(subset(FG,  ISLAND != "Palmyra" & ISLAND != "Jarvis" & ISLAND != "Kingman" & ISLAND != "Johnston" & ISLAND != "Wake")) * 0.368/2))
phoenix <-   gradientForest(data = subset(FG, ISLAND != "Palmyra" & ISLAND != "Jarvis" & ISLAND != "Kingman" & ISLAND != "Johnston" & ISLAND != "Wake"), 
                           predictor.vars = dri.name, 
                           response.vars = ind.name,
                           ntree = 1000, 
                           transform = NULL,
                           maxLevel = lev,
                           corr.threshold = 0.5, 
                           compact = F,
                           trace = T)

lev <- floor(log2(nrow(subset(FG, ISLAND == "Wake")) * 0.368/2))
wake <-   gradientForest(data = (subset(FG, ISLAND == "Wake")), 
                           predictor.vars = dri.name, 
                           response.vars = ind.name,
                           ntree = 1000, 
                           transform = NULL,
                           maxLevel = lev,
                           corr.threshold = 0.5, 
                           compact = F,
                           trace = T)

lev <- floor(log2(nrow(subset(FG, ISLAND == "Johnston")) * 0.368/2))
johnston <-   gradientForest(data = (subset(FG, ISLAND == "Johnston")), 
                           predictor.vars = dri.name, 
                           response.vars = ind.name,
                           ntree = 1000, 
                           transform = NULL,
                           maxLevel = lev,
                           corr.threshold = 0.5, 
                           compact = F,
                           trace = T)

# All: [1] DEPTH      CCA        meanWV     HARD_CORAL SD_SH_DIFF

# Wake: [1] SD_SH_DIFF DEPTH      HARD_CORAL meanWV     WV_SD  

# Johnston: [1] SD_SH_DIFF meanWV     CCA        HARD_CORAL WV_SD    

# Line: [1] DEPTH      CCA        SD_SH_DIFF HARD_CORAL MA 

# Phoenix: [1] DEPTH      meanWV     HARD_CORAL SD_SH_DIFF WV_SD   

# Save Rdata
save(johnston, file = "functional_GF/johnston/johnstonGF.RDATA")


###########
## PLOTS ##
###########

most_important <- names(importance(johnston))[1:7]

png(file = "functional_GF/johnston/johnston_var_importance.png")
plot(johnston, plot.type = "O")
dev.off()

png(file = "functional_GF/johnston/johnston_split_density.png",  width = 173.5, height = 173.5, units = "mm", res = 600)
plot(johnston, plot.type = "S", imp.vars = most_important, leg.posn = "topright", cex.legend = 0.4, cex.axis = 0.6, cex.lab = 0.7, johnston.ylab = 0.9, par.args = list(mgp = c(1.5, 0.5, 0), mar = c(3.1, 1.5, 0.1, 1)))
dev.off()

png(file = "functional_GF/johnston/johnston_sp_cumulative.png",  width = 173.5, height = 173.5, units = "mm", res = 600)
plot(johnston, plot.type = "C", imp.vars = most_important, show.overall = F, legend = T, leg.posn = "topleft", leg.nspecies = 5, cex.lab = 0.7, cex.legend = 0.4, cex.axis = 0.6, johnston.ylab = 0.9, par.args = list(mgp = c(1.5, 0.5, 0), mar = c(2.5, 1, 0.1, 0.5), omi = c(0, 0.3, 0, 0)))
dev.off()

png(file = "functional_GF/johnston/johnston_predictor_cumulative.png",  width = 173.5, height = 173.5, units = "mm", res = 600)
plot(johnston, plot.type = "C", imp.vars = most_important, show.species = F, common.scale = T, cex.axis = 0.6, cex.lab = 0.7, johnston.ylab = 0.9, par.args = list(mgp = c(1.5, 0.5, 0), mar = c(2.5, 1, 0.1, 0.5), omi = c(0, 0.3, 0, 0)))
dev.off()

png(file = "functional_GF/johnston/johnston_model_performance.png",  width = 173.5, height = 173.5, units = "mm", res = 600)
plot(johnston, plot.type = "P", show.names = F, horizontal = F, cex.axis = 1, cex.labels = 0.7, johnston = 2.5)
dev.off()

