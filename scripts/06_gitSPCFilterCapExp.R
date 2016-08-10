# Filtering and capping exploration
# SB
# August 8, 2016

setwd("~/GitHub/cred_fish")
load("data/TMPwsd.Rdata")

library(vegan)
library(reshape)
library(ggplot2)
library(plyr)

install.packages('devtools')
library(devtools)
install_github('fawda123/ggord')
library(ggord) #??

install_github("vqv/ggbiplot")
library(ggbiplot)

###########################
# Filtering

# All Islands together 
# Abundance
island_bio.matrix <- wsd[c(3,6,685:1332,1344)]
bio.onlysp <- island_bio.matrix[-c(1,2,651)]
site.info <- data.frame(wsd$ISLAND, wsd$SITE)
colnames(site.info) <- c("ISLAND", "SITE")

sp.pca <- prcomp(matrix.onlysp, scaling = T)
plot(sp.pca)
summary(sp.pca)
biplot(sp.pca)

#Biomass
bio.pca <- prcomp(bio.onlysp, scaling = T)
plot(bio.pca)
summary(bio.pca)
biplot(bio.pca)

#CHCH - 150
#MABI - 384
bio.rmMABI_CHCH <- bio.onlysp[-c(150,384)]

rm.pca <- prcomp(bio.rmMABI_CHCH, scaling = T)
plot(rm.pca)
biplot(rm.pca)
summary(rm.pca)

#BOMU - 90
bio.rmMABI_CHCH_BOMU <- bio.onlysp[-c(90,150,384)]

rm2.pca <- prcomp(bio.rmMABI_CHCH_BOMU, scaling = T)
plot(rm2.pca)
biplot(rm2.pca)
summary(rm2.pca)

# Each Island
#BAKER - remove MABI
bak.bio.matrix <- subset(island_bio.matrix, ISLAND == "Baker")
bak.bio <- bak.bio.matrix[-c(1,2,386,651)]
bak.pca <- prcomp(bak.bio, scaling = T)
plot(bak.pca)
biplot(bak.pca)

#HOWLAND - remove MABI
how.bio.matrix <- subset(island_bio.matrix, ISLAND == "Howland")
how.bio <- how.bio.matrix[-c(1,2,386,651)]
how.pca <- prcomp(how.bio, scaling = T)
plot(how.pca)
biplot(how.pca)

#JARVIS - remove MABI
jar.bio.matrix <- subset(island_bio.matrix, ISLAND == "Jarvis")
jar.bio <- jar.bio.matrix[-c(1,2,386,651)]
jar.pca <- prcomp(jar.bio, scaling = T)
plot(jar.pca)
biplot(jar.pca)

#JOHNSTON - remove MABI
joh.bio.matrix <- subset(island_bio.matrix, ISLAND == "Johnston")
joh.bio <- joh.bio.matrix[-c(1,2,386,651)]
joh.pca <- prcomp(joh.bio, scaling = T)
plot(joh.pca)
biplot(joh.pca)

#KINGMAN - remove MABI, CHCH
kin.bio.matrix <- subset(island_bio.matrix, ISLAND == "Kingman")
kin.bio <- kin.bio.matrix[-c(1,2,386,651)]
kin.pca <- prcomp(kin.bio, scaling = T)
plot(kin.pca)
biplot(kin.pca)

kin.bio.rmCHCH <- kin.bio.matrix[-c(1,2,152,386,651)]
kin.pca.rmCHCH <- prcomp(kin.bio.rmCHCH, scaling = T)
plot(kin.pca.rmCHCH)
biplot(kin.pca.rmCHCH)

#PALMYRA - remove MABI
pal.bio.matrix <- subset(island_bio.matrix, ISLAND == "Palmyra")
pal.bio <- pal.bio.matrix[-c(1,2,386,651)]
pal.pca <- prcomp(pal.bio, scaling = T)
plot(pal.pca)
biplot(pal.pca)

pal.bio.rmCHCH <- pal.bio.matrix[-c(1,2,152,386,651)]
pal.pca.rmCHCH <- prcomp(pal.bio.rmCHCH, scaling = T)
plot(pal.pca.rmCHCH)
biplot(pal.pca.rmCHCH)

#WAKE - remove MABI, CHCH, BOMU
wak.bio.matrix <- subset(island_bio.matrix, ISLAND == "Wake")
wak.bio <- wak.bio.matrix[-c(1,2,92,386,651)]
wak.pca <- prcomp(wak.bio, scaling = T)
plot(wak.pca)
biplot(wak.pca)

# for (i in island_bio.matrix$ISLAND){
  jpeg(file = paste("graphs_tables/filter_cap/pca_rmMABI",i))
  
  d <- subset(island_bio.matrix, ISLAND == i)
  
  d.pca <- prcomp(d[-c(1,2,386,651)], scaling = T)
  
  biplot(d.pca)
  
  dev.off()
} #Doesn't quite work... 

################################

# Capping

# First explore without MABI
wsd$rmMABI <- (wsd$TotFish - wsd$MABI)

for (i in wsd$ISLAND){
 
  jpeg(file = paste("graphs_tables/filter_cap/hist_rmMABI",i))
  
  d <- subset(wsd, ISLAND == i)
  hist(d$rmMABI)
  
  dev.off()
  
  }


