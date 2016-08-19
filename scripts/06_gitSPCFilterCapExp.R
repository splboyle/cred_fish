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

b <- PCA(bak.bio, stand = T)



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
  } 
# Doesn't quite work... 

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


ggplot(data = wsd, aes(x = ISLAND, y = TotFish)) +
  geom_boxplot()

ggplot(data = wsd, aes(x = ISLAND, y = rmMABI)) +
  geom_boxplot() 

b <- subset(wsd, ISLAND == "Baker")
b <- subset(wsd, ISLAND == "Howland")
b <- subset(wsd, ISLAND == "Jarvis")
b <- subset(wsd, ISLAND == "Johnston")
b <- subset(wsd, ISLAND == "Kingman")
b <- subset(wsd, ISLAND == "Palmyra")
b <- subset(wsd, ISLAND == "Wake")

b <- data.frame(b$SITE, b$rmMABI)

bx <- boxplot(b$b.rmMABI)
bx$stats

# BAKER boxplot stats - 6 outliers
# [1,]   8.185154
# [2,]  44.918691
# [3,]  75.097054
# [4,] 118.334215
# [5,] 214.000100

# HOWLAND boxplot stats - 3 outliers
# [1,]  15.41959
# [2,]  63.27150
# [3,]  94.25418
# [4,] 130.64459
# [5,] 216.51090

# JARVIS boxplot stats - 8 outliers
# [1,]   5.459686
# [2,]  77.868453
# [3,] 112.704238
# [4,] 156.278859
# [5,] 262.738789

# JOHNSTON boxplot stats - 8 outliers
# [1,]  1.437525
# [2,] 15.069649
# [3,] 24.752408
# [4,] 47.514122
# [5,] 94.284919



