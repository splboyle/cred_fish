## Data Exploration
# SB
# July 12, 2016

setwd("/Users/ShanBam/GitHub/cred_fish")
load("data/TMPwsd.Rdata")
richness <- read.csv("data/species_richness_by_SITEVISITID_SPC.csv")

library(ggplot2)
library(scales)

st.err <- function(x, na.rm=FALSE) {
  if(na.rm==TRUE) x <- na.omit(x)
  sd(x)/sqrt(length(x))
}

# Calculate mean wave energy across all years, SD and SE
wsd$meanWV <- rowMeans(wsd[,c("X2002", "X2003", "X2004", "X2005", "X2006", "X2007", "X2008", "X2009", "X2010", "X2011", "X2012")], na.rm=TRUE)
wsd$WV_SE <- apply(wsd[1333:1339], 1, st.err)
wsd$WV_SD <- apply(wsd[1333:1339], 1, sd)

# Add spcies richness data to wsd 
richnessSPC <- subset(richness, METHOD == "nSPC")
richnessSPC$X <- NULL
richnessSPC$METHOD <- NULL
wsd <- merge(richnessSPC, wsd, by = "SITEVISITID")

# Cap data at 95th and 5th percentile... ?  
wsd[is.na(wsd)] <- NA # change NaN to NA
# tofish_cap <- as.data.frame(squish(wsd$TotFish, range = quantile(wsd$TotFish, c(.05, .95))))
# tot <- data.frame(wsd$SITE, totfish_cap)
# colnames(tot) <- c("SITE", "TotFishCap")
# wsd <- merge(tot, wsd, by = "SITE")


########## Data Exploration ##########
### BIOMASS 
response<-c("TotFish")
response_label<-c("Total Fish Biomass") # label you want
pred.cols<-c("SD_SH_DIFF","HARD_CORAL", "MA", "CCA", "DEPTH")
pred_label<-c("Benthic Complexity", "Coral Cover", "Macroalgal Cover", "CCA Cover", "Depth") # predictor labels 

for(i in 1:length(pred.cols)){
##i<-1
d<-wsd[,c(response, pred.cols[i], "ISLAND")]
p.site <- ggplot(d, aes_string(x = unique(pred.cols)[i], y = response))+ 
 	geom_point(na.rm = T) + 
	geom_smooth(method="lm", formula=y~x, na.rm = T) +
    scale_y_log10() +
    facet_wrap(~ISLAND) +
    labs(x = pred_label[i], y = response_label)+ theme_classic()
  print(p.site)
  ggsave(p.site, filename=paste("ExploreSite",i,".png",sep=""), path = "graphs_tables/site_exploratory")
} 

# Mean wave (not included above) because need only forereef sites and where mean wave is greater than 0
ggplot(subset(wsd, REEF_ZONE == "Forereef" & meanWV >0), aes(meanWV, TotFish)) + 
  geom_point() + 
  geom_smooth(method = "lm", formula = y~x) +
  scale_y_log10() +
  facet_wrap(~ISLAND) +
  theme_classic() +
  labs(x = "Mean Wave Energy", y = expression(paste("Fish biomass (g ", m^-2,")")))

### RICHNESS

ggplot(subset(wsd, REEF_ZONE == "Forereef" & meanWV >0), aes(meanWV, SPECIESRICHNESS)) + 
  geom_point() + 
  geom_smooth(method = "lm", formula = y~x) +
  scale_y_log10() +
  facet_wrap(~ISLAND) +
  theme_classic() +
  labs(x = "Mean Wave Energy", y = expression(paste("Species Richness")))

response1<-c("SPECIESRICHNESS")
response_label1<-c("Richness") # label you want
pred.cols1<-c("SD_SH_DIFF","HARD_CORAL", "MA", "CCA", "DEPTH")
pred_label1<-c("Benthic Complexity", "Coral Cover", "Macroalgal Cover", "CCA Cover", "Depth") # predictor labels

for(i in 1:length(pred.cols1)){
  ##i<-1
  dd<-wsd[,c(response1, pred.cols1[i], "ISLAND")]
  pp <- ggplot(dd, aes_string(x = unique(pred.cols1)[i], y = response1))+ 
    geom_point(na.rm = T) + 
    geom_smooth(method="lm", formula=y~x, na.rm = T) +
    scale_y_log10() +
    facet_wrap(~ISLAND) +
    labs(x = pred_label1[i], y = response_label1) + 
    theme_classic()
  print(pp)
  ggsave(pp, filename=paste("Richness",i,".png",sep=""), path = "graphs_tables/site_exploratory")
} 

ggplot(wsd, aes(SPECIESRICHNESS, TotFish)) +
  geom_point() +
  scale_y_log10() +
  geom_smooth(method = "lm", formula = y~x, na.rm = T) +
  theme_classic()

ggplot(wsd, aes(ISLAND, SPECIESRICHNESS, fill = ISLAND)) +
  geom_boxplot() +
  theme_bw() + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + 
  theme(axis.title.x = element_blank()) +
  theme(legend.position = "none")
  
ggplot(wsd, aes(ISLAND, TotFish, fill = ISLAND)) +
  geom_boxplot() +
  theme_bw() + 
  scale_y_log10() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + 
  theme(axis.title.x = element_blank()) +
  theme(legend.position = "none")

ggplot(wsd, aes(ISLAND, PISCIVORE, fill = ISLAND)) +
  geom_boxplot() +
  theme_bw() + 
  scale_y_log10() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + 
  theme(axis.title.x = element_blank()) +
  theme(legend.position = "none")

ggplot(wsd, aes(ISLAND, PLANKTIVORE, fill = ISLAND)) +
  geom_boxplot() +
  theme_bw() + 
  scale_y_log10() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + 
  theme(axis.title.x = element_blank()) +
  theme(legend.position = "none")

ggplot(wsd, aes(ISLAND, PRIMARY, fill = ISLAND)) +
  geom_boxplot() +
  theme_bw() + 
  scale_y_log10() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + 
  theme(axis.title.x = element_blank()) +
  theme(legend.position = "none")

ggplot(wsd, aes(ISLAND, SECONDARY, fill = ISLAND)) +
  geom_boxplot() +
  theme_bw() + 
  scale_y_log10() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + 
  theme(axis.title.x = element_blank()) +
  theme(legend.position = "none")
  
ggplot(wsd, aes(CCA, MA)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y~x, na.rm = T)