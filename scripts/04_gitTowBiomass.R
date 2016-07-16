# Fish Biomass Plots
# SB
# July 13, 2016

setwd("/Users/ShanBam/GitHub/cred_fish")
TMP.FishTow_Mean_island_year_BIO <- read.csv("data/TMP.FishTow_Mean_island_year_BIO")
load("data/TMPspecies.Rdata")

### Color Brewer
library(ggplot2)
if (!require("RColorBrewer")) {
  install.packages("RColorBrewer")
  library(RColorBrewer)
}
display.brewer.all()
colorRampPalette(brewer.pal(4,"Dark2"))(4) #This will generate 100 colours based on the 9 from the ‘Blues’ palette.

FishTowSE <- data.frame(TMP.FishTow_SE_island_year_BIO$X, TMP.FishTow_SE_island_year_BIO$TotFish)
colnames(FishTowSE) <- c("X", "SE")
FishTowMeanSE <- merge(TMP.FishTow_Mean_island_year_BIO, FishTowSE, by = "X")


### This is each island mean tow biomass for each year (change over time) stacked by consumer group - do we want this? Similar to Pg. 29 in AmSam overview  
MeanSETowBiomass <- do.call(data.frame, aggregate(BIOGM2~REGION+ISLAND+OBS_YEAR+TROPHIC_MONREP, data = subset(TMPtowData, REEF_ZONE == "Forereef" & DEPTH > 10 & TAXONNAME!= "Manta birostris"), FUN = function(x){c(Mean = mean(x), SE = st.err(x))}))

ggplot(subset(MeanSETowBiomass, REGION == "PRIAs"), aes(x = OBS_YEAR, y = BIOGM2.Mean)) +
  geom_bar(stat = "identity", position = "stack", aes(fill = TROPHIC_MONREP), size = .5) +
  #geom_errorbar(aes(ymax = BIOGM2.Mean + BIOGM2.SE, ymin=BIOGM2.Mean - BIOGM2.SE), width = 0, size = .3) + 
  facet_grid(~ISLAND) +
  theme_bw() +
  theme(axis.title.x = element_blank()) + 
  ylab(expression(paste("Fish biomass (g ", m^-2,")"))) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(legend.position = "bottom") +
  theme(legend.title=element_blank()) +
  theme(axis.text.x=element_text(angle=50, hjust=1))+
  scale_y_continuous(expand = c(0,0), limits = c(0,20)) +
  scale_x_continuous(limits = c(2000,2015)) +
  scale_fill_manual(values = cols)

### Mean and SE across years 
yrMeanTowBiomass <- do.call(data.frame, aggregate(TotFish~REGION+ISLAND, data = TMP.FishTow_Mean_island_year_BIO, FUN = function(x){c(Mean = mean(x), SE = st.err(x))}))

cols<- c("MARIAN"="#1B9E77","MHI"="#D95F02", "NWHI"="#7570B3", "PRIAs"="chocolate2", "SAMOA"="#E7298A")
cols <- brewer.pal(5,"Dark2") #play around with

### All islands in the Pacific by large fish biomass, filled in by region 
ggplot(yrMeanTowBiomass, aes(x = reorder(ISLAND, -TotFish.Mean), y = TotFish.Mean, fill = REGION)) +
  geom_bar(stat = "identity", size = .3) +
  geom_errorbar(aes(ymax = TotFish.Mean + TotFish.SE, ymin=TotFish.Mean - TotFish.SE), width = 0, size = .3) + 
  theme_bw() + 
  theme(axis.title.x = element_blank()) + 
  xlab("Year") + 
  ylab(expression(paste("Fish biomass (g ", m^-2,")"))) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(axis.text.x=element_text(angle=50, hjust=1))+
  theme(legend.position = "bottom") +
  theme(legend.title=element_blank()) +
  scale_y_continuous(expand = c(0,0), limits = c(0,100)) +
  scale_fill_manual(values = cols)

ggplot(subset(yrMeanTowBiomass, REGION == "PRIAs"), aes(x = reorder(ISLAND, -TotFish.Mean), y = TotFish.Mean)) +
  geom_bar(stat = "identity", size = .3) +
  #geom_errorbar(aes(ymax = TotFish.Mean + TotFish.SE, ymin=TotFish.Mean - TotFish.SE), width = 0, size = .3) +
  theme_bw() + 
  theme(axis.title.x = element_blank()) + 
  xlab("Year") + 
  ylab(expression(paste("Fish biomass (g ", m^-2,")"))) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(axis.text.x=element_text(angle=50, hjust=1))+
  scale_y_continuous(expand = c(0,0), limits = c(0,100)) +
  scale_fill_manual(values = cols)


### NOT SURE OF THIS YET - Each year ~ time series, large fish biomass 
ggplot(subset(FishTowMeanSE, REGION == "PRIAs"), aes(x =YEAR, y = TotFish)) +
  geom_bar(stat = "identity", size = .5) +
  geom_errorbar(aes(ymax = TotFish + SE, ymin=TotFish - SE), width = 0, size = .3) + 
  facet_grid(~ISLAND) +
  theme_bw() +
  theme(axis.title.x = element_blank()) + 
  ylab(expression(paste("Fish biomass (g ", m^-2,")"))) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(axis.text.x=element_text(angle=50, hjust=1))+
  scale_y_continuous(expand = c(0,0), limits = c(0,100)) +
  scale_fill_manual(values = cols)



