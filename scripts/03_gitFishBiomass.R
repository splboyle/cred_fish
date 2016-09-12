# nSPC Fish Biomass Plots
# SB
# July 13, 2016

setwd("/Users/ShanBam/GitHub/cred_fish")
load("data/TMPwsd.Rdata")
load("data/tmp_RAMP_BASICdata_pooled_island.rdata")
load("data/tmp_RAMP_BASICdata_pooled_is_yr_RZ.rdata")
consgrp_stack <- read.csv("data/clean_data/clean_consgrp_stack.csv")
islandmean <- read.csv("data/working_data/islandmean.csv")
parrot_sum <- read.csv("data/working_data/dp_parrotfishSUM.csv")

library(ggplot2)
library(plyr)

# Standard error function used to calculate (not shown here)
st.err <- function(x, na.rm=FALSE) {
  if(na.rm==TRUE) x <- na.omit(x)
  sd(x)/sqrt(length(x))
}

dp <- as.data.frame(dp)

if (!require("RColorBrewer")) {
  install.packages("RColorBrewer")
  library(RColorBrewer)
}
display.brewer.all()
cols <- brewer.pal(6,"RdYlGn")
colorRampPalette(brewer.pal(6,"YlGn"))(6)

# All fish across all pacific islands 
ggplot(dpall, aes(x = reorder(Mean.ISLAND, -Mean.TotFish), y = Mean.TotFish, fill = Mean.REGION)) +
  geom_bar(stat = "identity", size = 0) +
  geom_errorbar(aes(ymax = Mean.TotFish + PooledSE.TotFish, ymin=Mean.TotFish - PooledSE.TotFish), width = 0, size = .3) + 
  theme_bw() + 
  theme(axis.title.x = element_blank()) + 
  #ggtitle("Total Fish Biomass Across All Pacific Islands") +
  xlab("Year") + 
  ylab(expression(paste("Fish biomass (g ", m^-2,")"))) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(axis.text.x=element_text(angle=90, hjust=1))+
  theme(legend.position = "top") +
  theme(legend.title=element_blank()) +
  scale_fill_manual(values=c( "#0868ac", "#bae4bc" , "#7bccc4",  "maroon", "#43a2ca"))
ggsave(file = "graphs_tables/clean_data_plots/TotFishSPC_AllIslands_skinny_toplegend.png")



############# SIZE CLASSES ##############

levels(dpall$Mean.REGION)[levels(dpall$Mean.REGION)=="Marianas"] <- "Mariana Archipelago"
levels(dpall$Mean.REGION)[levels(dpall$Mean.REGION)=="MHI"] <- "Main Hawaiian Islands"
levels(dpall$Mean.REGION)[levels(dpall$Mean.REGION)=="NWHI"] <- "Northwestern Hawaiian Islands"
levels(dpall$Mean.REGION)[levels(dpall$Mean.REGION)=="PRIAs"] <- "Pacific Remote Islands"
levels(dpall$Mean.REGION)[levels(dpall$Mean.REGION)=="America Samoa"] <- "American Samoa"


levels(dpall$Mean.ISLAND)[levels(dpall$Mean.ISLAND)=="Farallon de Pajaros"] <- "FDP"
levels(dpall$Mean.ISLAND)[levels(dpall$Mean.ISLAND)=="French Frigate"] <- "FFS"
levels(dpall$Mean.ISLAND)[levels(dpall$Mean.ISLAND)=="Pearl & Hermes"] <- "P & H"
levels(dpall$Mean.ISLAND)[levels(dpall$Mean.ISLAND)=="Ofu & Olosega"] <- "OFU"

mytheme <- theme(
  axis.title.x = element_blank(),
  axis.text.x = element_blank(),
  axis.ticks.x = element_blank()
)

p1 <- ggplot(dpall, aes(x = Mean.ISLAND, y = `Mean.0_20`, fill = Mean.REGION)) +
  geom_bar(aes(fill = Mean.REGION), position = "dodge", stat = "identity",size = 0) +
  geom_errorbar(aes(ymax = `Mean.0_20` + `PooledSE.0_20`, ymin=`Mean.0_20` - `PooledSE.0_20`), width = 0, size = .3) + 
  facet_grid(.~Mean.REGION, scales='free_x')+
  theme_bw() + 
  theme(axis.title.y=element_blank()) +
  #theme(axis.title.x = element_blank()) + 
  #ggtitle("Total Fish Biomass Across All Pacific Islands") +
  #ylab(expression(paste("Fish biomass (g ", m^-2,")"))) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  #theme(axis.text.x=element_text(angle=90, hjust=1))+
  theme(legend.position = "none") +
  theme(legend.title=element_blank()) +
  scale_fill_manual(values=c( "#0868ac", "#bae4bc" , "#7bccc4",  "maroon", "#43a2ca")) +
  mytheme

p2 <- ggplot(dpall, aes(x = Mean.ISLAND, y = `Mean.20_50`, fill = Mean.REGION)) +
  geom_bar(stat = "identity", size = 0) +
  geom_errorbar(aes(ymax = `Mean.20_50` + `PooledSE.20_50`, ymin=`Mean.20_50` - `PooledSE.20_50`), width = 0, size = .3) + 
  facet_grid(.~Mean.REGION, scales='free_x')+
  theme_bw() + 
  #theme(axis.title.x = element_blank()) + 
  #ggtitle("Total Fish Biomass Across All Pacific Islands") +
  ylab(expression(paste("Fish biomass (g ", m^-2,")"))) + 
  theme(strip.background = element_blank(), strip.text.x = element_blank()) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  #theme(axis.text.x=element_text(angle=90, hjust=1))+
  theme(legend.position = "none") +
  #theme(legend.title=element_blank()) +
  scale_fill_manual(values=c( "#0868ac", "#bae4bc" , "#7bccc4",  "maroon", "#43a2ca")) +
  mytheme

p3 <- ggplot(dpall, aes(x = Mean.ISLAND, y = `Mean.50_plus`, fill = Mean.REGION)) +
  geom_bar(stat = "identity", size = 0) +
  geom_errorbar(aes(ymax = `Mean.50_plus` + `PooledSE.50_plus`, ymin=`Mean.50_plus` - `PooledSE.50_plus`), width = 0, size = .3) + 
  facet_grid(.~Mean.REGION, scales='free_x')+
  theme_bw() + 
  theme(axis.title.x = element_blank()) + 
  #ggtitle("Total Fish Biomass Across All Pacific Islands") +
  #ylab(expression(paste("Fish biomass (g ", m^-2,")"))) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(axis.text.x=element_text(angle=90, hjust=1))+
  theme(strip.background = element_blank(), strip.text.x = element_blank()) +
  theme(legend.position = "none") +
  theme(axis.title.y=element_blank()) +
  #theme(legend.title=element_blank()) +
  #scale_x_discrete(limits=c("2","1","0.5")) +
  #scale_y_continuous(expand = c(0.02,0)) +
  #theme(axis.title.x = element_text(vjust=-5) , plot.margin = (unit(c(.5, .5, 2, .5), "cm"))) + 
  scale_fill_manual(values=c( "#0868ac", "#bae4bc" , "#7bccc4",  "maroon", "#43a2ca"))

g1 <- ggplotGrob(p1)
g2 <- ggplotGrob(p2)
g3 <- ggplotGrob(p3)

g1$widths <- g2$widths
g3$widths <- g2$widths

pall <- grid.arrange(g1, g2, g3, nrow=3, as.table = T)

ggsave(pall, file = "graphs_tables/clean_data_plots/SPC_size_classes_all_islands.png")



###############################################
### Total fish biomass for each individual island (TS bargraph)
# Individual island, total fish biomass, across years 
for (i in unique(tot$island)){
  d <- subset(tot, island == i)
  p <- ggplot(d, aes(year, biomass)) + 
    geom_bar(stat = "identity", col = "black", fill = "springgreen4", size = .25, width = .3) + 
    geom_errorbar(aes(ymax = biomass + SE, ymin=biomass - SE), width = 0, size = .25) + 
    theme_bw() + 
    theme(axis.title.x = element_blank()) + 
    ylab(expression(paste("Fish biomass (g ", m^-2,")"))) + 
    ggtitle(paste("ALL FISH\n", i)) +
    #scale_y_continuous(expand = c(0,0)) +
    scale_y_continuous(limits = c(0,500)) +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
  print(p)
  #ggsave(p,filename=paste("AllFish",i,".png",sep=""), path = "graphs_tables")
  }

###############################################
### Total fish biomass across all years faceted by island (TS bargraph)
ggplot(data = subset(dp, Mean.ANALYSIS_YEAR != 2008), aes(x = Mean.ANALYSIS_YEAR, y = Mean.TotFish, fill = Mean.ISLAND)) +
  geom_bar(stat = "identity", size = 0) +
  geom_errorbar(aes(ymax = Mean.TotFish + PooledSE.TotFish, ymin=Mean.TotFish - PooledSE.TotFish), width = 0, size = .3) + 
  facet_grid(~Mean.ISLAND) +
  theme_bw() + 
  theme(axis.title.x = element_blank()) + 
  ylab(expression(paste("Fish biomass (g ", m^-2,")"))) + 
  ggtitle("Total Fish Biomass Across the PRIMNM (2009-2015)") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(axis.text.x=element_text(angle=50, hjust=1)) +
  theme(legend.position = "none") +
  theme(legend.title=element_blank())
ggsave(file = "graphs_tables/AllFishSPC_IslandFacet.png") 

###############################################
### Each island, total fish biomass faceted by consumer group, across all years (TS bargraph)
for (i in unique(all.groups$island)) {
  c <- subset(all.groups, island == i)
  c.p <- ggplot(c, aes(x = year, y = biomass)) +
    geom_bar(stat = "identity", col = "black", aes(fill = group), size = .3) +
    facet_grid(~group) +
    geom_errorbar(aes(ymax = biomass + SE, ymin=biomass - SE), width = 0, size = .3) + 
    theme_bw() + 
    theme(axis.title.x = element_blank()) +
    theme(legend.position="none") +
    xlab("Year") + 
    ylab(expression(paste("Fish biomass (g ", m^-2,")"))) + 
    ggtitle(paste("FISH BIOMASS BY CONSUMER GROUP\n", i)) + 
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
    theme(axis.text.x=element_text(angle=45, hjust=1)) +
    scale_y_continuous(limits = c(0,150))
  print(c.p)
  #ggsave(c.p, filename=paste("ConsumerGroup",i,".png",sep=""), path = "graphs_tables")
}

###############################################
### Total fish biomass stacked by consumer group, faceted by island across all years (TS bargraph)
##################### ##################### 
# Need to change the order of legend items 
##################### ##################### 
selimits <- aes(ymax = All_TotFish + All_TotFishSE, ymin= All_TotFish - All_TotFishSE)

ggplot(data = subset(consgrp_stack, year != 2008), aes(x = year, y = TotFish, fill = group)) +
  geom_bar(stat = "identity", size = 0) +
  geom_errorbar(selimits, width = 0, size = .3) +
  facet_grid(.~island) +
  theme_bw() + 
  theme(axis.title.x = element_blank()) +
  ylab(expression(paste("Fish biomass (g ", m^-2,")"))) + 
  scale_x_continuous(breaks=c(2008,2010,2012,2014,2016))+
  ggtitle("Consumer Group Biomass Across the PRIMNM (2010-2016)") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(axis.text.x=element_text(angle=90, hjust=1)) +
  theme(legend.position = "bottom") +
  scale_fill_manual(values=c("royalblue", "coral2", "springgreen3",  "darkgoldenrod1")) +
  theme(legend.title=element_blank()) 



ggsave("graphs_tables/clean_data_plots/StackedConsumerGroup_IslandFacet_lowres.png")
ggsave("graphs_tables/clean_data_plots/StackedConsumerGroup_IslandFacet_1200dpi.png", dpi = 1200)
ggsave("graphs_tables/clean_data_plots/StackedConsumerGroup_IslandFacet_300dpi.png", dpi = 300) # must be last plot 



### Total fish biomass (mean of all years combined) for each island stacked by consumer group
limits <- aes(ymax = allF + allFSE, ymin= allF - allFSE)

ggplot(islandmean, aes(x = island, y = biomass, fill = group)) +
  geom_bar(stat = "identity", col = "black", size = .3) +
  geom_errorbar(limits, width = 0, size = 0.25) +
  theme_bw() + 
  theme(axis.title.x = element_blank()) +
  ylab(expression(paste("Fish biomass (g ", m^-2,")"))) + 
  ggtitle("Consumer Group Biomass Across the PRIMNM") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  #theme(axis.text.x=element_text(angle=45, hjust=1)) +
  theme(legend.position = "bottom") +
  theme(legend.title=element_blank())

ggsave("graphs_tables/StackedConsumerGroup_IslandMean.png", dpi = 1200)
ggsave("graphs_tables/StackedConsumerGroup_IslandMean_lowres.png")

#ggplot(tot.allisland, aes(x = island, y = biomass)) +
  geom_bar(stat = "identity", col = "black", fill = "darkred", size = .3) +
  geom_errorbar(aes(ymax=biomass+SE, ymin = biomass-SE), width = 0, size = .3) +
  theme_bw() + 
  theme(axis.title.x = element_blank()) +
  ylab(expression(paste("Fish biomass (g ", m^-2,")"))) + 
  ggtitle("ALL FISH") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
#ggsave("graphs_tables/NoConsGroup_IslandMean.png")


###############################################
### Parrotfish biomass across all years, faceted by island (TS bargraph)
ggplot(subset(parrot_sum, Mean.REGION == "PRIAs" & Mean.REEF_ZONE == "Forereef"), aes(x=Mean.ANALYSIS_YEAR, y=Parrotfish, fill = Mean.ISLAND)) +
  geom_bar(stat = "identity", col = "black", size = .3) +
  facet_grid(~Mean.ISLAND)+
  geom_errorbar(aes(ymax = Parrotfish+ParrotfishSE, ymin=Parrotfish-ParrotfishSE), width = 0, size = .25) +
  theme_bw() + 
  theme(axis.title.x = element_blank()) +
  ylab(expression(paste("Fish biomass (g ", m^-2,")"))) + 
  ggtitle("PARROTFISH") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(axis.text.x=element_text(angle=45, hjust=1)) +
  theme(legend.position = "none") +
  theme(legend.title=element_blank()) 
ggsave("graphs_tables/Parrot_IslandFacet.png") 

### Parrotfish biomass, comparative across islands (mean of all years combined)
parrot.allyr <- do.call(data.frame, aggregate(Parrotfish~ISLAND, data = subset(wsd, REEF_ZONE == "Forereef"), FUN = function(x){c(Mean = mean(x), SE = st.err(x))}))
colnames(parrot.allyr) <- c("island", "biomass", "SE")

ggplot(subset(parrot_sum, Mean.ISLAND == "WAKE" & Mean.REEF_ZONE == "Forereef"), aes(x=Mean.ANALYSIS_YEAR, y=Parrotfish)) +
  geom_bar(stat = "identity", col = "black", size = .3) +
  geom_errorbar(aes(ymax = Parrotfish+ParrotfishSE, ymin=Parrotfish-ParrotfishSE), width = 0, size = .25) +
  theme_bw() + 
  theme(axis.title.x = element_blank()) +
  ylab(expression(paste("Fish biomass (g ", m^-2,")"))) + 
  ggtitle("PARROTFISH") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(axis.text.x=element_text(angle=45, hjust=1)) +
  theme(legend.position = "none") +
  theme(legend.title=element_blank()) 
ggsave("graphs_tables/ParrotAllYears_IslandFacet.png") 

###############################################
### Island mean archipelagic comparisons 













