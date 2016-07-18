# nSPC Fish Biomass Plots
# SB
# July 13, 2016

setwd("/Users/ShanBam/GitHub/cred_fish")
load("data/TMPwsd.Rdata")

library(ggplot2)
library(plyr)

# Standard error function used to calculate (not shown here)
st.err <- function(x, na.rm=FALSE) {
  if(na.rm==TRUE) x <- na.omit(x)
  sd(x)/sqrt(length(x))
}

wsd$NoMABI <- (wsd$PLANKTIVORE - wsd$MABI)

# Time series means 
tot <- do.call(data.frame, aggregate(TotFish~OBS_YEAR+ISLAND, data = subset(wsd, REEF_ZONE == "Forereef"), FUN = function(x){c(Mean = mean(x), SE = st.err(x))}))
colnames(tot) <- c("year", "island", "biomass", "SE")

tot.allisland <- do.call(data.frame, aggregate(TotFish~ISLAND, data = subset(wsd, REEF_ZONE == "Forereef"), FUN = function(x){c(Mean = mean(x), SE = st.err(x))}))
colnames(tot.allisland) <- c("island", "biomass", "SE")

pisc <- do.call(data.frame, aggregate(PISCIVORE~OBS_YEAR+ISLAND, data = subset(wsd, REEF_ZONE == "Forereef"), FUN = function(x){c(Mean = mean(x), SE = st.err(x))}))
colnames(pisc) <- c("year", "island", "biomass", "SE")

plank <- do.call(data.frame, aggregate(NoMABI~OBS_YEAR+ISLAND, data = subset(wsd, REEF_ZONE == "Forereef"), FUN = function(x){c(Mean = mean(x), SE = st.err(x))}))
colnames(plank) <- c("year", "island", "biomass", "SE")

prim <- do.call(data.frame, aggregate(PRIMARY~OBS_YEAR+ISLAND, data = subset(wsd, REEF_ZONE == "Forereef"), FUN = function(x){c(Mean = mean(x), SE = st.err(x))}))
colnames(prim) <- c("year", "island", "biomass", "SE")

sec <- do.call(data.frame, aggregate(SECONDARY~OBS_YEAR+ISLAND, data = subset(wsd, REEF_ZONE == "Forereef"), FUN = function(x){c(Mean = mean(x), SE = st.err(x))}))
colnames(sec) <- c("year", "island", "biomass", "SE")

pisc$group <- c("Piscivores")
plank$group <- c("Planktivores")
prim$group <- c("Prim. cons.")
sec$group <- c("Sec. cons.")

all.groups <- rbind(prim, sec, plank, pisc)

# All years combined means 
pisc.allyr <- do.call(data.frame, aggregate(PISCIVORE~ISLAND, data = subset(wsd, REEF_ZONE == "Forereef"), FUN = function(x){c(Mean = mean(x), SE = st.err(x))}))
colnames(pisc.allyr) <- c("island", "biomass", "SE")

plank.allyr <- do.call(data.frame, aggregate(NoMABI~ISLAND, data = subset(wsd, REEF_ZONE == "Forereef"), FUN = function(x){c(Mean = mean(x), SE = st.err(x))}))
colnames(plank.allyr) <- c("island", "biomass", "SE")

prim.allyr <- do.call(data.frame, aggregate(PRIMARY~ISLAND, data = subset(wsd, REEF_ZONE == "Forereef"), FUN = function(x){c(Mean = mean(x), SE = st.err(x))}))
colnames(prim.allyr) <- c("island", "biomass", "SE")

sec.allyr <- do.call(data.frame, aggregate(SECONDARY~ISLAND, data = subset(wsd, REEF_ZONE == "Forereef"), FUN = function(x){c(Mean = mean(x), SE = st.err(x))}))
colnames(sec.allyr) <- c("island", "biomass", "SE")

pisc.allyr$group <- c("Piscivores")
plank.allyr$group <- c("Planktivores")
prim.allyr$group <- c("Prim. cons.")
sec.allyr$group <- c("Sec. cons.")

tot.allyr <- rbind(pisc.allyr, plank.allyr, prim.allyr, sec.allyr)

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
p.facet <- ggplot(tot, aes(x = year, y = biomass, fill = island)) +
  geom_bar(stat = "identity", col = "black", size = .25) +
  geom_errorbar(aes(ymax = biomass + SE, ymin=biomass - SE), width = 0, size = .25) + 
  facet_grid(~island) +
  theme_bw() + 
  theme(axis.title.x = element_blank()) + 
  ylab(expression(paste("Fish biomass (g ", m^-2,")"))) + 
  ggtitle("ALL FISH") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(axis.text.x=element_text(angle=45, hjust=1)) +
  theme(legend.position = "none") +
  theme(legend.title=element_blank())
ggsave(p.facet, "graphs_tables/AllFish_IslandFacet", device = "png") # doesn't work for some reason, saved manually

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
# Need errorbars
limits <- aes(ymax = tot$biomass + tot$SE, ymin = tot$biomass - tot$SE)

ggplot(all.groups, aes(x = year, y = biomass)) +
  geom_bar(stat = "identity", position = "stack", col = "black", aes(fill = group), size = .3) +
  #geom_errorbar(limits, width = 0.25) +
  facet_grid(~island) +
  theme_bw() + 
  theme(axis.title.x = element_blank()) +
  ylab(expression(paste("Fish biomass (g ", m^-2,")"))) + 
  ggtitle("ALL FISH") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(axis.text.x=element_text(angle=45, hjust=1)) +
  theme(legend.position = "bottom") +
  theme(legend.title=element_blank()) 
  #scale_y_continuous(expand = c(0,0))
ggsave("graphs_tables/StackedConsumerGroup_IslandFacet.png") # must be last plot 

### How do I add the error bar for the total mean? Its in a different dataframe...
### How do I change the stack order of the groups - i.e., I want Planktivores or Piscivores (or Herbivores) on the bottom

### Total fish biomass (mean of all years combined) for each island stacked by consumer group
ggplot(tot.allyr, aes(x = island, y = biomass)) +
  geom_bar(stat = "identity", position = "stack", col = "black", aes(fill = group), size = .3) +
  #geom_errorbar(limits, width = 0.25) +
  theme_bw() + 
  theme(axis.title.x = element_blank()) +
  ylab(expression(paste("Fish biomass (g ", m^-2,")"))) + 
  ggtitle("ALL FISH") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  #theme(axis.text.x=element_text(angle=45, hjust=1)) +
  theme(legend.position = "bottom") +
  theme(legend.title=element_blank())
ggsave("graphs_tables/StackedConsumerGroup_IslandMean.png")

ggplot(tot.allisland, aes(x = island, y = biomass)) +
  geom_bar(stat = "identity", col = "black", fill = "darkred", size = .3) +
  geom_errorbar(aes(ymax=biomass+SE, ymin = biomass-SE), width = 0, size = .3) +
  theme_bw() + 
  theme(axis.title.x = element_blank()) +
  ylab(expression(paste("Fish biomass (g ", m^-2,")"))) + 
  ggtitle("ALL FISH") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
ggsave("graphs_tables/NoConsGroup_IslandMean.png")


###############################################
### Parrotfish biomass across all years, faceted by island (TS bargraph)
parrot <- do.call(data.frame, aggregate(Parrotfish~OBS_YEAR+ISLAND, data = subset(wsd, REEF_ZONE == "Forereef"), FUN = function(x){c(Mean = mean(x), SE = st.err(x))}))
colnames(parrot) <- c("year", "island", "biomass", "SE")

ggplot(parrot, aes(x=year, y=biomass, fill = island)) +
  geom_bar(stat = "identity", col = "black", size = .3) +
  facet_grid(~island)+
  geom_errorbar(aes(ymax = biomass+SE, ymin=biomass-SE), width = 0, size = .25) +
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

ggplot(parrot.allyr, aes(x=island, y=biomass)) +
  geom_bar(stat = "identity", col = "black", size = .3, aes(fill = island)) +
  geom_errorbar(aes(ymax = biomass+SE, ymin=biomass-SE), width = 0, size = .25) +
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













