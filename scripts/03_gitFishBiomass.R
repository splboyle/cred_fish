# Fish Biomass Plots
# SB
# July 13, 2016

setwd("/Users/ShanBam/GitHub/cred_fish")
load("data/TMPwsd.Rdata")
#wsd_rmMABI <- read.csv("data/wsd_rmMABI.csv")
#prim_fish <- read.csv("data/PRIA_CONSUMER_FAMILY.csv")

library(ggplot2)
library(plyr)

# Standard error function used to calculate (not shown here)
st.err <- function(x, na.rm=FALSE) {
  if(na.rm==TRUE) x <- na.omit(x)
  sd(x)/sqrt(length(x))
}

wsd$NoMABI <- (wsd$PLANKTIVORE - wsd$MABI)

tot <- do.call(data.frame, aggregate(TotFish~OBS_YEAR+ISLAND, data = subset(wsd, REEF_ZONE == "Forereef"), FUN = function(x){c(Mean = mean(x), SE = st.err(x))}))
colnames(tot) <- c("year", "island", "biomass", "SE")

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

# Working data frame 
all.groups <- rbind(prim, sec, plank, pisc)

###############################################
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
  ggsave(p,filename=paste("AllFish",i,".png",sep=""), path = "graphs_tables")
  }

# Total fish biomass across years faceted by island 
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
# Each island, biomass faceted by consumer group, across years
for (i in unique(all.groups$island)) {
  c <- subset(all.groups, island == i)
  c.p <- ggplot(c, aes(x = year, y = biomass)) +
    geom_bar(stat = "identity", col = "black", aes(fill = group), size = .3) +
    facet_grid(~group) +
    geom_errorbar(aes(ymax = biomass + SE, ymin=biomass - SE), width = 0.25, size = .3) + 
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
  ggsave(c.p, filename=paste("ConsumerGroup",i,".png",sep=""), path = "graphs_tables")
}

# Each island, total biomass, stacked barplot by consumer group
limits <- aes(ymax = tot$biomass + tot$SE, ymin = tot$biomass - tot$SE)

p.grp <- 
  ggplot(all.groups, aes(x = year, y = biomass)) +
  geom_bar(stat = "identity", position = "stack", col = "black", aes(fill = group), size = .3) +
  geom_errorbar(limits, width = 0.25) +
  facet_grid(~island) +
  theme_bw() + 
  theme(axis.title.x = element_blank()) +
  ylab(expression(paste("Fish biomass (g ", m^-2,")"))) + 
  ggtitle("ALL FISH") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(axis.text.x=element_text(angle=45, hjust=1)) +
  theme(legend.position = "bottom") +
  theme(legend.title=element_blank()) +
  scale_y_continuous(expand = c(0,0), limits = c(0,500))

ggsave(p.grp, file = "graphs_tables/StackedConsumerGroup_AllIslands.png", device = "png")

### How do I add the error bar for the total mean? Its in a different dataframe...
### How do I change the stack order of the groups - i.e., I want Planktivores or Piscivores (or Herbivores) on the bottom

ggplot(subset(m_all.groups, method == "BLT"), aes(x = year, y = mean)) +
  geom_bar(stat = "identity", position = "stack", col = "black", aes(fill = group), size = .3) +
  facet_grid(~island) +
  theme_bw() + 
  theme(axis.title.x = element_blank()) +
  ylab("Fish biomass (g/m2)") + 
  ggtitle("ALL FISH") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(axis.text.x=element_text(angle=45, hjust=1)) +
  theme(legend.position = "bottom") +
  theme(legend.title=element_blank()) +
  scale_y_continuous(expand = c(0,0), limits = c(0,500)) +
  scale_x_continuous(limits = c(2001,2007))

###############################################
# Each island, major large fish groupings 



