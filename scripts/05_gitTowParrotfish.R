# Parrotfish Biomass Plots
# SB
# July 18, 2016

setwd("/Users/ShanBam/GitHub/cred_fish")
TMP.FishTow_Mean_family_tow_BIO <- read.csv("data/TMP FishTow_Mean_family_tow_level_BIO.csv")
load("data/TMPspecies.Rdata")

library(ggplot2)

st.err <- function(x, na.rm=FALSE) {
  if(na.rm==TRUE) x <- na.omit(x)
  sd(x)/sqrt(length(x))
}

# Subset PRIA and Scaridae
PRIA.tow <- subset(TMP.FishTow_Mean_family_tow_BIO, REGION.y == "PRIAs")
PRIA.tow.parrot <- data.frame(PRIA.tow$X, PRIA.tow$DIVEID, PRIA.tow$ISLAND.x, PRIA.tow$YEAR, PRIA.tow$STRATA, PRIA.tow$Scaridae)
colnames(PRIA.tow.parrot) <- c("X", "DIVEID", "ISLAND", "YEAR", "STRATA", "Scaridae")

parrot.tow.mean <- do.call(data.frame, aggregate(Scaridae~ISLAND+YEAR, data = subset(PRIA.tow.parrot, STRATA == "Forereef"), FUN = function(x){c(Mean = mean(x), SE = st.err(x))}))
colnames(parrot.tow.mean) <- c("island", "year", "biomass", "SE")

### Parrotfish biomass across years, faceted by island
parrot <- ggplot(subset(parrot.tow.mean, year > 2001), aes(year, biomass, fill = island))+
  geom_bar(stat = "identity", col = "black", size = .25) +
  geom_errorbar(aes(ymax = biomass+SE, ymin=biomass-SE), width = 0, size = .3)+
  facet_grid(~island) +
  theme_bw() + 
  theme(axis.title.x = element_blank()) +
  ylab(expression(paste("Fish biomass (g ", m^-2,")"))) + 
  ggtitle("Parrotfish Biomass Across the PRIMNM (2002-2015)") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(axis.text.x=element_text(angle=50, hjust=1))+
  theme(legend.position = "none")
ggsave("graphs_tables/tow/ParrotfishTow_TS_IslandFacet_lowres.png")

ggplot(subset(parrot.tow.mean, year > 2001 & island == "Wake"), aes(year, biomass))+
  geom_bar(stat = "identity", col = "black", fill ="#FB61D7", size = 0.25) +
  geom_errorbar(aes(ymax = biomass+SE, ymin=biomass-SE), width = 0, size = .3)+
  theme_bw() + 
  theme(axis.title.x = element_blank()) +
  ylab(expression(paste("Fish biomass (g ", m^-2,")"))) + 
  ggtitle("Parrotfish Biomass at Wake (2002-2015)") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(axis.text.x=element_text(angle=50, hjust=1))+
  theme(legend.position = "none")
ggsave("graphs_tables/tow/ParrotfishTow_TS_Wake_lowres.png")

wake.lm.parrot <- lm(year ~ biomass, data = subset(parrot.tow.mean, year > 2010 & island == "Wake"))
wake.lm.parrot <- subset(parrot.tow.mean, year > 2010 & island == "Wake")

ggplot_build(parrot)$data


### Parrotfish biomass for Wake across the years (with trendline...)
ggplot(subset(parrot.tow.mean, island == "Wake"), aes(year, biomass))+
  geom_bar(stat = "identity", col = "black", fill = "turquoise4") +
  geom_errorbar(aes(ymax = biomass+SE, ymin=biomass-SE), width = 0, size = .3)+
  geom_smooth(method = "lm") +
  theme_bw() + 
  theme(axis.title.x = element_blank()) +
  ylab(expression(paste("Fish biomass (g ", m^-2,")"))) + 
  ggtitle("PARROTFISH \n Wake") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(axis.text.x=element_text(angle=45, hjust=1))

### Parrotfish biomass for Palmyra across the years (with trendline...)
ggplot(subset(parrot.tow.mean, island == "Palmyra"), aes(year, biomass))+
  geom_bar(stat = "identity", col = "black", fill = "turquoise4") +
  geom_errorbar(aes(ymax = biomass+SE, ymin=biomass-SE), width = 0, size = .3)+
  geom_smooth(method = "lm") +
  theme_bw() + 
  theme(axis.title.x = element_blank()) +
  ylab(expression(paste("Fish biomass (g ", m^-2,")"))) + 
  ggtitle("PARROTFISH \n Palmyra") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(axis.text.x=element_text(angle=45, hjust=1))

### Parrotfish biomass for each individual island across the years (no trendline...)
for (i in unique(parrot.tow.mean$island)){
  d.p <- subset(parrot.tow.mean, island == i)
  pp <- ggplot(d.p, aes(year, biomass)) + 
    geom_bar(stat = "identity", col = "black", fill = "turquoise4", size = .25, width = .3) + 
    geom_errorbar(aes(ymax = biomass + SE, ymin=biomass - SE), width = 0, size = .25) + 
    #geom_smooth(method = "lm") +
    theme_bw() + 
    theme(axis.title.x = element_blank()) + 
    ylab(expression(paste("Fish biomass (g ", m^-2,")"))) + 
    ggtitle("PARROTFISH") +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
    scale_x_continuous(breaks = c(2001,2003,2005,2007,2009,2011,2013,2015))
  print(pp)
  ggsave(pp,filename=paste("LargeParrotfishlm",i,".png",sep=""), path = "graphs_tables/tow")
}