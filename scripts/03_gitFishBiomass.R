# Fish Biomass Plots
# SB
# July 13, 2016

setwd("/Users/ShanBam/GitHub/fish_cred")
prim_fish <- read.csv("data/PRIA_CONSUMER_FAMILY.csv")

# Mean and SE of all biomass by island, year, and method 
m_tot <- read.csv("data/working_data/m_tot.csv")
m_plank <- read.csv("data/working_data/m_plank.csv")
m_pisc <- read.csv("data/working_data/m_pisc.csv")
m_prim <- read.csv("data/working_data/m_prim.csv")
m_sec <- read.csv("data/working_data/m_sec.csv")

library(ggplot2)
library(plyr)

# Standard error function used to calculate (not shown here)
st.err <- function(x, na.rm=FALSE) {
  if(na.rm==TRUE) x <- na.omit(x)
  sd(x)/sqrt(length(x))
}

###############################################
# Could I loop this somehow?

m_tot$X <- NULL
m_plank$X <- NULL
m_prim$X <- NULL
m_sec$X <- NULL
m_pisc$X <- NULL

colnames(m_tot) <- c("year", "method", "island", "mean", "se")
colnames(m_plank) <- c("year", "method", "island", "mean", "se")
colnames(m_prim) <- c("year", "method", "island", "mean", "se")
colnames(m_sec) <- c("year", "method", "island", "mean", "se")
colnames(m_pisc) <- c("year", "method", "island", "mean", "se")

m_plank$group <- c("Planktivores")
m_prim$group <- c("Prim. cons.")
m_sec$group <- c("Sec. cons.")
m_pisc$group <- c("Piscivores")

# Working data frame 
m_all.groups <- rbind(m_prim, m_sec, m_plank, m_pisc)

###############################################
# Each island, total fish biomass, across years 

for (i in unique(mp_all$island)){
  d <- subset(mp_all, island == i & method == "nSPC")
  p <- ggplot(d, aes(year, mean)) + 
    geom_bar(stat = "identity", col = "black", fill = "springgreen4", size = .25) + 
    geom_errorbar(aes(ymax = mean + se, ymin=mean - se), width = 0.25, size = .25) + 
    theme_bw() + 
    xlab("Year") + 
    ylab("Fish biomass (g/m2)") + 
    ggtitle(paste("ALL FISH\n", i)) +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
  ggsave(p,filename=paste("AllFish",i,".png",sep=""), path = "graphs_tables", width = 3, height = 4)
}

# Total fish biomass across years faceted by island 
p.facet <- ggplot(subset(mp_all, method == "nSPC"), aes(x = year, y = mean)) +
  geom_bar(stat = "identity", col = "black", fill = "springgreen4", size = .3) +
  geom_errorbar(aes(ymax = mean + se, ymin=mean - se), width = 0.25, size = .3) + 
  facet_grid(~island) +
  theme_bw() + 
  xlab("Year") + 
  ylab("Fish biomass (g/m2)") + 
  ggtitle("ALL FISH") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(axis.text.x=element_text(angle=45, hjust=1))
# ggsave(p.facet, "alllfish_facet.png", device = "png", path = "graphs_tables", width = 5, height = 3) # doesn't work for some reason, saved manually

###############################################
# Each island, biomass faceted by consumer group, across years

for (i in unique(m_all.groups$island)) {
  c <- subset(m_all.groups, island == i & method == "nSPC")
  c.p <- ggplot(c, aes(x = year, y = mean)) +
    geom_bar(stat = "identity", col = "black", fill = "darkred", size = .3) +
    facet_grid(~group) +
    geom_errorbar(aes(ymax = mean + se, ymin=mean - se), width = 0.25, size = .3) + 
    theme_bw() + 
    xlab("Year") + 
    ylab("Fish biomass (g/m-2)") + 
    ggtitle(paste("FISH BIOMASS BY CONSUMER GROUP\n", i)) + 
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
    theme(axis.text.x=element_text(angle=45, hjust=1))
  print(c.p)
  ggsave(c.p, filename=paste("ConsumerGroup",i,".png",sep=""), path = "graphs_tables", width = 5, height = 3)
}





