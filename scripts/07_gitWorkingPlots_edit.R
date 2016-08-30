dp <- as.data.frame(dp)
write.csv(dp, "data/clean_data/dp.csv")

### Total fish biomass stacked by consumer group, faceted by island across all years (TS bargraph)
# Need errorbars
selimits <- aes(ymax = All_TotFish + All_TotFishSE, ymin= All_TotFish - All_TotFishSE)

ggplot(consgrp_stack, aes(x = year, y = TotFish, fill = group)) +
  geom_bar(stat = "identity", col = "black", size = .3) +
  geom_errorbar(selimits, width = 0, size = .25) +
  facet_grid(.~island) +
  theme_bw() + 
  theme(axis.title.x = element_blank()) +
  ylab(expression(paste("Fish biomass (g ", m^-2,")"))) + 
  ggtitle("Consumer Group Biomass Across the PRIMNM (2010-2015)") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(axis.text.x=element_text(angle=45, hjust=1)) +
  theme(legend.position = "bottom") +
  theme(legend.title=element_blank()) 

ggsave("graphs_tables/StackedConsumerGroup_IslandFacet_1.png")
ggsave("graphs_tables/StackedConsumerGroup_IslandFacet.png", dpi = 1200)


##### TOW #####
selimits <- aes(ymax = All_TotFish + All_TotFishSE, ymin= All_TotFish - All_TotFishSE)


ggplot(data = tow_familygrp_forplot, aes(x = YEAR, y = TotFish, fill = group)) +
  geom_bar(stat = "identity", size = 0) +
  geom_errorbar(selimits, width = 0, size = .3) +
  facet_grid(.~ISLAND) +
  theme_bw() + 
  theme(axis.title.x = element_blank()) +
  ylab(expression(paste("Large-Fish Biomass (g ", m^-2,")"))) + 
  scale_x_continuous(breaks=c(2000,2002,2004,2006,2008,2010,2012,2014))+
  ggtitle("Large Fish Biomass By Family Across the PRIMNM (2001-2015)") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(axis.text.x=element_text(angle=90, hjust=1)) +
  theme(legend.position = "bottom") +
  scale_fill_manual(values=c("#fb6a4a", "#238b45", "#cb181d", "#d94701",  "#6a51a3",  "gray55", "#6baed6", "#74c476", "#2171b5", "#fd8d3c", "#9e9ac8", "#bdd7e7", "#bae4b3", "#fcae91")) + 
  theme(legend.title=element_blank()) 

ggsave("graphs_tables/clean_data_plots/Tow_FamilyStack_byYear_PRIM.png", dpi = 1200)
ggsave("graphs_tables/clean_data_plots/Tow_FamilyStack_byYear_PRIM_lowres.png")  


display.brewer.all()
cols <- brewer.pal(6,"RdYlGn")
colorRampPalette(brewer.pal(6,"Dark2"))(14)
install.packages("colorspace")
rainbow_hcl(13)


for (i in unique(tow_familygrp_forplot$ISLAND)) {
  tt <- subset(tow_familygrp_forplot, ISLAND == i)
  tt.p <- ggplot(tt, aes(x = YEAR, y = TotFish, fill = group)) +
    geom_bar(stat = "identity", size = 0) +
    geom_errorbar(selimits, width = 0, size = .3) +
    theme_bw() + 
    theme(axis.title.x = element_blank()) +
    ylab(expression(paste("Large-Fish Biomass (g ", m^-2,")"))) + 
    scale_x_continuous(breaks=c(2000,2002,2004,2006,2008,2010,2012,2014))+
    ggtitle(paste("Large Fish Biomass By Family at", i, "(2000-2015)")) +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
    theme(axis.text.x=element_text(angle=50, hjust=1)) +
    theme(legend.position = "bottom") +
    scale_fill_manual(values=c("#fb6a4a", "#238b45", "#cb181d", "#d94701",  "#6a51a3",  "gray55", "#6baed6", "#74c476", "#2171b5", "#fd8d3c", "#9e9ac8", "#bdd7e7", "#bae4b3", "#fcae91")) +
    theme(legend.title=element_blank()) 
  print(tt.p)
  ggsave(tt.p,filename=paste("Tow_year_at",i,".png",sep="_"), path = "graphs_tables/clean_data_plots/still_working")
}


ggplot(data = subset(tow_familygrp_forplot, ISLAND == "Baker"), aes(x = YEAR, y = TotFish, fill = group)) +
  geom_bar(stat = "identity", size = 0) +
  geom_errorbar(selimits, width = 0, size = .3) +
  #facet_grid(.~ISLAND) +
  theme_bw() + 
  theme(axis.title.x = element_blank()) +
  ylab(expression(paste("Fish biomass (g ", m^-2,")"))) + 
  scale_x_continuous(breaks=c(2000,2002,2004,2006,2008,2010,2012,2014))+
  ggtitle("Large Fish Biomass By Family at Baker (2000-2015)") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(axis.text.x=element_text(angle=50, hjust=1)) +
  theme(legend.position = "bottom") +
  scale_fill_manual(values=c("#6a3d9a", "#cab2d6", "#ffff99", "#e31a1c",  "#b15928", "gray55", "#33a02c", "#1f78b4", "#ff7f00", "#b2df8a", "#fb6a4a", "#fdbf6f", "#a6cee3")) + 
  theme(legend.title=element_blank()) 



