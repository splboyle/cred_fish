## plotting in a loop


for (i in unique(wsd_1$ISLAND)){ d <- subset(wsd_1, ISLAND == i & REEF_ZONE == "Forereef" & meanWV > 0) wave <- ggplot(d, aes(meanWV, TotFish)) +  geom_point() +  geom_smooth(method="lm", formula=y~x) +  labs(title = paste("Mean Wave Energy (2002-2012) and \n Total Fish Biomass at", i), x = "Mean Wave Energy", y = "Total Fish Biomass (g/m-2)") +  ggsave(wave,filename=paste("wave_tot",i,".png",sep="_")) }
# It would be nice to either add a facet to visualize them all at once (which I also can't entirely figure out) or simply save them as I'm trying to do here. However, this for some reason isn't working and I'm not sure where I've gone wrong. I get this error:
# Saving 8.94 x 6.78 in image Error in grid.draw(plot) : object 'wave' not found
# and don't totally understand why. Any tips?
# Thanks for the notes you just sent as well!
