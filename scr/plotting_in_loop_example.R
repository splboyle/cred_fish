## plotting in a loop
setwd("~/Documents/GitHub/cred_fish")
library("ggplot2")
load("data/TMPwsd.Rdata")

WSD_SAVED<-wsd

## always get it to work for one before looping and test it by running the code inside the loop after setting i 
## when I tried your subset statement it didn't return any data because your pointer i wasn't quite right
## you'll need to replace X2012 with the variable you created meanWV



## working loop to return one graph per island
for (i in unique(wsd$ISLAND)){
	#i<-1
 	d<-subset(wsd, wsd$ISLAND ==  unique(wsd$ISLAND)[i] & REEF_ZONE == "Forereef" & X2012 > 0)
 	wave <- ggplot(d, aes(x=X2012, y=TotFish))+
	 geom_point() +
	 geom_smooth(method="lm", formula=y~x) + 
	 labs(title = paste("Mean Wave Energy (2002-2012) and \n Total Fish Biomass at", i), x = "Mean Wave Energy", y = "Total Fish Biomass (g/m-2)")
	  ggsave(wave,filename=paste("graphs_tables/wave_tot",i,".png",sep="_")) 
}

# It would be nice to either add a facet to visualize them all at once (which I also can't entirely figure out) or simply save them as I'm trying to do here. However, this for some reason isn't working and I'm not sure where I've gone wrong. I get this error:
# Saving 8.94 x 6.78 in image Error in grid.draw(plot) : object 'wave' not found
# and don't totally understand why. Any tips?
# Thanks for the notes you just sent as well!

## if you want to have a have facet for each island - then no need for a loop
 	wave <- ggplot(wsd, aes(x=X2012, y=TotFish))+
	 geom_point() + 
	 facet_grid(. ~ ISLAND) + 
	 geom_smooth(method="lm", formula=y~x) + 
	 labs(title = paste("Mean Wave Energy (2002-2012) and \n Total Fish Biomass at", i), x = "Mean Wave Energy", y = "Total Fish Biomass (g/m-2)")
	  ggsave(wave,filename=paste("graphs_tables/wave_tot",i,".png",sep="_"))

