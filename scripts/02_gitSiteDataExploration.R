## Data Exploration
# SB
# July 12, 2016

setwd("/Users/ShanBam/GitHub/fish_cred")
load("data/TMPwsd.Rdata")

library(ggplot2)

st.err <- function(x, na.rm=FALSE) {
  if(na.rm==TRUE) x <- na.omit(x)
  sd(x)/sqrt(length(x))
}

wsd$meanWV <- rowMeans(wsd[,c("X2002", "X2003", "X2004", "X2005", "X2006", "X2007", "X2008", "X2009", "X2010", "X2011", "X2012")], na.rm=TRUE)
wsd$WV_SE <- apply(wsd[1333:1339], 1, st.err)
wsd$WV_SD <- apply(wsd[1333:1339], 1, sd)

# Cap quantiles 
fun <- function(x){
  quantiles <- quantile( x, c(.05, .95 ) )
  x[ x < quantiles[1] ] <- quantiles[1]
  x[ x > quantiles[2] ] <- quantiles[2]
  x
}
fun( yourdata )



### set up variables to be plotted here
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
  print(p)
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







############ Didn't work ############## go through logic later #############
# WORKING-ISH ONE - we can pick apart the logic of the one below if you want to understand why it doesn't work
preds<-c("meanWV", "SD_SH_DIFF","HARD_CORAL", "MA", "CCA", "DEPTH")


for(i in unique(preds)){
  #i<-1
  d <- subset(wsd, REEF_ZONE == "Forereef" & meanWV > 0)
  d<-d[,c(preds[i], "TotFish", "ISLAND")]
  DATA_COL<-names(d)[1]
  p <- ggplot(wsd, aes_string(x = DATA_COL, y = wsd$TotFish))+ 
    geom_point(na.rm = T) + 
    geom_smooth(method="lm", formula=y~x, na.rm = T) +
    scale_y_log10() +
    facet_wrap(~ISLAND) +
    labs(x = i, y = "Total Fish Biomass")
  print(p)
}
