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

### VERY MESSY - WILL CLEAN UP
## Struggling with for Loop

# WORKING TO RUN THROUGH DIFFERENT PREDICTOR TERMS


### set up variables to be plotted here
response<-c("TotFish")

response_label<-c("Total Fish Biomass") # label you want

pred.cols<-c("meanWV", "SD_SH_DIFF","HARD_CORAL", "MA", "CCA", "DEPTH")

pred_label<-c("Mean Wave Energy", "Benthic Complexity", "Coral Cover", "Macroalgal Cover", "CCA Cover", "Depth") # predictor labels 

####

for(i in 1:length(pred.cols)){

##i<-1

d<-wsd[,c(response, pred.cols[i], "ISLAND")]

p <- ggplot(d, aes_string(x = unique(pred.cols)[i], y = response))+ 
 	geom_point(na.rm = T) + 
	geom_smooth(method="lm", formula=y~x, na.rm = T) +
    scale_y_log10() +
    facet_wrap(~ISLAND) +
    labs(x = pred_label[i], y = response_label)+ theme_classic()
  print(p)

} 


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
# This one still isn't correctly identifying that I want to plot all values within the predictor columns. Here 'i' is seen as the first value within the first variable "meanWV = 114.556...". The graph doesn't look entirely wrong but i is definitely not correctly identified. 


## Should look like this plot for all six predictor variables. 
ggplot(wsd, aes(DEPTH, TotFish)) + 
  geom_point() + 
  geom_smooth(method = "lm", formula = y~x) +
  scale_y_log10() +
  facet_wrap(~ISLAND)

