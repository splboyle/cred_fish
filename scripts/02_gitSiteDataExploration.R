## Data Exploration
# SB
# July 12, 2016

setwd("/Users/ShanBam/GitHub/fish_cred")
load("data/TMPwsd.Rdata")

library(ggplot2)

#st.err <- function(x, na.rm=FALSE) {
  if(na.rm==TRUE) x <- na.omit(x)
  sd(x)/sqrt(length(x))
}

wsd$meanWV <- rowMeans(wsd[,c("X2002", "X2003", "X2004", "X2005", "X2006", "X2007", "X2008", "X2009", "X2010", "X2011", "X2012")], na.rm=TRUE)
wsd$WV_SE <- apply(wsd[1333:1339], 1, st.err)
wsd$WV_SD <- apply(wsd[1333:1339], 1, sd)


preds<-c("meanWV", "SD_SH_DIFF","HARD_CORAL", "MA", "CCA", "DEPTH")

for (i in unique(preds))
  {
  #i<-1
  d <- subset(wsd, REEF_ZONE == "Forereef" & meanWV > 0)
  p <- ggplot(d, aes(x = preds[i], y = TotFish)) + 
    geom_point() + 
    geom_smooth(method="lm", formula=y~x) +
    facet_wrap(~ISLAND)
print(p)

}

ggsave(pred,filename=paste("TotFish",i,".png",sep="")