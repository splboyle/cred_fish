setwd("G:/Fish/SiteProject")
load("TMPwsd.Rdata")
load("functional_FG/Clean PRIA Site_FG.RData")

sp.rich <- read.csv("species_richness_by_SITEVISITID_SPC.csv")
sp.rich <- subset(sp.rich, METHOD == "nSPC")

st.err <- function(x, na.rm=FALSE) {
  if(na.rm==TRUE) x <- na.omit(x)
  sd(x)/sqrt(length(x))
}

wsd$meanWV <- rowMeans(wsd[,c("X2002", "X2003", "X2004", "X2005", "X2006", "X2007", "X2008", "X2009", "X2010", "X2011", "X2012")], na.rm=TRUE)
wsd$WV_SE <- apply(wsd[1333:1339], 1, st.err)
wsd$WV_SD <- apply(wsd[1333:1339], 1, sd)


wsd1 <- merge(sp.rich, wsd, by = "SITEVISITID")
wsd1$METHOD.y <- NULL
wsd1$X <- NULL

nn <- data.frame(wsd1$SITEVISITID, wsd1$SITE, wsd1$SPECIESRICHNESS, wsd1$meanWV, wsd1$WV_SD, wsd1$SD_SH_DIFF)
colnames(nn) <- c("SITEVISITID", "SITE", "SPECIESRICHNESS", "meanWV", "WV_SD", "SD_SH_DIFF")

wsd <- subset(wsd, OBS_YEAR != 2008)

FG <- merge(nn, wsdFG, by = "SITEVISITID")
FG$SITE.y <- NULL
colnames(FG)[2] <- "SITE"

PIenv <- data.frame(FG$SITE, FG$ISLAND, FG$DEPTH, FG$meanWV, FG$WV_SD, FG$HARD_CORAL, FG$SAND, FG$MA, FG$CCA, FG$SD_SH_DIFF)

