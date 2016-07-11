### 
rm(list=ls())
setwd("~/GitHub/cred_fish")
load("/Users/ShanBam/GitHub/cred_fish/data/TMPwsd.Rdata")

# wsd<-wsd_mains - unnecessary?

#set.seed(627)
#########
require(reshape2)
require(gradientForest)
require(ggplot2)
require(extendedForest)
require(grid)

source("lib/gradForestFunc_v002.R") ## from Jamie Tam and Scott Large
###########
#Set up data (use region specific env and bio dataframes)
#dat<-cbind(PIenv, PIbio)

###################
## fish indicators
#
#"Other_A"         "CAME_A"          "MOGR_A"         
#"PAPO_A"          "SCRU_A"          "ZEFL_A"          "Other"          
#"CAME"            "MOGR"            "PAPO"            "SCRU"           
#"ZEFL"            "Other_ML"        "CAME_ML"         "MOGR_ML"        
#"PAPO_ML"         "SCRU_ML"         "ZEFL_ML"         "SPECIESRICHNESS"
#"PISCIVORE"       "PLANKTIVORE"     "PRIMARY"         "SECONDARY"      
#"UNKNOWN"         "ALL_FISH_BIO"    "B"               "C"              
#"DET"             "GD"              "LSE"             "MI"             
#"OM"              "OTHER_H"         "PST"             "PIS"            
#"PK"              "SSE"             "SI"              "PISC_PLANK"   

##########
## habitat drivers, enviro and human pressures
##"DEPTH"          
#"HARD_CORAL"      "SOFT_CORAL"      "MA"              "CCA"            
#"TA"              "SAND"            "CYANO"           "OTHER_BENTHIC"  
# "ComplexityValue"
#"SST_SD"          "SST_CLIM_M"      "SST_ANOM_M"      "SST_ANOM_F"     
#"PAR_MEAN"        "PAR_SD"          "PAR_CLIM_M"      "PAR_ANOM_M"     
#"PAR_ANOM_F"      "CHL_MEAN"        "CHL_SD"          "CHL_CLIM_M"     
#"CHL_ANOM_M"      "CHL_ANOM_F"      "WAVES_MEAN"      "WAVES_SD"       
#"WAVES_CLIM"      "WAVES_ANOM"      "WAVES_AN_1"      "HUMANS20"  

ind.name<-c("ALL_FISH_BIO", "SPECIESRICHNESS", "PISCIVORE","PLANKTIVORE","PRIMARY",
            "SECONDARY", "PISC_PLANK","PISC_BENTH","ML_ADULT_ALL","B", "DET", "GD", 
            "LSE", "SSE", "CAME","MOGR","PAPO","SCRU","ZEFL")
dri.name<-c("HARD_CORAL", "CCA", "MA", "ComplexityValue", "DEPTH", "HUMANS20", "MEAN_HII_40",
            "SST_MEAN", "SST_SD", "SST_CLIM_M", "SST_ANOM_M","SST_ANOM_F",
            "PAR_MEAN","PAR_SD", "PAR_CLIM_M","PAR_ANOM_F", "PAR_ANOM_M", 
            "CHL_MEAN","CHL_SD","CHL_CLIM_M", "CHL_ANOM_F","CHL_ANOM_M",
            "WAVES_MEAN", "WAVES_SD", "WAVES_CLIM", "WAVES_ANOM","WAVES_AN_1") 

# Many of the pressure variables do not have full data (some NA):
## Option 1- Impute:
#dat <- na.roughfix(dat)
#
## Option 2- Get rid of the columns without full time series:
#
# dat.sc <- dat.full[,apply(dat.full, 2, function(x)!all(is.na(x)))]
#
# Somewhat roundabout way of doing this, but can also be used to find the best
# contiguous set of data.
# if any column has less than XX years, omit.
#
# cuts <- 10
# len.list <- sapply(dat.sc, function(x) length(na.contiguous(x)))
# keep.list <- names(len.list[len.list >= cuts])
# dat.kl <- dat.sc[, keep.list] 
# dat <- dat.kl[apply(dat.kl, 1, function(x)!any(is.na(x))),]
#
## Option 3- Simply use as is (what I initially tried, but it might not run...)
# dat <- dat.full
#dat1<-dat.full



# Maximum level of splits
lev <- floor(log2(nrow(wsd) * 0.368/2))
#colnames(dat)[!colnames(dat) %in% c(dri.name, ind.name)]\
#dat<-data.frame(dat)
#
## GF analysis ##
tester <-   gradientForest(data = wsd, 
                           predictor.vars = dri.name, 
                           response.vars = ind.name,
                           ntree = 1000, 
                           transform = NULL,
                           maxLevel = lev,
                           corr.threshold = 0.5, 
                           compact = F,
                           trace = T)

#save Rdata

save(tester, file = "tester.RDATA")

#load("tester.RDATA")


#
##########################
# Model Importance Table #
##########################
#
# Cumulative importance

var.order <- names(importance(tester, type = "Weighted", sort = TRUE))
#
indicatorIMP <- tester$imp.rsq
indicatorNA <- colnames(indicatorIMP)[apply(indicatorIMP, 2, function(x)all(is.na(x)))]
indicatorIMP <- indicatorIMP[apply(indicatorIMP, 2, function(x)!all(is.na(x))),]
#
indicatorDF <- data.frame("VARIABLE" = colnames(indicatorIMP),
                          "TYPE" = "INDICATOR", melt(apply(indicatorIMP, 2, mean, na.rm = T),
                                                     variable.name = "VARIABLE",
                                                     value.name = "MEAN"),
                          melt(apply(indicatorIMP, 2, min, na.rm = T),
                               variable.name = "VARIABLE", 
                               value.name = "MIN"),
                          melt(apply(indicatorIMP, 2, max, na.rm = T),
                               variable.name = "VARIABLE", 
                               value.name = "MAX"))
#
pressureIMP <- tester$imp.rsq
pressureNA <- row.names(pressureIMP)[apply(pressureIMP, 1, function(x)all(is.na(x)))]
pressureIMP <- pressureIMP[apply(pressureIMP, 1, function(x)!all(is.na(x))),]
pressureDF <- data.frame("VARIABLE" = row.names(pressureIMP),
                         "TYPE" = "PRESSURE", melt(apply(pressureIMP, 1, mean, na.rm = T),
                                                   variable.name = "VARIABLE",
                                                   value.name = "MEAN"),
                         melt(apply(pressureIMP, 1, min, na.rm = T),
                              variable.name = "VARIABLE", 
                              value.name = "MIN"),
                         melt(apply(pressureIMP, 1, max, na.rm = T),
                              variable.name = "VARIABLE", 
                              value.name = "MAX"))
#
indicatorDF[,c(3:5)] <- apply(indicatorDF[,c(3:5)], 2, signif, 2)
pressureDF[,c(3:5)] <- apply(pressureDF[,c(3:5)], 2, signif, 2)
# indicatorDF$VARIABLE <- factor(indicatorDF$VARIABLE, labels = c(ENTER APPROPRIATE NAMES HERE))
# pressureDF$VARIABLE <- factor(pressureDF$VARIABLE, labels = c(ENTER APPROPRIATE NAMES HERE))

#pressureDF <- rbind(pressureDF, cbind(VARIABLE = pressureNA,TYPE = "PRESSURE", MEAN = "NA", MIN = "NA", MAX = "NA"))
#
modPerformance <- rbind(pressureDF, indicatorDF)
##
write.csv(modPerformance, file = "testerModelPerformance_v001_2.csv")
#         
#########
# PLOTS #
#########
#
imp.vars <- names(importance(tester)[importance(tester) > 0])
#
# Overall Importance
png(file = "tester_importance_v01.png", width = 83, height = 83, units = "mm", res = 600)
par(mgp = c(.75, 0.05, 0), 
    mar = c(2.75, 6.5, 0.1, .5),
    omi = c(0, 0.3, 0.1, 0),
    family = "sans", 
    tck = -0.015)
impPlot(tester,
        cex.main = 0.7)
mtext(expression(paste(R^2, " weighted importance")), side = 1, line = 1.75, cex = .75)
dev.off()
#
#
# Split Ratio
png(file = "testersplitRatio_v01.png", width = 173.5, height = 173.5, units = "mm", res = 600)
# tiff(file = paste0(figure.dir, "PIE-splitRatio_v01.tiff"), width = 173.5, height = 173.5, units = "mm", res = 600)
par(mgp = c(.75, 0.05, 0), 
    mar = c(2.25, 1, 0.1, .5),
    omi = c(0,0.3, 0.1, 0),
    family = "sans", 
    tck = -0.015)
spRatio(tester,
        imp.vars = imp.vars,
        #           imp.vars.names = #c(ADD NAMES HERE),
        leg.posn = "topright",
        cex.legend = 0.4, cex.axis = 0.6,
        cex.lab = 0.7, line.ylab = 0.9)
dev.off()

# Density of Splits
png(file = "testersplitImportance_v01.png", width = 173.5, height = 173.5, units = "mm", res = 600)
# tiff(file = paste0(figure.dir, "PIE-splitImportance_v01.tiff"), width = 173.5, height = 173.5, units = "mm", res = 600)
par(mgp = c(.75, 0.05, 0), 
    mar = c(2.25, 1, 0.1, .5),
    omi = c(0,0.3, 0.1, 0),
    family = "sans", 
    tck = -0.015)
spImportance(tester,
             imp.vars = imp.vars,
             #imp.vars.names = c("Coastal_engineering"),
             leg.posn = "topright", cex.legend = 0.4, cex.axis = 0.6,
             cex.lab = 0.7, line.ylab = 0.9)
dev.off()
#
# Density of Data
png(file = "testersplitData_v01.png", width = 173.5, height = 173.5, units = "mm", res = 600)
# tiff(file = paste0(figure.dir, "PIE-splitData_v01.tiff"), width = 173.5, height = 173.5, units = "mm", res = 600)
#
par(mgp = c(.75, 0.05, 0), 
    mar = c(2.25, 1, 0.1, .5),
    omi = c(0,0.3, 0.1, 0),
    family = "sans", 
    tck = -0.015)
spData(tester,
       imp.vars = imp.vars,
       #           imp.vars.names = #c(ADD NAMES HERE),
       leg.posn = "topright", cex.legend = 0.4, cex.axis = 0.6,
       cex.lab = 0.7, line.ylab = 0.9)
dev.off()
#
# Cumulative Importance (split)
png(file ="testercumImportanceSplit_v01.png", width = 173.5, height = 173.5, units = "mm", res = 600)
# tiff(file = paste0(figure.dir, "PIE-cumImportanceSplit_v01.tiff"), width = 173.5, height = 173.5, units = "mm", res = 1000)

par(mgp = c(1.0, 0.1, 0),
    mar = c(2.25, 1, 0.1, 0.5),
    omi = c(0,0.3, 0.1, 0),
    family = "sans", tck = -0.015)
spCumPlot(tester, 
          imp.vars= imp.vars, 
          #           imp.vars.names = #c(ADD NAMES HERE),
          show.species = TRUE,
          legend = TRUE,
          show.overall = FALSE,
          common.scale = TRUE,
          leg.nspecies = 13,
          #           leg.posn = "topright",
          cex.lab = 0.9, cex.legend = 1, cex.axis = 0.8, line.ylab = 0.8)
dev.off()
#
# Cumulative Importance (total)
png(file = "testercumImportance_v01.png", width = 173.5, height = 173.5, units = "mm", res = 600)
# tiff(file = paste0(figure.dir, "PIE-cumImportance_v01.tiff"), width = 173.5, height = 173.5, units = "mm", res = 600)
#
par(mgp = c(1.0, 0.1, 0),
    mar = c(2.25, 1, 0.1, 0.5),
    omi = c(0,0.3, 0.1, 0),
    family = "sans", tck = -0.015)
spCumPlot(tester, 
          imp.vars = imp.vars, 
          #           imp.vars.names = #c(ADD NAMES HERE),
          show.species = FALSE,
          show.overall = TRUE,
          common.scale = TRUE,
          leg.nspecies = 13,
          cex.lab = 0.9, cex.legend = 0.7, cex.axis = 0.8, line.ylab = 0.8)
dev.off()
#
# Collect PCA info
Trns_grid <- predict(tester, wsd_mains[, imp.vars])
row.names(Trns_grid) <- c(2003:2013)
PCs <- prcomp(Trns_grid[, imp.vars])
#
ggsave(file="PIPCA_v01.png", PCbiplot(PCs),
       width = 83, height = 83, units = "mm", scale = 2)

################
#find names of important variables
imp.vars
#pull data to get threshold quantiles from split density
a<-tester$d$Exploitation$x
b<-tester$d$Population$x
c<-tester$d$Landings$x
d<-tester$d$AMO$x
e<-tester$d$SST$x
f<-tester$d$Seafood$x
g<-tester$d$NAO_w$x
h<-tester$d$GS$x
i<-tester$d$MEI$x
PI.dens<-cbind(a,b,c,d,e,f,g,h,i)

PI.dens<-as.data.frame(PI.dens)
colnames(PI.dens)<-c("Exploitation", "Population", "Landings", "AMO", "SST", "Seafood", "NAO_w", "GS", "MEI")
PI.d<-sapply(PI.dens, summary)
write.csv(PI.d, file="PI.dens.split.csv")