rm(list=ls())
setwd("~/Documents/GitHub/cred_fish")

library(gdata)             # needed for drop_levels()
library(reshape)           # reshape library inclues the cast() function used below

source("lib/fish_team_functions.r")
source("lib/Islandwide Mean&Variance Functions.R")

#################################################################### TOW WORKUP ###########################################################################
## FIRST FISH, BENTHIC IS BELOW ... BE AWARE THAT THESE ARE MUCH ROUGHER AND GLITCHY THAN THE FISH REA SCRIPTS .. FAR LESS WORK HAS BEEN DONE ON THESE THAN FOR REA
#################################################################### TOW WORKUP ###########################################################################

load(file="data/ALL_TOW_FISH_RAW.rdata")

# FISH TOW WORKINGS -------------------------------------------------------
wd<-df

## drop any rows which have NOSC and MISS in the species field, these are tows which were aborted part way through
nosc<-which(wd$SPECIES == "NOSC")
wd<-wd[-nosc,]
miss<-which(wd$SPECIES == "MISS")
wd<-wd[-miss,]

summary(wd)
unique(wd[wd$PROJECTEDLENGTH<50,]$ISLAND)

#remove segments with distance less than 50m
###wd<-wd[wd$PROJECTEDLENGTH>50,]

#wd<-subset(wd, wd$REGION=="SAMOA", drop=TRUE)
#wd<-subset(wd, wd$ISLAND != "South Bank", drop=TRUE)
#wd<-droplevels(wd)
wd[is.na(wd$COUNT),]$COUNT<-0
wd[is.na(wd$DEPTH),]$DEPTH<-0	
wd[is.na(wd$SIZE_),]$SIZE_<-0	
wd[is.na(wd$CENTROIDLAT),]$CENTROIDLAT<-0	
wd[is.na(wd$CENTROIDLON),]$CENTROIDLON<-0	
length(unique(wd$DIVEID))

tmp.lev<-levels(wd$REEF_ZONE); head(tmp.lev)
levels(wd$REEF_ZONE)<-c(tmp.lev, "UNKNOWN")
tmp.lev<-levels(wd$FAMILY); head(tmp.lev)
levels(wd$FAMILY)<-c(tmp.lev, "UNKNOWN")
tmp.lev<-levels(wd$TAXONNAME); head(tmp.lev)
levels(wd$TAXONNAME)<-c(tmp.lev, "UNKNOWN")
tmp.lev<-levels(wd$TROPHIC_MONREP); head(tmp.lev)
levels(wd$TROPHIC_MONREP)<-c(tmp.lev, "UNKNOWN")

wd[is.na(wd$REEF_ZONE),"REEF_ZONE"]<-"UNKNOWN"
wd[is.na(wd$TAXONNAME),"TAXONNAME"]<-"UNKNOWN"
wd[is.na(wd$FAMILY),"FAMILY"]<-"UNKNOWN"
wd[is.na(wd$TROPHIC_MONREP),"TROPHIC_MONREP"]<-"UNKNOWN"

length(unique(wd$DIVEID))

wd$biomass_g<-wd$LW_A*wd$COUNT*((wd$SIZE*wd$LENGTH_CONVERSION_FACTOR)^wd$LW_B)

#Fix errors in the database
wd[wd$SIZE_ <50,]
wd[wd$SIZE_ < 50,]$COUNT<-0    # These are really presence records ... NOT a QUAN cont record


# set ISLAND of Lehua to Niihau (Lehua is a rock slightly offshore of Niihau)
wd[wd$ISLAND=="Lehua",]$ISLAND<-"Niihau"


#summarize tow information (length, depth, lat-long, date)
#    first do that by segment
TOW_DATA<-c("REGION", "ISLAND", "CENTROIDLAT", "CENTROIDLON", "DATE_", "DEPTH", "STARTLOCALTIME", "STRATA", "PROJECTEDLENGTH", "DIVEID")   

SEGMENT_ID2<-c( "DIVEID", "SEGMENT")
SEGMENT_INFO<-c("REGION", "ISLAND", "DATE_", "OBS_YEAR")
SEGMENT_INFO_TO_SUM<-c("PROJECTEDLENGTH")
SEGMENT_INFO_TO_AVE<-c("CENTROIDLAT", "CENTROIDLON", "DEPTH")
SEGMENT_INFO_TO_MIN<-c("STARTLOCALTIME")
SEGMENT_INFO_TO_MODE<-c("REEF_ZONE")

SEGMENT_FIELDS<-c(SEGMENT_INFO, SEGMENT_INFO_TO_SUM, SEGMENT_INFO_TO_AVE, SEGMENT_INFO_TO_MODE, SEGMENT_ID2)
DIVE_INFO<-c("DIVEID", SEGMENT_INFO)

#clean up the data file ## adel comment: this creates 14 warnings.... ### return to this, extract numeric only columns
##- invalid for factors with NA entries
wd[is.na(wd$BIOMASS),]$BIOMASS<-0
wd[is.na(wd$BIOMASS_G_M2),]$BIOMASS_G_M2<-0

segment.info<-aggregate(wd$COUNT, by=wd[,SEGMENT_FIELDS], sum)## aggregate sums total count of all fishes per record, using field_list 
segment.info<-segment.info[,SEGMENT_FIELDS] # drop the count - was just using that to generate a summary table

length(unique(segment.info$DIVEID))
	
#sum up to total length etc.. for the dive ID
#set depth, and centroid lat-long field to NaN if zero ... 
segment.info[segment.info$DEPTH==0,"DEPTH"]<-NaN
segment.info[segment.info$CENTROIDLAT==0,"CENTROIDLAT"]<-NaN
segment.info[segment.info$CENTROIDLON==0,"CENTROIDLON"]<-NaN

sum.segments<-aggregate(segment.info[,SEGMENT_INFO_TO_SUM],by=segment.info[,DIVE_INFO], sum, na.rm=TRUE);
dimnames(sum.segments)[[2]]<-c(DIVE_INFO, SEGMENT_INFO_TO_SUM)
ave.segments<-aggregate(segment.info[,SEGMENT_INFO_TO_AVE],by=segment.info[,DIVE_INFO], mean, na.rm=TRUE)  
med.segments<-aggregate(segment.info[,SEGMENT_INFO_TO_AVE],by=segment.info[,DIVE_INFO], median, na.rm=TRUE)  
mode.segments<-aggregate(segment.info[,SEGMENT_INFO_TO_MODE],by=segment.info[,DIVE_INFO], Mode)
dimnames(mode.segments)[[2]]<-c(DIVE_INFO, SEGMENT_INFO_TO_MODE)

tt<-merge(ave.segments, mode.segments[,c("DIVEID",SEGMENT_INFO_TO_MODE)], by="DIVEID"); dim(tt)
dive.info<-merge(tt, sum.segments[,c("DIVEID",SEGMENT_INFO_TO_SUM)], by="DIVEID"); dim(dive.info)

dive.info[is.na(dive.info$DEPTH),]
###FILLING IN SOME MISSING DEPTH VALUES FOR TOWS FROM THE CRUISE REPORT METADATAS
dive.info[dive.info$DIVEID == 200202261,]$DEPTH<-23
dive.info[dive.info$DIVEID == 200202262,]$DEPTH<-23.5
dive.info[dive.info$DIVEID == 200202263,]$DEPTH<-17.5
dive.info[dive.info$DIVEID == 200602192,]$DEPTH<-17  
dive.info[dive.info$DIVEID == 200602193,]$DEPTH<-17  
dive.info[dive.info$DIVEID == 200602194,]$DEPTH<-21.5  
dive.info[dive.info$DIVEID == 200602195,]$DEPTH<-16.5  
dive.info[dive.info$DIVEID == 200602196,]$DEPTH<-16.5  

dive.info[dive.info$DIVEID == 200602235,]$DEPTH<-15.83333333
dive.info[dive.info$DIVEID == 200803111,]$DEPTH<-15.60606061
dive.info[dive.info$DIVEID == 200803112,]$DEPTH<-14.39393939
dive.info[dive.info$DIVEID == 200803113,]$DEPTH<-18.18181818
dive.info[dive.info$DIVEID == 200803114,]$DEPTH<-17.72727273
dive.info[dive.info$DIVEID == 200803115,]$DEPTH<-17.12121212
dive.info[dive.info$DIVEID == 200803116,]$DEPTH<-15.15151515

dive.info[dive.info$DIVEID == 200202264,]$DEPTH<-0  #NO DEPTH INFO IN METADATFILE
dive.info[dive.info$DIVEID == 200202184,]$DEPTH<-0  #NO DEPTH INFO IN METADATFILE
dive.info[dive.info$DIVEID == 200402115,]$DEPTH<-0  #CAN NOT FIND THIS SURVEY IN THE CRUISE REPORT OR THE ORIGINAL CRUISE DATA
dive.info[dive.info$DIVEID == 200402116,]$DEPTH<-0  #CAN NOT FIND THIS SURVEY IN THE CRUISE REPORT OR THE ORIGINAL CRUISE DATA
dive.info[dive.info$DIVEID == 201503131,]$DEPTH<-18.9  #Average manually lifted from the SeaBird file
dive.info[dive.info$DIVEID == 201503132,]$DEPTH<-15.3  #Average manually lifted from the SeaBird file
dive.info[dive.info$DIVEID == 201503133,]$DEPTH<-17.4  #Average manually lifted from the SeaBird file
dive.info[dive.info$DIVEID == 201503134,]$DEPTH<-16.7  #Average manually lifted from the SeaBird file
dive.info[dive.info$DIVEID == 201503135,]$DEPTH<-16.9  #Average manually lifted from the SeaBird file

dive.info[is.na(dive.info$DEPTH),]$DIVEID

write.csv(dive.info, file="data/tmp Tows.csv")
############################################################
### Now sum abundance and biomass data per species per dive,
### and convert to gm2 and abund m2
############################################################

#Pull all species information into a separate df, for possible later use ..
FISH_SPECIES_FIELDS<-c("SPECIES","FAMILY", "TAXONNAME", "TROPHIC_MONREP")
t.species.table<-aggregate(wd$COUNT,by=wd[,FISH_SPECIES_FIELDS], sum, na.rm=FALSE)

sum.abund.bio<-aggregate(wd[,c("COUNT", "biomass_g")],by=wd[,c("DIVEID", "SPECIES", "SIZE_")], sum, na.rm=TRUE);
tfd<-merge(sum.abund.bio, dive.info[,c("DIVEID","PROJECTEDLENGTH")], by="DIVEID")
tfd$BIOGM2<-tfd$biomass_g / (10*tfd$PROJECTEDLENGTH)
tfd$ABUNM2<-tfd$COUNT / (10*tfd$PROJECTEDLENGTH)
length(unique(tfd$DIVEID))

## add consumer group to tow data, filter to forereef ONLY, add depth to give option to later filter by depth range .. then pool up by island & year and save SE
tfd<-merge(tfd, t.species.table[, FISH_SPECIES_FIELDS], by="SPECIES")
# add data about the tow (island, zone, year, depth)
tfd<-merge(tfd, dive.info[, c("DIVEID", "REGION", "ISLAND", "REEF_ZONE", "OBS_YEAR", "DEPTH")], by="DIVEID")
length(unique(tfd$DIVEID))

write.csv(tfd, file="data/TMPtowData.csv")

#filter out forereef tows only...!!!!! Ie drop Rose Backreef and Lagoon tows
tfd<-subset(tfd, tfd$REEF_ZONE %in% c("Forereef", "Unspecified"))
#set the one tow from Tutuuila with undpecified REEF_ZONE to Forereef .. depths are ~10m, so probably must be forereef (never mind that every other tow ever is forereef)
tfd[tfd$REEF_ZONE=="Unspecified",]$REEF_ZONE<-"Forereef"
length(unique(tfd$DIVEID))

summary(tfd$DEPTH)

#Filter out tows less than 10m OR greater than 20m
tfd<-subset(tfd, tfd$DEPTH>10 & tfd$DEPTH<20)
tfd<-droplevels(tfd)
length(unique(tfd$DIVEID))

### CAN FILTER OUT BELOW CERTAIN SIZES HERE...
SIZE_CUT_OFF<-1
tfd[tfd$SIZE_<SIZE_CUT_OFF, c("BIOGM2", "ABUNM2")]<-0

#### DROPPING BARRACUA - TOO VARIABLE
#tfd[tfd$FAMILY=="Sphyraenidae",]$BIOGM2<-0
#tfd[tfd$FAMILY=="Sphyraenidae",]$ABUNM2<-0

#use these as generic dtaa and pooling levels
tfd$DATA_COL<-tfd$BIOGM2
tfd$GROUPING<-tfd$SPECIES

xx<-aggregate(tfd$DATA_COL, by=tfd[,c("DIVEID", "REGION", "ISLAND", "OBS_YEAR", "REEF_ZONE", "GROUPING")], sum, na.rm=TRUE)
dimnames(xx)[[2]]<-c("DIVEID", "REGION", "ISLAND", "YEAR", "STRATA", "GROUPING", "DATA_COL")
#now format this more or less as a crosstab, with field of interest as column variable
wtd<-cast(xx, DIVEID + REGION + ISLAND + YEAR + STRATA ~ GROUPING, value="DATA_COL", fill=0)
data.cols<-levels(tfd$GROUPING)
wtd$TotFish<-rowSums(wtd[,data.cols])  

length(unique(wtd$DIVEID))

#aggregate - average per island/strata/year
tfi.mean<-aggregate(wtd[,c("TotFish", data.cols)],by=wtd[,c("REGION", "ISLAND", "STRATA", "YEAR")], mean, na.rm=TRUE)
tfi.n<-aggregate(wtd[,c("TotFish")],by=wtd[,c("REGION", "ISLAND", "STRATA", "YEAR")], length)
tfi.var<-aggregate(wtd[,c( "TotFish", data.cols)],by=wtd[,c("REGION", "ISLAND", "STRATA", "YEAR")], var, na.rm=TRUE)
tfi.se<-tfi.mean
tfi.se[,c("TotFish", data.cols)]<-sqrt(tfi.var[,c("TotFish", data.cols)])/sqrt(tfi.n$x)

# add the N to the mean and se dfs before writing them
tfi.mean$n<-tfi.se$n<-tfi.n$x

write.csv(tfi.mean, file="data/TMP FishTow_Mean.csv")
write.csv(tfi.se, file="data/TMP FishTow_SE.csv")


###################################################################
# using only 2008 onwards .. pool across any multiple years of surveys .. weighting each year's data equally
######## this is rough - but works for now! #############
island.data<-data.frame(tfi.mean, tfi.se)
island.data<-subset(island.data, island.data$STRATA=="Forereef", drop=TRUE)
island.data<-subset(island.data, island.data$YEAR>2007, drop=TRUE)
island.data<-droplevels(island.data)
idw<-aggregate(island.data[,5:9],by=island.data[,c("REGION","ISLAND")], mean, na.rm=TRUE)
idw.se<-aggregate(island.data[,11:15],by=island.data[,c("REGION","ISLAND")], tmpPooledSE)
island.data<-merge(idw, idw.se,by=c("REGION", "ISLAND"))
write.csv(island.data, file="tow fish 2008on forereef equallyweighted.csv")



# BENTHIC TOW -------------------------------------------------------------

##* BEGIN BENTHIC TOW - VISUAL ESTIMATES - RAW *******************
## NOTE THAT ALMOST NO WORK DONE ON THIS DATA IN THESE SCRIPTS TO DATE .. THERE IS ALMOST NO CHECKING OF NAs and So ON .. SOME OF THAT IS DONE ABOVE FOR THE FISH TOW .. THIS NEEDS MUCH MORE WORK TO BE ROBUST

load("data/ALL_TOW_BENT_RAW.rdata")
y<-df

BENTHIC_TOW_FIELDS<-c("REGION", "ISLAND", "YEAR", "DIVEID", "SEGMENT", "REEF_ZONE", "DEPTH", "STARTLOCALTIME", "CENTROIDLON", "CENTROIDLAT", "DOMINANT_HABITAT", "COMPVALUE","LIVE_CORAL_MID", "STRESSED_CORAL_BIN", "SOFT_CORAL_MID", "SAND_MID", "RUBBLE_MID", "PAVE_MID", "ROCK_MID", "ALGAE_MID", "MACROALGAE_MID", "CORALLINE_MID", "CROWN_OF_THORN", "URCHIN", "BORING_URCHIN", "FREE_URCHIN", "GIANT_CLAM", "PROJECTEDLENGTH")   


wtd<-y[,BENTHIC_TOW_FIELDS]

#create a depth field - default to the benthic depth field (if non blank), but use fish depth field if that exists and benthic one doesnt
x<-wtd$DEPTH; x[is.na(x) | x==0]<-NaN; wtd$DEPTH<- -x
x<-wtd$CENTROIDLON; x[is.na(x) | x==0]<-NaN; wtd$CENTROIDLON<-x
x<-wtd$CENTROIDLAT; x[is.na(x) | x==0]<-NaN; wtd$CENTROIDLAT<-x

# set ISLAND of Lehua to Niihau (Lehua is a rock slightly offshore of Niihau)
wtd[wtd$ISLAND=="Lehua",]$ISLAND<-"Niihau"

B_SEGMENT_ID<-c( "DIVEID", "SEGMENT")
B_SEGMENT_INFO<-c("REGION", "ISLAND", "YEAR")
B_SEGMENT_INFO_TO_SUM<-c("PROJECTEDLENGTH", "CROWN_OF_THORN", "URCHIN", "BORING_URCHIN", "FREE_URCHIN", "GIANT_CLAM")    #fields that should be summed across the entire tow (but that would only make sense if we have a length field for the segment or tow - leaving in for now) 
B_SEGMENT_INFO_TO_AVE<-c("CENTROIDLAT", "CENTROIDLON", "DEPTH", "COMPVALUE", "LIVE_CORAL_MID", "SOFT_CORAL_MID", "RUBBLE_MID", "PAVE_MID", "ROCK_MID", "ALGAE_MID", "MACROALGAE_MID", "CORALLINE_MID")
B_SEGMENT_INFO_TO_MIN<-c("STARTLOCALTIME")
B_SEGMENT_INFO_TO_MODE<-c("DOMINANT_HABITAT", "REEF_ZONE")
B_SEGMENT_FIELDS<-unique(c(B_SEGMENT_INFO, B_SEGMENT_INFO_TO_SUM, B_SEGMENT_INFO_TO_AVE, B_SEGMENT_INFO_TO_MIN, B_SEGMENT_ID, B_SEGMENT_INFO_TO_MODE))
B_SEGMENT_DATA<-unique(c(B_SEGMENT_INFO_TO_AVE, B_SEGMENT_INFO_TO_MODE))
B_DIVE_INFO<-c("DIVEID", B_SEGMENT_INFO)

0
b.sum.data<-aggregate(wtd[,B_SEGMENT_INFO_TO_SUM],by=wtd[,B_DIVE_INFO], sum, na.rm=TRUE) 
xx<-b.sum.data[,B_SEGMENT_INFO_TO_SUM]/(10*b.sum.data$PROJECTEDLENGTH)
xx$PROJECTEDLENGTH<-b.sum.data$PROJECTEDLENGTH 
b.abundM2<-b.sum.data
b.abundM2[,B_SEGMENT_INFO_TO_SUM]<-xx
b.ave.data<-aggregate(wtd[,B_SEGMENT_INFO_TO_AVE],by=wtd[,B_DIVE_INFO], mean, na.rm=TRUE)  
b.mode.data<-aggregate(wtd[,B_SEGMENT_INFO_TO_MODE],by=wtd[,B_DIVE_INFO], Mode)  
dimnames(b.mode.data)[[2]]<-c(B_DIVE_INFO, B_SEGMENT_INFO_TO_MODE)

xx<-merge(b.ave.data, b.mode.data[,c("DIVEID", B_SEGMENT_INFO_TO_MODE)], by="DIVEID")
tow.info<-merge(xx, b.abundM2[,c("DIVEID", B_SEGMENT_INFO_TO_SUM)], by="DIVEID")

#aggregate - average per island/year
t.benth.island.mean<-aggregate(tow.info[,B_SEGMENT_INFO_TO_AVE],by=tow.info[,c("REGION", "ISLAND","YEAR", "REEF_ZONE")], mean, na.rm=TRUE)
t.benth.island.var<-aggregate(tow.info[,B_SEGMENT_INFO_TO_AVE],by=tow.info[,c("REGION", "ISLAND","YEAR", "REEF_ZONE")], var, na.rm=TRUE)
t.benth.island.n<-aggregate(tow.info[,c("DIVEID")],by=tow.info[,c("REGION", "ISLAND", "YEAR", "REEF_ZONE")], length)

t.benth.island.se<-sqrt(t.benth.island.var[,B_SEGMENT_INFO_TO_AVE])/sqrt(t.benth.island.n$x)
t.benth.island.mean$N<-t.benth.island.n$x

write.csv(t.benth.island.mean, file="tow_benthic_vis_est.csv")
write.csv(t.benth.island.se, file="tow_benthic_vis_est_se.csv")

###################################################################
# using only 2008 onwards .. pool across any multiple years of surveys .. weighting each year's data equally
######## this is rough - but works for now! #############
island.data<-data.frame(t.benth.island.mean, t.benth.island.se)
island.data<-subset(island.data, island.data$REEF_ZONE=="Forereef", drop=TRUE)
island.data<-subset(island.data, island.data$YEAR>2007, drop=TRUE)
island.data<-droplevels(island.data)
idw<-aggregate(island.data[,5:16],by=island.data[,c("REGION","ISLAND")], mean, na.rm=TRUE)
idw.se<-aggregate(island.data[,18:29],by=island.data[,c("REGION","ISLAND")], tmpPooledSE)
island.data<-merge(idw, idw.se,by=c("REGION", "ISLAND"))
write.csv(island.data, file="tow benthic 2008on forereef equallyweighted.csv")




#################################################################### TOW WORKUP END #######################################################################








































