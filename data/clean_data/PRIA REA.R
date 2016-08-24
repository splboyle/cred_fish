rm(list=ls())
library(gdata)             # needed for drop_levels()
library(reshape)           # reshape library inclues the cast() function used below

#LOAD LIBRARY FUNCTIONS ... 
source("/Users/ivor.williams/Documents/CRED/Fish Team/FishPaste/fish-paste/lib/fish_team_functions.R")
source("/Users/ivor.williams/Documents/CRED/Fish Team/FishPaste/fish-paste/lib/Islandwide Mean&Variance Functions.R")

# get strata and sectors data data - NB - the data in the raw file should be checked and updated
sectors<-read.csv("/Users/ivor.williams/Documents/CRED/Fish Team/FishPaste/fish-paste/data/Sectors-Strata-Areas2015.csv", stringsAsFactors=FALSE)
# load site master to merge with sector names
site_master<-read.csv("/Users/ivor.williams/Documents/CRED/Fish Team/FishPaste/fish-paste/data/SITE MASTER2015.csv")
site_master$SITE<-SiteNumLeadingZeros(site_master$SITE)

setwd("/Users/ivor.williams/Documents/CRED/Fish Team/Data Requests/PRIA Monitoring Report")
#source("WPRFMC_functions.R")
sb<-read.csv("SB Func Groups.csv")
sb$SPECIES<-sb$Code
sb$X<-sb$X.1<-sb$X.2<-sb$X.3<-NA

# Attached is the cleaned up, finalized functional grouping. The last question I had was about the gobies and blennies. I created a separate category for them ("Gobies/Blennies") that is separate from "Other" (which includes just Synodontids). I'm not sure if we want to remove gobies and blennies completely, keep them together in a separate group, or group them in "Other". I'm fine with whatever you think.

# Below is the list of the various data things I need for the project and the report. I tried to make it as clear as possible but let me know if you have questions. 

# ALL SPC DATA (for report and project)
	# •	Remove true noise (species highlighted in excel file as well) 
	# ◦	DEMA
	# ◦	ENPU 
	# ◦	MABI 
	# ◦	SPDE 
	# ◦	SECR 
	# ◦	KUSA
# SPC for Functional Assemblage PROJECT
	# •	Biomass of each functional group at site level 
	# ◦	FILTER: Noise removed
	# ◦	NO cap 
# SPC Data for REPORT
	# •	Biomass (total, cons. groups, and parrotfish) at site level without cap (will be good to have) 
	# •	Biomass (total, cons. groups, and parrotfish) at site level with:
	# ◦	FILTER: Noise removed
	# ◦	CAP: Island-specific 95% cap on biomass of total and each group 
	# •	Island-level and island-level-by-year means (I believe they are weighted by strata) and SE for total biomass and biomass of each group 

### IDW  -ALSO REMOVE TURTLES, DOLPHINS etc,,
##### IDW ALSO NEED TO PROVIDE THE OLD BELT DATA FOR THEM TO GENERATE LONG-TERM TRENDS. 
##### IT MAY OR MAY NOT BE POSSIBLE TO USE THE WEIGHTED AVERAGING ROUTINES .. BUT PROBABLY WONT WORK WELL, AS THOSE ARE ALL OR NEARLY ALL MID-DEPTH REEFS .. SO PROBABLY PROVIDE THOSE JUST AS SITE LEVEL DATA

## LOAD AND CLEAN fish data
load("/Users/ivor.williams/Documents/CRED/Fish Team/FishPaste/fish-paste/data/ALL_REA_FISH_RAW.rdata")
x<-df


# HOUSEKEEPING ------------------------------------------------------------
DATA_COLS<-c("SITEVISITID", "METHOD", "DATE_", "OBS_YEAR",  "SITE", "REEF_ZONE",  "DEPTH_BIN",  "ISLAND", "LATITUDE",  "LONGITUDE",  "REGION" , "REGION_NAME", "SECTOR", "SPECIAL_AREA", "EXCLUDE_FLAG",
"REP",  "REPLICATEID", "DIVER", "HABITAT_CODE", "DEPTH", 
"HARD_CORAL", "MA",  "TA",  "CCA",  "SAND",  "SOFT_CORAL", "CLAM" , "SPONGE", "CORALLIMORPH", "CYANO", "TUNICATE", "ZOANTHID" , "COMPLEXITY", "TRAINING_YN",
"SPECIES", "COUNT", "SIZE_", "OBS_TYPE", 
"SUBSTRATE_HEIGHT_0", "SUBSTRATE_HEIGHT_20", "SUBSTRATE_HEIGHT_50", "SUBSTRATE_HEIGHT_100", "SUBSTRATE_HEIGHT_150", "MAX_HEIGHT",
"SCIENTIFIC_NAME",  "TAXONNAME", "COMMONNAME", "GENUS", "FAMILY" , "COMMONFAMILYALL", "LMAX", "LW_A",  "LW_B",  "LENGTH_CONVERSION_FACTOR", "TROPHIC", "TROPHIC_MONREP")
head(x[,DATA_COLS])
x<-x[,DATA_COLS]

#generate a simple "Strata" field, by concatenating Stratum and Depth fields
x$STRATA<-paste(x$REEF_ZONE, x$DEPTH_BIN, sep='')

## Update SITE to have three numeric digits (eg OAH-01 becomes OAH-001)
x$SITE<-SiteNumLeadingZeros(x$SITE)

x[is.na(x$TRAINING_YN),]$TRAINING_YN<-FALSE   # Training falg of NA is equivalent to a FALSE .. as none of the odler data was 'training data'
x<-subset(x, x$TRAINING_YN==FALSE)
x<-subset(x, x$EXCLUDE_FLAG==0, drop=TRUE)
x<-subset(x, x$METHOD %in% c("nSPC"), drop=TRUE)
x<-subset(x, x$OBS_YEAR >2006, drop=TRUE)
x<-subset(x, x$OBS_TYPE %in% c("U","I","N"))  # note this includes all the data .. wlll need to add filtering to the scripts that analyse the data
x<-subset(x, x$REGION %in% c("PRIAs"))  # note this includes all the data .. wlll need to add filtering to the scripts that analyse the data

#add SITE MASTER information to x  #IDW - note that if we join on SITE then SITE MASTER would also join to all surveys at a site .. for nSPC there are no duplicates, but some of those sites were oldeer BLT sites that were also survyed in earlier years.
# this would be better if SECTOR field in database was up to date properly .. rather than merge with the site_Sectors spreadsheet
x<-merge(x, site_master[,c("SITE", "SEC_NAME", "ANALYSIS_SEC", "ANALYSIS_YEAR", "ANALYSIS_STRATA")], by="SITE", all.x=TRUE)

#setting the MHI and NWHI sitse ANALYSIS YEAR to the range
table(x$OBS_YEAR, x$REGION)

x$ANALYSIS_YEAR<-as.character(x$ANALYSIS_YEAR)
x[x$REGION %in% c("PRIAs") & x$OBS_YEAR %in% c(2008,2009),]$ANALYSIS_YEAR<-"2008-09"
x[x$REGION %in% c("PRIAs") & x$OBS_YEAR %in% c(2010,2011),]$ANALYSIS_YEAR<-"2010-11"
x[x$REGION %in% c("PRIAs") & x$OBS_YEAR %in% c(2012,2013, 2014),]$ANALYSIS_YEAR<-"2012-14"
x[x$REGION %in% c("PRIAs") & x$OBS_YEAR %in% c(2015,2016, 2017),]$ANALYSIS_YEAR<-"2015-17"

#CHECK THAT all ANALYSIS_SEC are present in the site_master file)
idw<-x[is.na(x$ANALYSIS_SEC)  & x$METHOD=="nSPC", c("REGION", "SITE","OBS_YEAR", "METHOD"),]
if(dim(idw)[1]>0) {cat("nSPC sites with MISSING ANALYSIS_SEC")}   # should be 0

#for ones that are missing, set it to ISLAND
no_secs<-is.na(x$ANALYSIS_SEC)
tmp<-as.character(x$ANALYSIS_SEC)
tmp[no_secs]<-as.character(x[no_secs,]$ISLAND)
x$ANALYSIS_SEC<-tmp

############################################################################################
###### new section .. where there is substrate_height data, work out average height && ave_height_variability so that we get standardized complexity metrics (mean hieght, mean height variability, max-height) 
sh_out<-CalcMeanSHMeanSHDiff(x)
x$MEAN_SH<-sh_out[[1]]
x$MEAN_SH_SD<-sh_out[[2]]

# remove the component SUBSTRATE_HEIGHT fields
x<-x[, setdiff(names(x),c("SUBSTRATE_HEIGHT_0", "SUBSTRATE_HEIGHT_20", "SUBSTRATE_HEIGHT_50", "SUBSTRATE_HEIGHT_100", "SUBSTRATE_HEIGHT_150"))]
############################################################################################
x<-droplevels(x)

#convert COMPLEXITY to a numeric field ### 
x$COMPLEXITY<-as.vector(toupper(x$COMPLEXITY))
x[is.na(x$COMPLEXITY),"COMPLEXITY"]<-"UNKNOWN"
COMPLEXITY_VALUES<-toupper(c("Low", "Med-Low", "Med", "Med-Hi", "Hi", "Very-Hi"))
x$ComplexityValue<-NaN
for (i in 1:length(COMPLEXITY_VALUES)){
	if(COMPLEXITY_VALUES[i] %in% x$COMPLEXITY){
		x[x$COMPLEXITY==COMPLEXITY_VALUES[i],]$ComplexityValue<-i
	}
}

#######################
## CLEAN UP NAs #######
#######################
tmp.lev<-levels(x$HABITAT_CODE); head(tmp.lev)
levels(x$HABITAT_CODE)<-c(tmp.lev, "UNKNOWN")
tmp.lev<-levels(x$SCIENTIFIC_NAME); head(tmp.lev)
levels(x$SCIENTIFIC_NAME)<-c(tmp.lev, "UNKNOWN")
tmp.lev<-levels(x$COMMONNAME); head(tmp.lev)
levels(x$COMMONNAME)<-c(tmp.lev, "UNKNOWN")
tmp.lev<-levels(x$GENUS); head(tmp.lev)
levels(x$GENUS)<-c(tmp.lev, "UNKNOWN")
tmp.lev<-levels(x$FAMILY); head(tmp.lev)
levels(x$FAMILY)<-c(tmp.lev, "UNKNOWN")
tmp.lev<-levels(x$COMMONFAMILYALL); head(tmp.lev)
levels(x$COMMONFAMILYALL)<-c(tmp.lev, "UNKNOWN")
tmp.lev<-levels(x$TROPHIC_MONREP); head(tmp.lev)
levels(x$TROPHIC_MONREP)<-c(tmp.lev, "UNKNOWN")

x[is.na(x$HABITAT_CODE),"HABITAT_CODE"]<-"UNKNOWN"
x[is.na(x$SCIENTIFIC_NAME),"SCIENTIFIC_NAME"]<-"UNKNOWN"
x[is.na(x$COMMONNAME),"COMMONNAME"]<-"UNKNOWN"
x[is.na(x$GENUS),"GENUS"]<-"UNKNOWN"
x[is.na(x$FAMILY),"FAMILY"]<-"UNKNOWN"
x[is.na(x$COMMONFAMILYALL),"COMMONFAMILYALL"]<-"UNKNOWN"
x[is.na(x$TROPHIC_MONREP),"TROPHIC_MONREP"]<-"UNKNOWN"

#x[is.na(x$COUNT),]$COUNT<-0
#x[is.na(x$SIZE_),]$SIZE_<-0
###x[is.na(x$LMAX),]$LMAX<-999

x<-droplevels(x)

#BENTHOS DOES NOT ALWAYS SUM TO 100% .. THIS IS A (LONG-LIVED!) TEMP FIX .. PROBABLY BETTER TO FIX THIS INSIDE ORACLE
BENTHIC_FIELDS<-c("HARD_CORAL", "SOFT_CORAL", "MA", "CCA", "TA", "SAND", "CYANO", "CLAM", "CORALLIMORPH", "ZOANTHID", "TUNICATE", "SPONGE")
UNIQUE_ROUND<-c("REGION", "OBS_YEAR", "METHOD")
round_table<-Aggregate_InputTable(x, UNIQUE_ROUND)

x$countBD<-apply(x[,BENTHIC_FIELDS], 1, function(xx) length(which(!is.na(xx))))  #IDW 10-22-2013 checking for situation where there is NO benthic data at all
for(i in 1:dim(round_table)[1])
{
	if(round_table[i,"METHOD"] %in% c("nSPC", "nSPC-CCR"))
	{
		tmp_data<-x[x$OBS_YEAR==round_table[i,"OBS_YEAR"] & x$METHOD==round_table[i,"METHOD"] & x$REGION==round_table[i,"REGION"],]

		#go through BENTHIC_FIELDS, checking whether there are some NAs and some data values
		for(j in 1:length(BENTHIC_FIELDS))
		{
			## IF there are both non NAs and NAs
			if(length(tmp_data[!is.na(tmp_data[,BENTHIC_FIELDS[j]]),BENTHIC_FIELDS[j]]) > 0 
			        & length(tmp_data[is.na(tmp_data[,BENTHIC_FIELDS[j]]),BENTHIC_FIELDS[j]]) > 0) 
			{
				#set all NAs of that field to 0
				tmp_data[is.na(tmp_data[,BENTHIC_FIELDS[j]]),BENTHIC_FIELDS[j]]<-0	

				#now rewrite the benthic fields with NAs converted to zeros
				x[x$OBS_YEAR==round_table[i,"OBS_YEAR"] & x$METHOD==round_table[i,"METHOD"] & x$REGION==round_table[i,"REGION"],BENTHIC_FIELDS[j]]<-tmp_data[,BENTHIC_FIELDS[j]]
			}
		}
	}
}
# now reset zeros to NAs for all records where there was NO benthic data at all
x[x$countBD==0,BENTHIC_FIELDS]<-NA

x<-merge(x, sb[,c("SPECIES", "FG")], by="SPECIES", all.x=T)

#CONG seems unlikely to be an eel, as there are three individuals by oneobserver in one year (Kara Osada, and all around 10cm or smaller). I suspect these are a species of gobi or bleenny
x[x$SPECIES %in% c("CONG"),]$TROPHIC_MONREP<-"SECONDARY"
x[x$SPECIES %in% c("CONG"),]$FG<-"OTHER"

#May or may not be a good idea to change TROPHIC_MONREP for some of these .. probably that should be done in Oracle if at all
x[x$SPECIES %in% c("BLEN"),]$TROPHIC_MONREP<-"SECONDARY"
x[x$SPECIES %in% c("ABNO"),]$TROPHIC_MONREP<-"PRIMARY"
x[x$SPECIES %in% c("SOLD"),]$TROPHIC_MONREP<-"PLANKTIVORE"

levels(x$FG)
x[x$SPECIES %in% c("ACAN", "SURG"),]$FG<-"Grazer"
x[x$SPECIES %in% c("AMSP", "BLEN", "GOBI", "FISH", "LIZA"),]$FG<-"OTHER"
x[x$SPECIES %in% c("WRAS", "PSOT", "PSSP", "THCR", "TRIG", "HAWK"),]$FG<-"Macro-invertivore"
x[x$SPECIES %in% c("PARR"),]$FG<-"Scrapers/small excavators"
x[x$SPECIES %in% c("CARD", "SOLD", "MYSP"),]$FG<-"Nocturnal planktivore"
x[x$SPECIES %in% c("ABNO", "BOXF", "ANGE", "PUFF", "FILE"),]$FG<-"Benthic omnivore"
x[x$SPECIES %in% c("GOAT"),]$FG<-"Pisci-invertivore"
x[x$SPECIES %in% c("BUTT"),]$FG<-"Corallivore"
x[x$SPECIES %in% c("CEML", "DAMS"),]$FG<-"Small Grazers"
x[x$SPECIES %in% c("DAMS") & x$COUNT>9,]$FG<-"Diurnal planktivore"
x[x$SPECIES %in% c("DAMS") & x$COUNT>9,]$TROPHIC_MONREP<-"PLANKTIVORE"

unique(x[is.na(x$FG),c("SPECIES", "TAXONNAME", "FAMILY")])
unique(x[x$TROPHIC_MONREP=="UNKNOWN",]$SPECIES)

wd<-droplevels(x)
save(wd, file="Clean PRIA wd.Rdata")  #Save clean working data



##### FILTER OUT CERTAIN SPECIES #################
wd[wd$SPECIES %in% c("DEMA", "ENPU", "MABI", "SPDE", "SECR", "KUSA"),]$COUNT<-0
##### IDW IDW IDW   CHECK FOR DOPHINS OR MAMMALS OR TURTLES ######

unique(wd[wd$COUNT>0,]$FAMILY)
wd[wd$FAMILY=="UNKNOWN",]
# no wierd families .. I guess those werent recorded at PRIA sites

wd<-droplevels(wd)

#base information about the survey - field names should match those in input file (obviously!)
UNIQUE_SURVEY<-c("SITEVISITID","METHOD")
UNIQUE_REP<-c(UNIQUE_SURVEY, "REP")
UNIQUE_COUNT<-c(UNIQUE_REP, "REPLICATEID")

#get base survey info, calculate average depth+complexity+so on
SURVEY_INFO<-c("OBS_YEAR", "REGION", "REGION_NAME", "ISLAND", "ANALYSIS_SEC", "ANALYSIS_YEAR", "ANALYSIS_STRATA", "SEC_NAME", "SITE", "DATE_", "REEF_ZONE", "DEPTH_BIN", "LATITUDE", "LONGITUDE", "SITEVISITID", "METHOD")
survey_table<-Aggregate_InputTable(wd, SURVEY_INFO)

OTHER_BENTHIC<-c("CLAM", "CORALLIMORPH", "ZOANTHID", "TUNICATE", "SPONGE")
wd$OTHER_BENTHIC<-rowSums(wd[,OTHER_BENTHIC],na.rm=T)
SURVEY_SITE_DATA<-c("DEPTH", "HARD_CORAL", "SOFT_CORAL", "MA", "CCA", "TA", "SAND", "CYANO", "OTHER_BENTHIC", "ComplexityValue", "MEAN_SH", "MEAN_SH_SD", "MAX_HEIGHT")

# Generate a data frame with all benthic and site level information for each survey
survey_est_benthos<-Calc_Site_nSurveysArea(wd, UNIQUE_SURVEY, UNIQUE_REP, UNIQUE_COUNT, SURVEY_SITE_DATA)   #Calc_Site_nSurveysArea deals better with situations where one REP has benthic data and other doesnt. 
surveys<-merge(survey_table, survey_est_benthos, by=UNIQUE_SURVEY)
save(surveys, file="TMPsurveys.Rdata")
write.csv(surveys, file="TMP PRIA surveys.csv")

wd<-droplevels(wd)
WD_SAVE<-wd

#Pull all species information into a separate df, for possible later use ..
FISH_SPECIES_FIELDS<-c("SPECIES","TAXONNAME", "FG", "FAMILY", "COMMONFAMILYALL", "TROPHIC_MONREP", "LW_A", "LW_B", "LENGTH_CONVERSION_FACTOR")
species_table<-Aggregate_InputTable(wd, FISH_SPECIES_FIELDS)
save(species_table, file="TMPspecies.Rdata")

# GENERATE SUMMARY METRICS --------------------------------------------------
r1<-Calc_Site_Bio(wd, "FG"); fg.cols<-names(r1[3:dim(r1)[2]])
r2<-Calc_Site_Bio(wd, "TROPHIC_MONREP"); tr.cols<-names(r2[3:dim(r2)[2]])

#Merge Site Data and Count Data Per Site Per Grouping Variable (e.g. Species, Tropic_MonRep, Family) 
wsdFG<-merge(surveys,r1,by=UNIQUE_SURVEY)
write.csv(wsdFG, file="CREP PRIA Data By Site_FG.csv")
save(wsdFG, file="Clean PRIA Site_FG.RData")

#Merge Site Data and Count Data Per Site Per Grouping Variable (e.g. Species, Tropic_MonRep, Family) 
wsd<-merge(surveys,r2,by=UNIQUE_SURVEY)
wsd$TotFish<-rowSums(wsd[,tr.cols])
write.csv(wsd, file="CREP PRIA Data By Site_ConsumerGroup.csv")
save(wsd, file="Clean PRIA Site_ConsumerGroup.RData")

WSD_UNCAPPED<-wsd

### DO CAPPING HERE ... 95% PERCENTILE PER ISLAND (ALL YEARS) PER CONSUMER GROUP
PERCENTILE=0.95
Calc_cap<-function(x){  
	return(quantile(x, c(PERCENTILE), na.rm = F))
} # end Calc_cap
CAPS<-aggregate(wsd[,c(tr.cols)], by=wsd[,c("REGION", "ISLAND")], FUN=Calc_cap)

means<-aggregate(wsd[,c(tr.cols)], by=wsd[,c("REGION", "ISLAND")], FUN=mean)
max<-aggregate(wsd[,c(tr.cols)], by=wsd[,c("REGION", "ISLAND")], FUN=max)
PERCENTILE=0.975
p97.5<-aggregate(wsd[,c(tr.cols)], by=wsd[,c("REGION", "ISLAND")], FUN=Calc_cap)
capping_info<-list(means=means, max_vals=max, p95_cap=CAPS, p975_cap=p97.5)
save(capping_info,file="Capping Info.Rdata")

#Show island means.max and the caps we are going to impose
capping_info


#NOW cap values in wsd
islands<-unique(wsd$ISLAND)
c_groups<-unique(wd$TROPHIC_MONREP)
for(i in islands){
	for(j in c_groups){
		cap<-CAPS[CAPS$ISLAND==i, j]
		wsd[wsd$ISLAND==i & wsd[,j]>cap, j]<-cap
	}
}
WSD_CAPPED<-wsd
save(wsd, file="Capped95% PRIA Site_ConsumerGroup.RData")

# OK .. we have capped and uncapped wsd values now ... ready to go ahead and generate island/year values


data.cols<-c("TotFish", tr.cols, "DEPTH", "HARD_CORAL")

####################################################################################################################################################################
#
#     CHECK FOR SITUATIONS IN WHICH WE DONT HAVE ENOUGH DATA WITHIN A SECTOR (i.e. FEWER THAN 2 REPS) AND MANUALLY DEAL WITH THAT BY POOLING SECTORS
#
####################################################################################################################################################################
## Nearly always DOING THIS ONLY WITH nSPC data ####
wsd<-subset(wsd, wsd$METHOD=="nSPC")
wsd<-droplevels(wsd)

## check wwhether we have ISLANDS that arent in the sectors file
setdiff(unique(wsd$ISLAND),unique(sectors$ISLAND))

WSD_SAVED<-wsd
SECTORS_SAVED<-sectors
####################################################################################################################################################################
#
#     SET THE ANALYIS SCHEME (ie WHICH SECTORS ARE WE USING THIS TIME .. IS IT THE BASIC ONES, OR THE ONES THAT WE USED FOR GUAMM2011 SURVEYS OR WHATEVER)
#
#
#     BE AWARE THAT THIS NEXT STEP REQUIRES SOME MANUAL FIDDLING.. WHEN YOU RUN THE CODE YOU MUST DECIDE ON THE APPROPRAITE STRATIFICATION SCHEME. IN SEVERAL CASES IT WILL
#        BE NECESSARY TO RUN SEVERAL SCHEMES (eg MARIANA 2011, then MARIANA 2014, etc...) AND THEN MANUALLY PUT THE DATA TOGETHER INTO A MASTER OUTPUT (eg run all with RAMP_BASIC, 
#        then run just Guam 2011 with MARIAN2011, then run Guam2014 with MARIAN2014, and then pool the various data files (eg by cutting and pasting from MAR2011 output into the master etc..)
#
###################################################################################################################################################################
# COME BACK HERE TO RE RUN FOR EACH ANALYSIS SCHEME
SPATIAL_POOLING_BASE<-c("REGION","ISLAND","ANALYSIS_SEC","ANALYSIS_STRATA", "REEF_ZONE")    
POOLING_LEVEL<-c(SPATIAL_POOLING_BASE, "ANALYSIS_YEAR")

# THIS IS A CRITICAL STEP - SET THE ANALYSIS SCHEME HERE .. ALL STEPS BELOW WILL WORK OFF THIS SCHEME (THIS IS HOW ISLANDS ARE BROKEN DOWN INTO SECTORS #####
CURRENT_SCHEME<-"RAMP_BASIC"
	
sectors$ANALYSIS_SEC<-sectors[,CURRENT_SCHEME]
# DETERMINE THE BASIC STRATIFICATION WITHIN SECTORS - DEFAULT IS REEF_ZONE AND DEPTH_BIN, BUT THIS CODE ALLOWS PSSIBILITY OF CHOOSING ANOTHER
sectors$ANALYSIS_STRATA<-paste(sectors$REEF_ZONE, sectors$DEPTH_BIN, sep='')

##DETERMINE WHICH SITES HAVE ANALYSIS STRATA THAT ARE NOT IN THIS 
analysis_secs<-unique(wsd$ANALYSIS_SEC)
missing_secs<-unique(analysis_secs[!analysis_secs %in% unique(sectors$ANALYSIS_SEC)])
if(length(missing_secs)>0) {
	cat("ANALYSIS SECTORS missing from this scheme:", missing_secs)
}
tmp<-aggregate(wsd$DEPTH,by=wsd[,c("REGION", "ISLAND", "ANALYSIS_YEAR", "ANALYSIS_SEC")], sum, na.rm=FALSE)  
tmp[tmp$ANALYSIS_SEC %in% missing_secs,]

### CHECK REPLICATION WITHIN STRATA
tmp<-aggregate(wsd[,"METHOD"], by=wsd[,c(POOLING_LEVEL ,"SITE")], length)
tmp<-aggregate(tmp[,"x"], by=tmp[,c(POOLING_LEVEL)], length)
tmp<-merge(sectors, tmp[,c("ANALYSIS_YEAR", "ANALYSIS_SEC", "ANALYSIS_STRATA","x")],by=c("ANALYSIS_SEC", "ANALYSIS_STRATA"),all.y=TRUE)
names(tmp)[names(tmp)=="x"]<-"n_sites"
a<-cast(tmp, ANALYSIS_YEAR + REGION + ISLAND + ANALYSIS_SEC ~ ANALYSIS_STRATA, value="n_sites", sum, fill=NA)
a

#DROPPING ANYTHING THAT ISNT FOREREEF
wsd<-subset(wsd, wsd$REEF_ZONE %in% c("Forereef"))
wsd<-droplevels(wsd)

#IDW .. STRONGLY THINK WE SHOULD CHECK CLASSIFICATION OF THE DIFFERENT PRIA REEF ZONES ... MAKE SURE THAT THEY ARE CONCISTENTLY DONE .. DEALW ITH KINGMAN AND JOHNSTON
#IDW .. STRONGLY THINK WE SHOULD CHECK CLASSIFICATION OF THE DIFFERENT PRIA REEF ZONES ... MAKE SURE THAT THEY ARE CONCISTENTLY DONE .. DEALW ITH KINGMAN AND JOHNSTON
#IDW .. STRONGLY THINK WE SHOULD CHECK CLASSIFICATION OF THE DIFFERENT PRIA REEF ZONES ... MAKE SURE THAT THEY ARE CONCISTENTLY DONE .. DEALW ITH KINGMAN AND JOHNSTON
#IDW .. STRONGLY THINK WE SHOULD CHECK CLASSIFICATION OF THE DIFFERENT PRIA REEF ZONES ... MAKE SURE THAT THEY ARE CONCISTENTLY DONE .. DEALW ITH KINGMAN AND JOHNSTON
#IDW .. STRONGLY THINK WE SHOULD CHECK CLASSIFICATION OF THE DIFFERENT PRIA REEF ZONES ... MAKE SURE THAT THEY ARE CONCISTENTLY DONE .. DEALW ITH KINGMAN AND JOHNSTON
#IDW .. STRONGLY THINK WE SHOULD CHECK CLASSIFICATION OF THE DIFFERENT PRIA REEF ZONES ... MAKE SURE THAT THEY ARE CONCISTENTLY DONE .. DEALW ITH KINGMAN AND JOHNSTON

# Monitoring Report OUTPUT sites per years (appendix 3) -------------------------------------
#save(a, file=paste(CURRENT_SCHEME, "MRsites_year_reef_zone_depth_bin.rdata")) ## use this for table in appendix 3 - see appendices R file

#clean up the sectors table so pool all sub sectors within a scheme into a total for this scheme's sectors
sectors<-aggregate(sectors[,"AREA_HA"], by=sectors[,c(SPATIAL_POOLING_BASE)], sum)
names(sectors)[names(sectors)=="x"]<-"AREA_HA"

#################################################################################################################################
############################################# NOW DO THE CALCAULTION OF WITHIN-STRATA AND POOLED UP DATA VALUES #################
#################################################################################################################################

ADDITIONAL_POOLING_BY<-c("ANALYSIS_YEAR", "METHOD")                                    # additional fields that we want to break data at, but which do not relate to physical areas (eg survey year or method)
#generate within strata means and vars
POOLING_LEVEL<-c(SPATIAL_POOLING_BASE, ADDITIONAL_POOLING_BY)
data.per.strata<-Calc_PerStrata(wsd, data.cols, POOLING_LEVEL)
write.csv(data.per.strata,"tmp strata data.csv")
#save(data.per.strata, file=paste(CURRENT_SCHEME, "strata_data.rdata", sep=""))

###### REMOVE STRATA with N=1 (cannot pool those up)
data.per.strata$Mean<-data.per.strata$Mean[data.per.strata$Mean$N>1,]
data.per.strata$SampleVar<-data.per.strata$SampleVar[data.per.strata$SampleVar$N>1,]
data.per.strata$SampleSE<-data.per.strata$SampleSE[data.per.strata$SampleSE$N>1,]

### SAVE THE DATA AT WHATEVER LEVEL YOU WANT ... SOME EXAMPLES BELOW..

# e.g. SAVE BY ISLAND PER YEAR
AGGREGATION_LEVEL<-c("REGION","ISLAND")       # Spatial Level to agggregate output data to (eg per REGION or per (REGION, ISLAND) etc... 
dp<-Calc_Pooled(data.per.strata$Mean, data.per.strata$SampleVar, data.cols, AGGREGATION_LEVEL, ADDITIONAL_POOLING_BY, SPATIAL_POOLING_BASE, sectors)
write.csv(dp,file="data_pooled_is_yr.csv")
save(dp, file="data_pooled_is_yr.rdata")


#################################################################################################################################
############################################# REDO WITH UNCAPPED DATA #################
#################################################################################################################################
wsd<-WSD_UNCAPPED
wsd<-subset(wsd, wsd$REEF_ZONE %in% c("Forereef"))

data.per.strata<-Calc_PerStrata(wsd, data.cols, POOLING_LEVEL)
write.csv(data.per.strata,"tmp strata data UNCAPPED.csv")
#save(data.per.strata, file=paste(CURRENT_SCHEME, "strata_data.rdata", sep=""))

###### REMOVE STRATA with N=1 (cannot pool those up)
data.per.strata$Mean<-data.per.strata$Mean[data.per.strata$Mean$N>1,]
data.per.strata$SampleVar<-data.per.strata$SampleVar[data.per.strata$SampleVar$N>1,]
data.per.strata$SampleSE<-data.per.strata$SampleSE[data.per.strata$SampleSE$N>1,]

### SAVE THE DATA AT ISLAND PER YEAR
AGGREGATION_LEVEL<-c("REGION","ISLAND")       # Spatial Level to agggregate output data to (eg per REGION or per (REGION, ISLAND) etc... 
dp<-Calc_Pooled(data.per.strata$Mean, data.per.strata$SampleVar, data.cols, AGGREGATION_LEVEL, ADDITIONAL_POOLING_BY, SPATIAL_POOLING_BASE, sectors)
write.csv(dp,file="data_pooled_is_yr UNCAPPED.csv")
save(dp, file="data_pooled_is_yr UNCAPPED.rdata")




