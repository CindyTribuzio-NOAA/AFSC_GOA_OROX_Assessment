# Title: GOA Other Rockfish Assessment ----
# The below documents the steps in the process for conducting the GOA OROX stock assessment
# Updated: Oct 5 2021
# Recent Author: Cindy Tribuzio

# Setup ----
libs <- c("tidyverse", "janitor", "Hmisc", "RColorBrewer", "gridExtra", "gtable", "grid", "flextable", "officer")
if(length(libs[which(libs %in% rownames(installed.packages()) == FALSE )]) > 0) {
  install.packages(libs[which(libs %in% rownames(installed.packages()) == FALSE)])}
lapply(libs, library, character.only = TRUE)
'%nin%'<-Negate('%in%') #this is a handy function

# Create Directories ----
SYR <- 2021 #survey year
AYR <- 2021 #assessment year
endyr <- 2021 #for RFX model end year

dir.create(paste(getwd(),"/Data/Annual_updates/",AYR,sep=""), showWarnings = T)
dir.create(paste(getwd(),"/Output/",AYR,sep=""), showWarnings = T)
dir.create(paste(getwd(),"/Documents/",AYR,sep=""), showWarnings = T)

# Survey Data Collection ----
# these steps need to be automated for future assessments

#1) RACE_CATCHYYYY.csv
#2) RACE_HAULYYYY.csv
#3) RACE_SURVEYSYYYY.csv
#4) RACE_STRATUMYYYY.csv
#5) RACE_GOAOROX_Ages.csv
#6) RACE_GOAOROX_Lengths.csv

#1) Standard AKFIN query: RACE Survey -> Catch by Haul. All Years, all surveys, all species.
#Run each year to ensure any updates to historical data are included (should be rare). Download .csv to Data ->
# Annual_updates -> Assessment year. Downloads as race_catch_by_haul.zip, unpack and rename. 

#2) Standard AKFIN query: RACE Survey -> Haul Descriptions. All Years, all surveys, all areas.
#Run each year to ensure any updates to historical data are included (should be rare). Download .csv to Data ->
# Annual_updates -> Assessment year. Direct download for this file, no zip. Need to open and Save As for file because it's not a true .csv 
#(not sure why it downloads that way). Just do a save as .csv and it will work fine in R.

#3) Standard AKFIN query: RACE Survey -> Survey/Cruise Description. All Years, all surveys.
#Run each year to ensure any updates to historical data are included (should be rare). Download .csv to Data ->
# Annual_updates -> Assessment year. Direct download for this file, no zip. Need to open and Save As for file because it's not a true .csv 
#(not sure why it downloads that way). Just do a save as .csv and it will work fine in R.

#4) Standard AKFIN query: RACE Survey -> Stratum Descriptions. All Years, all surveys, all strata.
#Run each year to ensure any updates to historical data are included (should be rare). Download .csv to Data ->
# Annual_updates -> Assessment year. Direct download for this file, no zip. Need to open and Save As for file because it's not a true .csv 
#(not sure why it downloads that way). Just do a save as .csv and it will work fine in R.

#5) Standard AKFIN query: RACE Survey -> GOA - Age Compositions Total. Query for all species, all years.
#These data were not included in the assessment starting in 2021, due to some discrepancies between the AGP database and RACEBASE
#suggesting that RACEBASE Age comps were based on multiple sources beyond just the GOA trawl survey

#6) Standard AKFIN query: RACE Survey -> Size Composition by Haul for all OROX species, all years, GOA survey

# Fishery Data Collection ----
# these steps need to be automated for future assessments
#  Files needed:
#1) NORPAC_OROX_species_catch.csv
#2) CAS_OROX_species_catch.csv
#3) CAS_OROX_group_catch.csv
#4) CAS_GFtotfishery_confidential.csv


#1) Query built in AKFIN: Catalog>Shared Folders>Stock Assessment>NORPAC_OROX_species_catch>open>edit
#Here you will need to edit the filters (criteria tab) to add the recent years, add current year
#to first and last two filters. Don't forget to "save" the changes. Run query (i.e., got to results tab) 
#	and export data. To export, go to second icon which is "export this analysis">DATA>CSV format.
#	Make sure to double check with previous years data to make sure nothing has changed or should 
#	be changed. Need to open and Save As for file because it's not a true .csv 
#(not sure why it downloads that way). Just do a save as .csv and it will work fine in R.

#2) Query built in AKFIN: Catalog>Shared Folders>Stock Assessment>CAS_OROX_species_catch>open>export.
#This one shouldn't require editing. Run query (i.e., got to results tab) and export data. 
#	To export, go to second icon which is "export this analysis">DATA>CSV format. Make sure to double check with 
#	previous years data to make sure nothing has changed, or should be changed. Need to open and 
#	Save As for file because it's not a true .csv (not sure why it downloads that way). 
#Just do a save as .csv and it will work fine in R.

#3) AKFIN standard query: Dashboards>Stock Assessment>Catch Data>Groundfish Total Discards:
#  Year: 1991-present
#FMP Area: GOA
#Species Group: GOA Pelagic shelf Rockfish, Other Rockfish, Other Slope Rockfish, 
#Pelagic shelf rockfish, Slope Rockfish (note that starting in 2003, CAS called 
#                                        "other slope rockfish" "other rockfish") 
#Need to open and Save As for file because it's not a true .csv (not sure why it downloads that way). Just do a save as .csv 
#	and it will work fine in R.

#4) AKFIN standard query: Dashboards>Stock Assessment>Catch Data>Groundfish Total Catch By Fishery
# Year = 2010 - present
# FMP Area = GOA
# Species Group = GOA Pelagis Shelf Rockfish, Other Rockfish, Other Slope Rockfish, Pelagic Shelf Rockfish, Slope Rockfish

# Biomass Estimates ----
#We estimate a designed based estimate of biomass and variance for:
#1) Each M-group (n = 5)
#2) Each Tier (n=2)
#3) Both Tiers 4 and 5 combined
# There are a few other groupings of species run for references, but not used in the specifications (e.g., DSR)
# NOTE: Biomass estimates by group are created for each survey, not needed for this assessment, but nice to look at

#Open and run RACE_GOA_OROX_biomass.R. Only need to update the bgroups if the M-groups or tiers of the species
#changes between assessments or to test alternative groups

source(paste(getwd(),"/Code/RACE_Biomass/RACE_GOA_OROX_biomass.R",sep=""))

#quick comparison to previous estimates to look for errors
Biom_Current <- read_csv(paste(getwd(),"/Output/",AYR,"/RACE_Biomass/RACE_Biomass_GOA_OROX.csv",sep=""))
Biom_Current$Version <- AYR

PYR <- AYR-1 #this value depends on when last biomass was run
Biom_Previous <- read_csv(paste(getwd(),"/Output/",PYR,"/RACE_Biomass/RACE_Biomass_GOA_OROX.csv",sep=""))
Biom_Previous$Version <- PYR

Biom_dat <- Biom_Current %>% 
  bind_rows(Biom_Previous) %>% 
  filter(SURVEY == "GOA")

ggplot(Biom_dat, aes(x=YEAR,y=Biomass,color=as.factor(Version), Shape = as.factor(Version)))+
  geom_point()+
  facet_grid(Group~REGULATORY_AREA_NAME)
#dots should exactly overlap, with the exception of the current assessment year

# Random Effects ----
#Run RFX on each of the M-groups
#1) Each M-group (n = 5)
#2) Each Tier (n=2)
#3) Both Tiers 4 and 5 combined
# Code also runs some other species groupings for reference
# NOTE: this runs for all surveys which is not needed for assessment but nice to look at

#Open and run RFX_GOA_OROX.R. Only need to update if more biomass groups are available

source(paste(getwd(),"/Code/RFX/RFX_GOA_OROX.R",sep=""))

# Catch Updates ----
# have to re-estimate catch by species 2003 - 2010
# catch by species provided by CAS 2010 - present
# NOTE: there are some minor things that need to be updated within the code, make sure to look at code

source(paste(getwd(),"/Code/Catch/OROX_catch_hist.R",sep=""))


# Harvest Specifications ----
# Calculates OFL/ABC and apportionment

source(paste(getwd(),"/Code/Harvest_specs/OROX_ABCOFL_Apport.R",sep=""))

# Changes in ABCs
# GOA wide
spec_hist <- read_csv(paste(getwd(),"/Data/OROX_specs_history.csv",sep=""))

# the below will throw an error if you run the harvest spec code more than once, because the AYR+1 will be repeated
spec_change <- spec_hist %>% 
  filter(Year == AYR | Year == AYR+1) %>% 
  select(Year, ABC, OFL) %>% 
  pivot_longer(!Year, names_to = "Spec", values_to = "MT") %>% 
  pivot_wider(names_from = Year, values_from = MT) %>% 
  rename(L_Assess = 2, Curr_Assess = 3) %>% 
  mutate(Pchange = paste(round(((Curr_Assess - L_Assess)/L_Assess)*100,0),"%",sep=""))

# by fmp_subarea


# Figures Code ----
# all of the figs for the document and presentatin
# NOTE: there are some minor things that need to be updated within the code, make sure to look at code

source(paste(getwd(),"/Code/Figures/OROX_figures.R",sep=""))

# Tables Code ----
# only has Exec Summary tables at this time, need to add the rest
# need to make sure to round catch at the lower resolution to ensure all tables add up the same way, done by hand in 2021
# 

source(paste(getwd(),"/Code/Tables/GOAOROX_Tables.R",sep=""))

# SARA Code ----
