# Directory Setup ----
SARAdir <- paste(getwd(),"/Output/", AYR, "/SARA", sep="")
dir.create(SARAdir)

# Static SARA info ----
Species <- "OROCK"
FMP <- "GOA"
Tier <- "4/5/6"
Update <- "full_update"
Flim <- "Maximum_catch"
Abund <- "random_effects_model"
Snames <- "GOA_trawl_biomass_mtons"

# Updated output ----
RFX_dat <- read_csv(paste(getwd(),"/Output/",AYR,"/RFX/RFX_Biomass_GOA_OROX.csv",sep="")) %>% 
  filter(REGULATORY_AREA_NAME == "GOA",
         Group == "OROX_all")

RFX_AYR <- RFX_dat %>% 
  filter(YEAR == max(YEAR))

OROXbiomass_dat <- read_csv(paste(getwd(),"/Output/", AYR, "/RACE_Biomass/RACE_Biomass_GOA_OROX.csv",sep="")) %>% 
  filter(SURVEY == "GOA",
         REGULATORY_AREA_NAME == "GOA",
         Group == "OROX_all") %>% 
  select(YEAR, Biomass)

catchdat<-read_csv(paste(getwd(),"/Output/", AYR, "/Catch/OROXcomplex_catch",AYR,"_confidential.csv",sep="")) %>% 
  group_by(year) %>% 
  summarise(OROX_catch = sum(tot_catch))


# Compile the .dat file ----
cat(Species, "#STOCK","\n",
    FMP, "#REGION","\n",
    AYR, "#ASSESSYEAR", "\n",
    Tier, "#TIER", "\n",
    Update, "#UPDATE", "\n",
    Flim, "#FLIMIT", "\n",
    RFX_AYR$Biom_est, "#BESTB", "\n",
    RFX_AYR$Biom_LL, "MINB", "\n",
    RFX_AYR$Biom_UL, "MAXB", "\n",
    Abund, "#ABUNDMETH", "\n",
    "#STOCKNOTES", "\n",
    "#TOTALCATCH", "\n",
    1, "\n",
    catchdat$year, "\n",
    round(catchdat$OROX_catch), "\n",
    "#ABUNDANCE","\n",
    1,"\n",
    RFX_dat$YEAR, "\n",
    RFX_dat$Biom_est, "\n",
    "#SURVEYNAMES", Snames, "\n",
    1, "\n",
    "#SURVEY1","\n",
    OROXbiomass_dat$YEAR, "\n",
    round(OROXbiomass_dat$Biomass,0), "\n",
    "#SURVEY2","\n",
    "#SURVEY3","\n",
    "#SURVEY4","\n",
    sep=" ",file=paste(SARAdir,"/OROCKGOA",AYR,".dat",sep=""))
