specdir <- paste(getwd(),"/Output/", AYR, "/Harvest_Specs", sep="")
dir.create(specdir)

#Tier 4/5 DATA ----

RFXB<-read_csv(paste(getwd(),"/Output/",AYR,"/RFX/RFX_Biomass_GOA_OROX.csv",sep=""))

#Weighted M for Tier 5 ----
WTMend <- RFXB %>% 
  filter(grepl("M0", Group),
         REGULATORY_AREA_NAME == "GOA") %>% 
  mutate(M = if_else(Group == "OROX_M01", 0.1,
                     if_else(Group == "OROX_M0092", 0.092,
                             if_else(Group == "OROX_M007", 0.07,
                                     if_else(Group == "OROX_M006", 0.06, 0.05))))) %>% 
  group_by(YEAR) %>% 
  summarise(WTM = crossprod(Biom_est,M)/sum(Biom_est))
write.csv(WTMend, paste(specdir,"/OROX_Wtd_M.csv",sep=""),row.names = F)

WTM<-WTMend %>% 
  filter(YEAR == AYR) %>% 
  select(WTM)

#Harvest specs ----

#Sharpchin, FABC and FOFL from Tier 4 methods (need to find files for that)
FABC<-0.065
FOFL<-0.079
SC_specs <- RFXB %>% 
  filter(YEAR == endyr,
         Group == "OROX_SC",
         REGULATORY_AREA_NAME == "GOA") %>% 
  mutate(ABC = round(Biom_est*FABC, 0),
         OFL = round(Biom_est*FOFL, 0),
         Group = "SC",
         Apport = 1) %>% 
  select(REGULATORY_AREA_NAME, Biom_est, Apport, ABC, OFL, Group)

#Tier 5 species
T5FOFL<-round(as.numeric(WTM),3)
T5FABC<-0.75*T5FOFL
T5_specs <- RFXB %>% 
  filter(YEAR == endyr,
         Group == "OROX_allnoSC",
         REGULATORY_AREA_NAME == "GOA") %>% 
  mutate(ABC = round(Biom_est*T5FABC, 0),
         OFL = round(Biom_est*T5FOFL, 0),
         Group = "T5",
         Apport = 1) %>% 
  select(REGULATORY_AREA_NAME, Biom_est, Apport, ABC, OFL, Group)

#OROX specs - separated the steps so Tier 5 could be viewed as desired
T45_specs <- bind_rows(SC_specs,T5_specs)

# Apportionment ----

#Sharpchin
SCapp <- RFXB %>% 
  filter(YEAR == endyr, Group == "OROX_SC", 
         grepl(" GOA", REGULATORY_AREA_NAME)) %>% 
  select(REGULATORY_AREA_NAME, Biom_est) %>% 
  mutate(Apport = Biom_est/sum(Biom_est),
         Group = "SC")

#Tier 5
T5app <- RFXB %>% 
  filter(YEAR == endyr, Group == "OROX_allnoSC", 
         grepl(" GOA", REGULATORY_AREA_NAME)) %>% 
  select(REGULATORY_AREA_NAME, Biom_est) %>% 
  mutate(Apport = Biom_est/sum(Biom_est),
         Group = "T5")

#Apportioned ABC

#Sharpchin
SCAppABC <- SCapp %>% 
  mutate(ABC = round(Apport*as.numeric(SC_specs$ABC), 0),
         Group = "SC")

#Tier 5 species
T5AppABC <- T5app %>% 
  mutate(ABC = round(Apport*as.numeric(T5_specs$ABC), 0),
         Group = "T5")

T45_AppABC <- rbind(SCAppABC,T5AppABC)

#E Goa split fraction ----
split_vals <- read_csv(paste(getwd(),"/Data/Annual_updates/",AYR,"/Biomass Fractions in Eastern GOA.csv", sep=""))
split_vals <- split_vals %>% 
  clean_names() %>% 
  select(!c(record_count,loaded_to_repository)) %>% 
  mutate(management_group = if_else(management_group == "OROXT4", "SC", "T5")) %>% 
  rename(Group = management_group)

T4_split <- split_vals %>% 
  filter(Group == "SC")

T45_split <- split_vals %>% 
  filter(Group != "SC") %>% 
  mutate(Group = "T5") %>% 
  group_by(year, Group) %>% 
  summarise_all(funs(sum)) %>% 
  mutate(eastern_fraction = eastern_biomass_mt/total_biomass_mt,
         western_fraction = western_biomass_mt/total_biomass_mt) %>% 
  bind_rows(T4_split)

#E Goa split fraction, with annual 4:6:9 weighting
splits <- T45_split %>% 
  filter(year > 2010) %>% 
  select(year, Group, eastern_fraction) %>% 
  #arrange(management_group, year) %>% 
  group_by(Group) %>% 
  mutate(Wt6 = lag(eastern_fraction, order_by = Group, n=1),
         Wt4 = lag(Wt6)) %>% 
  filter(year >2014) %>% 
  mutate(Wt_EY_frac = (Wt4*4+Wt6*6+eastern_fraction*9)/19) %>% 
  rename(Year = year) %>% 
  filter(Year == 2021) %>% 
  select(Year, Group, Wt_EY_frac)

#need to split the EGOA ABCs and calc the proportion of the total ABC taht each is
T45_EY_specs <- T45_AppABC %>% 
  filter(REGULATORY_AREA_NAME == "EASTERN GOA") %>% 
  left_join(splits) %>% 
  mutate(EYABC = round(ABC*Wt_EY_frac, 0),
         EYBiom = Biom_est*Wt_EY_frac,
         EYfracbig = Apport*Wt_EY_frac,
         REGULATORY_AREA_NAME = "SOUTHEAST") %>% 
  select(REGULATORY_AREA_NAME, EYBiom, EYABC, EYfracbig, Group) %>% 
  rename(Biom_est = EYBiom,
         ABC = EYABC,
         Apport = EYfracbig)

T45_EGOA_specs <- T45_AppABC %>% 
  filter(REGULATORY_AREA_NAME == "EASTERN GOA") %>% 
  left_join(splits) %>% 
  mutate(WYABC = round(ABC*(1-Wt_EY_frac), 0),
         WYBiom = Biom_est*(1-Wt_EY_frac),
         WYfracbig = Apport*(1-Wt_EY_frac),
         REGULATORY_AREA_NAME = "WYAK") %>% 
  select(REGULATORY_AREA_NAME, WYBiom, WYABC, WYfracbig, Group) %>% 
  rename(Biom_est = WYBiom,
         ABC = WYABC,
         Apport = WYfracbig) %>% 
  bind_rows(T45_EY_specs)

#Tier 4 and 5 summary table ----
OFL <-T45_specs %>% select(Group, OFL)
T45_GOA_specs <- T45_AppABC %>% 
  bind_rows(T45_EGOA_specs) %>% 
  left_join(OFL) %>% 
  bind_rows(T45_specs) %>% 
  mutate(ABC = round(ABC,0),
         OFL = round(OFL,0))


#Tier 6 ----
#DSR species are only outside of EY/se, ABC by area
#aurora and shortbelly (however, shortbelly catch only occured prior to 2013)
T6byr <- 2013 #beginning year for Tier 6 calcs
T6eyr <- 2016 #end year for Tier 6 calcs

OROX_catch <- read_csv(paste(getwd(),"/Output/",AYR,"/Catch/OROXcomplex_catch",AYR,"_confidential.csv",sep=""))
OROX_specs_species <- OROX_catch %>% 
  filter(fmp_subarea %nin% c("SEI","PWSI"),
         year >= T6byr & year <= T6eyr,
         NORPAC_name %in% c("CANARY ROCKFISH","CHINA ROCKFISH","QUILLBACK ROCKFISH","ROSETHORN ROCKFISH",
                            "TIGER ROCKFISH","YELLOWEYE ROCKFISH","COPPER ROCKFISH", "AURORA ROCKFISH",
                            "SHORTBELLY ROCKFISH")) %>% 
  group_by(NORPAC_name, fmp_subarea) %>% 
  summarise(maxCatch = max(tot_catch, na.rm=T),
            OFL = round(maxCatch,0),
            ABC = round(OFL*0.75, 0))
write.csv(OROX_specs_species, paste(specdir, "/T6_species_specs.csv",sep=""), row.names = F)

OROX_specs <- OROX_specs_species %>% 
  group_by() %>% 
  summarise(OROX_OFL = sum(OFL),
            OROX_ABC = sum(ABC)) %>% 
  mutate(REGULATORY_AREA_NAME = "GOA",
         Biom_est = 0, 
         Apport = 0,
         Group = "T6") %>% 
  rename(OFL = OROX_OFL,
         ABC = OROX_ABC) %>% 
  select(REGULATORY_AREA_NAME, Biom_est, Apport, Group, ABC, OFL)

OROX_T6_EGOA <- OROX_specs_species %>% 
  filter(fmp_subarea == "WY" | fmp_subarea == "SE") %>% 
  group_by() %>% 
  summarise(area_ABC = round(sum(ABC),0)) %>% 
  bind_cols(OROX_specs$OFL) %>% 
  mutate(Biom_est = 0,
         Apport = 0, 
         Group = "T6",
         REGULATORY_AREA_NAME = "EASTERN GOA") %>% 
  rename(OFL = ...2,
         ABC = area_ABC) %>% 
  select(REGULATORY_AREA_NAME, Biom_est, Apport, Group, ABC, OFL)

OROX_specs_area <- OROX_specs_species %>% 
  group_by(fmp_subarea) %>% 
  summarise(area_ABC = round(sum(ABC),0)) %>% 
  bind_cols(OROX_specs$OFL) %>% 
  mutate(Biom_est = 0,
         Apport = 0, 
         Group = "T6",
         REGULATORY_AREA_NAME = ifelse(fmp_subarea == "CG", "CENTRAL GOA",
                                       ifelse(fmp_subarea == "WG", "WESTERN GOA",
                                              ifelse(fmp_subarea == "WY", "WYAK", "SOUTHEAST")))) %>% 
  rename(OFL = ...3,
         ABC = area_ABC) %>% 
  select(REGULATORY_AREA_NAME, Biom_est, Apport, Group, ABC, OFL) %>% 
  bind_rows(OROX_specs, OROX_T6_EGOA)
  
#Summary table for whole complex ----
OROX_recs <- bind_rows(OROX_specs_area, T45_GOA_specs)

Area_specs <- OROX_recs %>% 
  filter(REGULATORY_AREA_NAME != "GOA") %>% # & REGULATORY_AREA_NAME != "WY" & REGULATORY_AREA_NAME != "EYSEO"
  group_by(REGULATORY_AREA_NAME) %>% 
  summarise(tot_OFL = sum(unique(OFL)),
            tot_ABC = sum(ABC)) %>% 
  mutate(Biom_est = 0,
         Apport = 0, 
         Group = "OROX") %>% 
  rename(OFL = tot_OFL,
         ABC = tot_ABC) %>% 
  select(REGULATORY_AREA_NAME, Biom_est, Apport, Group, ABC, OFL)

GOA_specs <- OROX_recs %>% 
  filter(REGULATORY_AREA_NAME != "GOA" & REGULATORY_AREA_NAME != "EASTERN GOA") %>% 
  group_by() %>% 
  summarise(tot_OFL = sum(unique(OFL)),
            tot_ABC = sum(ABC)) %>% 
  mutate(Biom_est = 0,
         Apport = 0, 
         Group = "OROX",
         REGULATORY_AREA_NAME = "GOA") %>% 
  rename(OFL = tot_OFL,
         ABC = tot_ABC) %>% 
  select(REGULATORY_AREA_NAME, Biom_est, Apport, Group, ABC, OFL) %>% 
  bind_rows(OROX_recs, Area_specs)

write.csv(GOA_specs, paste(specdir, "/OROX_harvestrecs_",AYR,".csv",sep=""),row.names=F)

# Error Checks ----
# Check to make sure there aren't any rounding errors


# add apportionment values to historical tables
# add ABCs to historical tables
# there are some glitches in this to fix next year

#GOA summary
AYR_specs <- read_csv(paste(getwd(),"/Output/",AYR,"/Harvest_specs/OROX_harvestrecs_",AYR,".csv",sep=""))
AYR_specs <- AYR_specs %>% 
  filter(Group == "OROX",
         REGULATORY_AREA_NAME == "GOA") %>% 
  mutate(Year = AYR+1,
         TAC = NA, #will have to fill this in next time around, or once it's set
         Management_Group = "Other Rockfish - includes widow and yellowtail") %>%  #this will change if species get reorganized/DSR subgroup movesout
  select(Year, TAC, ABC, OFL, Management_Group)

spec_hist <- read_csv(paste(getwd(),"/Data/OROX_specs_history.csv",sep=""))
spec_hist <- spec_hist %>% 
  bind_rows(AYR_specs)

write.csv(spec_hist, paste(getwd(),"/Data/OROX_specs_history.csv",sep=""), row.names = F)

#fmp summary
WC_specs <- GOA_specs %>% 
  filter(REGULATORY_AREA_NAME %in% c("CENTRAL GOA", "WESTERN GOA"),
         Group == "OROX") %>% 
  group_by(Group) %>% 
  summarise(WC_ABC = sum(ABC),
            WC_OFL = mean(OFL)) %>% 
  mutate(REGULATORY_AREA_NAME = "WEST_CENTRAL",
         Group = "OROX",
         Biom_est = 0,
         Apport = 0) %>% 
  rename(ABC = WC_ABC,
         OFL = WC_OFL)

fmp_specs <- GOA_specs %>% 
  filter(REGULATORY_AREA_NAME %in% c("WYAK", "SOUTHEAST"),
         Group == "OROX") %>% 
  bind_rows(WC_specs) %>% 
  rename(fmp_subarea = REGULATORY_AREA_NAME) %>% 
  mutate(year = AYR+1) %>% 
  select(year, fmp_subarea, ABC)

fmpsub_specs_hist <- read_csv(paste(getwd(),"/Data/OROX_ABC_App_historical.csv",sep=""))

fmpsub_specs_hist <- fmpsub_specs_hist %>% 
  bind_rows(fmp_specs)

write.csv(fmpsub_specs_hist, paste(getwd(),"/Data/OROX_ABC_App_historical.csv",sep=""), row.names = F)


