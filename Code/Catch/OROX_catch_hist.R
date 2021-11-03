# For years prior to 2010, catch by species is extrapolated based on the observed (NORPAC) species ratios
# applied to the complex catch in those years. For years after 2010, species specific catch is available.
# catch estimates prior to 2003 are fixed and can be found in AFSC_GOA_OROX_Assessment/Data/Static/OROX_catch_91_02.csv
# methods for pre-2003 catch are in AFSC_GOA_OROX_Assessment/Data/Static/OROX_pre2003.xlsx

#setup ----
catchdir <- paste(getwd(),"/Output/", AYR, "/Catch", sep="")
dir.create(catchdir)
datadir <- paste(getwd(),"/Data/Annual_updates/", AYR, sep="")

# Bring in Data ----
##NOTE: may have to open and do "save as .csv" to get these files in the right format, they download from AKFIN as a .txt, even though it says .csv
NORPAC_dat<-read_csv(paste(datadir,"/NORPAC_OROX_species_catch_",AYR,"_confidential.csv",sep="")) #from self built query in AKFIN
NORPAC_dat <- NORPAC_dat %>% 
  clean_names()
colnames(NORPAC_dat)
#[1] "haul_year"               "haul_fmp_area"           "haul_fmp_subarea"        "haul_fmp_gear"           "sample_species_name"     "sample_species_code"    
#[7] "sum_extrapolated_weight"

CAS_spec_dat<-read_csv( paste(datadir,"/CAS_OROX_species_catch_",AYR,"_confidential.csv",sep=""))#from self built query in AKFIN
CAS_spec_dat <-  CAS_spec_dat %>% 
  clean_names()
colnames(CAS_spec_dat)
#[1] "year"                "week_number"         "fmp_area"            "zone"                "fmp_subarea"         "fmp_gear"            "species_common_name" "species_group_name" 
#[9] "species_code"        "weight_posted_sum"   "retained_discarded"  "trip_target_name"   

CAS_group_dat<-read_csv( paste(datadir,"/CAS_OROX_group_catch_",AYR,"_confidential.csv",sep=""))#from Groundfish Total Discards table in AKFIN
CAS_group_dat <- CAS_group_dat %>% 
  mutate(Species = gsub('[\"]', '', Species)) %>% 
  clean_names()
colnames(CAS_group_dat)
#[1] "year"                       "detail_record_count"        "date_loaded_from_akr"       "date_loaded_to_repository"  "fmp_area"                   "fmp_subarea"               
#[7] "nmfs_area"                  "gear"                       "target"                     "species_group"              "species"                    "processor_federal_permit"  
#[13] "processor_name"             "processor_vessel"           "processor_plant_operation"  "vessel_federal_permit"      "vessel_name"                "vessel_length"             
#[19] "number_distinct_processors" "number_distinct_vessels"    "conf_flag"                  "discards_mt"                "retained_mt"                "total_catch_mt"            
#[25] "discard_rate"               "reporting_area_code" 

OROX_codes<-read.csv(paste(getwd(),"/Data/Static/GOA_OROX_codes.csv",sep=""), header=T)
colnames(OROX_codes)
#[1] "NORPAC_name" "NORPAC_code" "CAS_name"    "CAS_code" 

# CAS Species Specific Catch ----
#Preps species specific catch estimates, 2003 - present only
CAS_spec_totcatchshort <- CAS_spec_dat %>% 
  filter(year > 2002,
         fmp_subarea != "EY") %>% 
  select(year, fmp_subarea, species_common_name, weight_posted_sum, retained_discarded) %>% 
  group_by(year, fmp_subarea, retained_discarded, species_common_name) %>% 
  summarise(catch_mt = sum(weight_posted_sum)) %>% 
  pivot_wider(names_from = species_common_name, values_from = catch_mt) %>% 
  replace(is.na(.),0) %>% 
  pivot_longer(cols = starts_with("rockfish"), names_to = "CAS_name", values_to = "Catch_mt") %>% 
  left_join(OROX_codes)

# PSR Species Catch ----
##Former PSR species, widow and yellowtail, need to calculate as if still in PSR complex
##calculated from 2003-2009 only
PSR_species <- OROX_codes %>% 
  filter(CAS_name %in% c("rockfish, dusky","rockfish, dark","rockfish, blue",
                         "rockfish, black","rockfish, widow","rockfish, yellowtail"))
PSR_catch_dat <- CAS_group_dat %>% 
  filter(year < 2010 & year > 2002,
         species_group == "GOA Pelagic Shelf Rockfish")

#get total PSR Discards
PSR_discards <- PSR_catch_dat %>% 
  group_by(fmp_subarea, year) %>% 
  summarise(catch_discard = sum(discards_mt))

#put species observer data into usable format
obs_spec_dat <- NORPAC_dat %>% 
  filter(haul_year < 2010 & haul_year > 2002,
         sample_species_name %in% c("DUSKY ROCKFISH","DARK ROCKFISH","WIDOW ROCKFISH","YELLOWTAIL ROCKFISH")) %>% 
  group_by(haul_fmp_subarea, haul_year, sample_species_name) %>% 
  summarise(spec_wt = sum(sum_extrapolated_weight))

#calculate area/year total over all PSR species
obs_tot <- obs_spec_dat %>% 
  group_by(haul_fmp_subarea, haul_year) %>% 
  summarise(tot_wt = sum(spec_wt))

#calculate proportions by species
obs_spec_prop <- obs_spec_dat %>% 
  left_join(obs_tot) %>% 
  mutate(spec_prop = spec_wt/tot_wt) %>% 
  rename(fmp_subarea = haul_fmp_subarea,
         year = haul_year)

#multiply proportions by total discards for the complex
disc_est_spec <- obs_spec_prop %>% 
  left_join(PSR_discards) %>% 
  mutate(discard_wt = spec_prop*catch_discard) %>% 
  select(fmp_subarea, year, sample_species_name, discard_wt) %>% 
  rename(NORPAC_name = sample_species_name)

#get retained catch for PSR species
PSR_ret_catch <- CAS_spec_totcatchshort %>% 
  #rename(species = CAS_name) %>% 
  left_join(OROX_codes) %>% 
  filter(CAS_code %in% c(154, 156, 155), #using code here because CAS has duplicates for common name, which bring in "dusky rockfish unknown"
         retained_discarded == "Retained",
         year < 2010) %>% 
  ungroup() %>% 
  select(year, fmp_subarea, NORPAC_name, Catch_mt) %>% 
  left_join(disc_est_spec) %>% 
  mutate(discard_wt = replace_na(discard_wt, 0),
         tot_catch = Catch_mt+discard_wt) %>% 
  filter(NORPAC_name %in% c("WIDOW ROCKFISH","YELLOWTAIL ROCKFISH"))%>% 
  rename(Retained = Catch_mt, Discarded = discard_wt)

# Non-PSR OROX Species Catch ----

##Catch history for OROX species (not former PSR) 
## For 2003-2009
#Getting total non-PSR discards
ORsub_discards <- CAS_group_dat %>% 
  filter(year < 2010 & year > 2002,
         species_group == "Other Rockfish", #used to have other groups, but for this year range, this is all you need
         fmp_subarea != "EY") %>% #this is only in 1991 and outside the range for these calcs
  group_by(fmp_subarea, year) %>% 
  summarise(ORsub_disc = sum(discards_mt)) 

#put species observer data into usable format
ORsub_obs_spec_dat <- NORPAC_dat %>% 
  filter(haul_year < 2010 & haul_year > 2002,
         sample_species_name %nin% c("BLACK ROCKFISH","BLUE ROCKFISH","DUSKY ROCKFISH","DUSKY ROCKFISH UNIDENTIFIED",
                                   "DARK ROCKFISH","DARK ROCKFISH UNIDENTIFIED","WIDOW ROCKFISH",
                                   "YELLOWTAIL ROCKFISH")) %>% 
  group_by(haul_fmp_subarea, haul_year, sample_species_name) %>% 
  summarise(obs_spec_catch = sum(sum_extrapolated_weight))

#calculate area/year total over all ORsub species
ORsub_obs_tot <- NORPAC_dat %>% 
  filter(haul_year < 2010 & haul_year > 2002,
         sample_species_name %nin% c("BLACK ROCKFISH","BLUE ROCKFISH","DUSKY ROCKFISH","DUSKY ROCKFISH UNIDENTIFIED",
                                     "DARK ROCKFISH","DARK ROCKFISH UNIDENTIFIED","WIDOW ROCKFISH",
                                     "YELLOWTAIL ROCKFISH")) %>% 
  group_by(haul_fmp_subarea, haul_year) %>% 
  summarise(obs_tot_catch = sum(sum_extrapolated_weight))

#calculate proportions by species
ORsub_obs_spec_prop <- ORsub_obs_spec_dat %>% 
  left_join(ORsub_obs_tot) %>% 
  mutate(prop = obs_spec_catch/obs_tot_catch) %>% 
  rename(fmp_subarea = haul_fmp_subarea,
         year = haul_year)

#multiply proportions by total discards for the complex
ORsub_obs_disc_dat <- ORsub_obs_spec_prop %>% 
  left_join(ORsub_discards) %>% 
  mutate(discard_wt = prop*ORsub_disc) %>% 
  select(fmp_subarea, year, sample_species_name, discard_wt) %>% 
  rename(NORPAC_name = sample_species_name)
  

#get total catch for ORsub species
ORsub_ret_catch <- CAS_spec_totcatchshort %>% 
  #rename(species = CAS_name) %>% 
  left_join(OROX_codes) %>% 
  filter(CAS_code %nin% c(142, 167, 154, 172, 173, 156, 155), #using code here because CAS has duplicates for common name, which bring in "dusky rockfish unknown"
         retained_discarded == "Retained",
         year < 2010) %>% 
  ungroup() %>% 
  select(year, fmp_subarea, NORPAC_name, Catch_mt) %>% 
  full_join(ORsub_obs_disc_dat) %>% 
  mutate(discard_wt = replace_na(discard_wt, 0),
         tot_catch = Catch_mt+discard_wt) %>% 
  rename(Retained = Catch_mt, Discarded = discard_wt)

# OROX Catch 2010 - Present ----
##Catch for OROX species
## 2010 into the future
#Getting OROX group data into usable format

OROX_catch_dat<-read_csv(paste(datadir, "/CAS_GFtotfishery_confidential.csv", sep=""))

NROX <- OROX_catch_dat %>% 
  clean_names() %>% 
  mutate(species = gsub('[\"]', '', species),
         species = as.character(noquote(species))) %>% 
  rename(CAS_name = species) %>% 
  filter(CAS_name == "rockfish, northern",
         fmp_subarea != "WG" & fmp_subarea != "CG") %>% 
  select(fmp_subarea, year, CAS_name, retained_discarded, catch_mt) 

DSRspec <- c("rockfish, canary", "rockfish, china", "rockfish, copper", "rockfish, quillback",
             "rockfish, rosethorn", "rockfish, tiger", "rockfish, yelloweye")
DSR <- OROX_catch_dat %>% 
  clean_names() %>% 
  mutate(species = gsub('[\"]', '', species),
         species = as.character(noquote(species))) %>% 
  rename(CAS_name = species) %>% 
  filter(CAS_name %in% DSRspec,
         fmp_subarea != "SE") %>% 
  select(fmp_subarea, year, CAS_name, retained_discarded, catch_mt)

OROX_catch_dat2 <- OROX_catch_dat %>% 
  clean_names() %>% 
  mutate(species = gsub('[\"]', '', species),
         species = as.character(noquote(species))) %>% 
  rename(CAS_name = species) %>% 
  filter(CAS_name != "rockfish, northern",
         CAS_name != "rockfish, dusky",
         CAS_name %nin% DSRspec) %>% # SE/WY catch will be added back in
  bind_rows(NROX, DSR) %>% 
  select(fmp_subarea, year, CAS_name, retained_discarded, catch_mt) %>% 
  left_join(OROX_codes) %>% 
  select(fmp_subarea, year, NORPAC_name, retained_discarded, catch_mt) %>% 
  filter(!is.na(NORPAC_name)) %>% 
  group_by(fmp_subarea, year, NORPAC_name, retained_discarded) %>% 
  summarise(catch_sum = sum(catch_mt)) %>% 
  pivot_wider(names_from = NORPAC_name, values_from = catch_sum) %>%
  replace(is.na(.),0) %>% 
  pivot_longer(!c(year, fmp_subarea, retained_discarded), names_to = "NORPAC_name", values_to = "catch_sum") %>% 
  pivot_wider(names_from = retained_discarded, values_from = catch_sum) %>% 
  replace(is.na(.),0) %>% 
  mutate(tot_catch = Retained+Discarded)

# Giant Summary Table ----
##Combining the above into one table
##

DSRspec_NORPAC <- c("CANARY ROCKFISH", "CHINA ROCKFISH", "COPPER ROCKFISH", "QUILLBACK ROCKFISH",
             "ROSETHORN ROCKFISH", "TIGER ROCKFISH", "YELLOWEYE ROCKFISH")
OROXcomplex <- PSR_ret_catch %>% 
  bind_rows(ORsub_ret_catch, OROX_catch_dat2) %>% 
  mutate(OROXsubgroup = if_else(NORPAC_name %in% DSRspec_NORPAC, "DSR", "Slope")) %>% 
  filter(!(fmp_subarea == "SE" & OROXsubgroup == "DSR"),
         !(fmp_subarea == "WG" & NORPAC_name == "NORTHERN ROCKFISH"),
         !(fmp_subarea == "CG" & NORPAC_name == "NORTHERN ROCKFISH"))

#make sure no DSR species in SE, or northerns in WG/CG!!!!!

write.csv(OROXcomplex,paste(catchdir,"/OROXcomplex_catch",AYR,"_confidential.csv",sep=""),row.names=F)

# Discards ----
# Overall discards for full complex, prior to 2003 is fixed
pre2003 <- read_csv(paste(getwd(),"/Data/Static/OROX_catch_91_02_confidential.csv",sep=""))
OROX_comp_catch <- pre2003 %>% 
  rename(year = Haul.Year, fmp_subarea = Haul.FMP.Subarea, NORPAC_name = Sample.Species.Name,
         Discarded = Discards, tot_catch = Total.Catch) %>% 
  mutate(OROXsubgroup = if_else(NORPAC_name %in% DSRspec_NORPAC, "DSR", "Slope")) %>% 
  bind_rows(OROXcomplex)

FMP_discard <- OROX_comp_catch %>% 
  group_by(year) %>% 
  summarise(disc = sum(Discarded, na.rm = T),
            tot = sum(tot_catch, na.rm = T)) %>% 
  mutate(disc_rate = disc/tot, fmp_subarea = "GOA",
         OROXsubgroup = "OROX")

FMP_subarea_discard <- OROX_comp_catch %>% 
  group_by(year, fmp_subarea) %>% 
  summarise(disc = sum(Discarded, na.rm = T),
            tot = sum(tot_catch, na.rm = T)) %>% 
  mutate(disc_rate = disc/tot, OROXsubgroup = "OROX")

OROX_suba_subg_discard <-OROX_comp_catch %>% 
  group_by(year, fmp_subarea, OROXsubgroup) %>% 
  summarise(disc = sum(Discarded, na.rm = T),
            tot = sum(tot_catch, na.rm = T)) %>% 
  mutate(disc_rate = disc/tot) %>% 
  replace(is.na(.),0)

OROX_subg_FMP_discard <-OROX_comp_catch %>% 
  group_by(year, OROXsubgroup) %>% 
  summarise(disc = sum(Discarded, na.rm = T),
            tot = sum(tot_catch, na.rm = T)) %>% 
  mutate(disc_rate = disc/tot, fmp_subarea = "GOA") %>% 
  replace(is.na(.),0)

OROX_discards <- FMP_discard %>% 
  bind_rows(FMP_subarea_discard, OROX_suba_subg_discard, OROX_subg_FMP_discard) %>% 
  filter(!(fmp_subarea == "SE" & OROXsubgroup == "DSR"))

write.csv(OROX_discards, paste(catchdir,"/OROX_discards",AYR,"_confidential.csv",sep=""),row.names=F)

# discards by gear
OROX_catch_dat3 <- OROX_catch_dat %>% 
  clean_names() %>% 
  mutate(species = gsub('[\"]', '', species),
         species = as.character(noquote(species)),
         gear = replace(gear, gear == "NPT", "TWL"),
         gear = replace(gear, gear == "PTR", "TWL")) %>% 
  rename(CAS_name = species) %>% 
  filter(CAS_name != "rockfish, northern",
         CAS_name %nin% DSRspec) %>% # SE/WY catch will be added back in
  bind_rows(NROX, DSR) %>% 
  select(fmp_subarea, year, CAS_name, retained_discarded, catch_mt, gear) %>% 
  left_join(OROX_codes) %>% 
  select(fmp_subarea, year, NORPAC_name, retained_discarded, catch_mt, gear) %>% 
  filter(!is.na(NORPAC_name)) %>% 
  mutate(OROXsubgroup = if_else(NORPAC_name %in% DSRspec_NORPAC, "DSR", "Slope")) %>% 
  group_by(fmp_subarea, year, OROXsubgroup, retained_discarded, gear) %>% 
  summarise(catch_sum = sum(catch_mt, na.rm=T)) %>% 
  mutate(gear = replace_na(gear, "unk")) %>% 
  pivot_wider(names_from = OROXsubgroup, values_from = catch_sum) %>%
  replace(is.na(.),0) %>% 
  pivot_longer(!c(year, fmp_subarea, retained_discarded, gear), names_to = "OROXsubgroup", values_to = "catch_sum") %>% 
  pivot_wider(names_from = retained_discarded, values_from = catch_sum) %>% 
  replace(is.na(.),0) %>% 
  mutate(tot_catch = Retained+Discarded, 
         disc_rate = if_else(tot_catch ==0, 0, Discarded/tot_catch))

GOA_gear <- OROX_catch_dat3 %>% 
  group_by(year, gear, OROXsubgroup) %>% 
  summarise(disc = sum(Discarded, na.rm=T),
            tot = sum(tot_catch, na.rm=T)) %>% 
  mutate(disc_rate = if_else(tot ==0, 0, disc/tot),
         fmp_subarea = "GOA") %>% 
  rename(Discarded = disc, tot_catch = tot)

OROX_gear_discards <- OROX_catch_dat3 %>% 
  bind_rows(GOA_gear) %>% 
  filter(!(fmp_subarea == "SE" & OROXsubgroup == "DSR"))

write.csv(OROX_gear_discards, paste(catchdir,"/OROX_gear_discards",AYR,"_confidential.csv",sep=""),row.names=F)

