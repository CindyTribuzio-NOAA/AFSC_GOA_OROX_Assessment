# NOTES ----
# These are output to html, then copy pasted into word. Could be done in a single markdown, but still would have to copy paste.

# SETUP ----
tabdir <- paste(getwd(),"/Output/", AYR, "/Tables", sep="")
dir.create(tabdir)

#adapted from theme_booktabs
theme_SAFE<-function(x){
  big_border <- fp_border(width = 2)
  std_border <- fp_border(width = 1)
  x<-fontsize(x,size=10,part="all")
  x<-font(x,fontname="Times New Roman",part="all")
  x<-bold(x,part="header")
  x <- hline_top(x, border = big_border, part = "header")
  x <- hline(x, border = std_border, part = "header")
  x <- hline_bottom(x, border = big_border, part = "body")
  x <- align(x,align="center",part="header")
}

# EXEC SUMMARY ----
# Tier 4 Summary Tables ----
#need to add all table parts, but for now, it's just the numbers

#automate these as well
T4FOFL <- 0.079
T4FABC <- 0.065
T4M <- 0.06

#previous assesssment: this will need to be fixed for next assessment due to format changes before 2021 code version
specs_old <- read_csv(paste(getwd(),"/Output/", AYR-2, "/Harvest_specs/OROX_harvestrecs_", AYR-2, ".csv",sep=""))

T4_specs_old <- specs_old %>% 
  filter(Group == "SC",
         REG_AREA == "GOA")

T4_specs_old2 <- as.data.frame(rbind(T4M, 4, round(T4_specs_old$Biom_est, 0), T4FOFL, T4FABC, T4FABC, 
                                    round(T4_specs_old$OFL, 0), round(T4_specs_old$ABC, 0), round(T4_specs_old$ABC, 0)),
                              row.names = F) 

T4_specs_old3 <- cbind(T4_specs_old2, T4_specs_old2)
#colnames(T4_specs_old3)<- c(AYR, AYR+1)
T4_specs_old3 <- rbind(T4_specs_old3, c(AYR-2, AYR-1))

#current assessment
specs <- read_csv(paste(getwd(),"/Output/", AYR, "/Harvest_specs/OROX_harvestrecs_", AYR, ".csv",sep=""))

T4_specs <- specs %>% 
  filter(Group == "SC",
         REGULATORY_AREA_NAME == "GOA")

T4_specs_new<- as.data.frame(rbind(T4M, 4, round(T4_specs$Biom_est, 0), T4FOFL, T4FABC, T4FABC, 
                                    round(T4_specs$OFL, 0), round(T4_specs$ABC, 0), round(T4_specs$ABC, 0)),
                              row.names = F)
T4_specs_new2 <- cbind(T4_specs_new, T4_specs_new)
#colnames(T4_specs_new2)<- c(AYR, AYR+1)
T4_specs_new2 <- rbind(T4_specs_new2, c(AYR-1, AYR))

#combined T4 specs
spec_cols <- c("Quantity", "AYR", "AYR+1", "NEW_AYR+1", "AYR+2")
Quans <- c("M (natural mortality rate)", "Tier", "Biomass (t)", "FOFL", "maxFABC", "FABC",
           "OFL (t)", "maxABC (t)", "ABC (t)", "Status")
T4_specs_dat <- cbind(Quans, T4_specs_old3, T4_specs_new2)
names(T4_specs_dat) <- spec_cols

T4_specs_table <- flextable(T4_specs_dat,
                            col_keys = spec_cols,
                            theme_fun = theme_SAFE)
save_as_html(T4_specs_table, path=paste(tabdir,"/EXEC_summ_T4specs.html",sep=""))

#Tier 5 Summary Table ----
WTDM <- read_csv(paste(getwd(),"/Output/", AYR, "/Harvest_specs/OROX_Wtd_M.csv",sep=""))
T5FOFL_old <- 0.070
T5FABC_old <- 0.053
T5M_old <- T5FOFL_old

T5FOFL <- round(WTDM %>% filter(YEAR == AYR) %>% select(WTM), 3)
T5FABC <- round(T5FOFL*0.75, 3)
T5M <- T5FOFL
#previous assesssment: this will need to be fixed for next assessment due to format changes before 2021 code version
T5_specs_old <- specs_old %>% 
  filter(Group == "T5",
         REG_AREA == "GOA")

T5_specs_old2 <- as.data.frame(rbind(T5M_old, 5, round(T5_specs_old$Biom_est, 0), T5FOFL_old, T5FABC_old, T5FABC_old, 
                                     round(T5_specs_old$OFL, 0), round(T5_specs_old$ABC, 0), round(T5_specs_old$ABC, 0)),
                               row.names = F) 

T5_specs_old3 <- cbind(T5_specs_old2, T5_specs_old2)
T5_specs_old3 <- rbind(T5_specs_old3, c(AYR-2, AYR-1))

#current assessment
T5_specs <- specs %>% 
  filter(Group == "T5",
         REGULATORY_AREA_NAME == "GOA")

T5_specs_new<- as.data.frame(rbind(T5M, 5, round(T5_specs$Biom_est, 0), T5FOFL, T5FABC, T5FABC, 
                                   round(T5_specs$OFL, 0), round(T5_specs$ABC, 0), round(T5_specs$ABC, 0)),
                             row.names = F)
T5_specs_new2 <- cbind(T5_specs_new, T5_specs_new)
T5_specs_new2 <- rbind(T5_specs_new2, c(AYR-1, AYR))

#combined T5 specs
T5_specs_dat <- cbind(Quans, T5_specs_old3, T5_specs_new2)
names(T5_specs_dat) <- spec_cols

T5_specs_table <- flextable(T5_specs_dat,
                            col_keys = spec_cols,
                            theme_fun = theme_SAFE)
save_as_html(T5_specs_table, path=paste(tabdir,"/EXEC_summ_T5specs.html",sep=""))

#Tier 6 Summary Table ----
#previous assesssment: this will need to be fixed for next assessment due to format changes before 2021 code version
#T6_specs_old <- specs_old %>% #what's in the last assessment is diff from what is in specs file
#  filter(Group == "T6",
#         REG_AREA == "GOA")

T6_OFL_old <- 275
T6_ABC_old <- 206

T6_specs_old2 <- as.data.frame(rbind(6, T6_OFL_old, T6_ABC_old, T6_ABC_old),
                               row.names = F) 

T6_specs_old3 <- cbind(T6_specs_old2, T6_specs_old2)
T6_specs_old3 <- rbind(T6_specs_old3, c(AYR-2, AYR-1))

#current assessment
T6_specs <- specs %>% 
  filter(Group == "T6",
         REGULATORY_AREA_NAME == "GOA")

T6_specs_new<- as.data.frame(rbind(6, T6_specs$OFL, T6_specs$ABC, T6_specs$ABC),
                             row.names = F)
T6_specs_new2 <- cbind(T6_specs_new, T6_specs_new)
T6_specs_new2 <- rbind(T6_specs_new2, c(AYR-1, AYR))

#combined T6 specs
T6_quans <- c("Tier", "OFL (t)", "maxABC (t)", "ABC (t)", "Status")
T6_specs_dat <- cbind(T6_quans, T6_specs_old3, T6_specs_new2)
names(T6_specs_dat) <- spec_cols

T6_specs_table <- flextable(T6_specs_dat,
                            col_keys = spec_cols,
                            theme_fun = theme_SAFE)
save_as_html(T6_specs_table, path=paste(tabdir,"/EXEC_summ_T6specs.html",sep=""))

# Complex Summary ----
# previous assessment
OROX_OFL_old <- 5320 #automate this next time
OROX_ABC_old <- 4053

OROX_specs_old2 <- as.data.frame(rbind(456, OROX_OFL_old, OROX_ABC_old, OROX_ABC_old),
                               row.names = F) 

OROX_specs_old3 <- cbind(OROX_specs_old2, OROX_specs_old2)
OROX_specs_old3 <- rbind(OROX_specs_old3, c(AYR-2, AYR-1))

#current assessment
OROX_specs <- specs %>% 
  filter(Group == "OROX",
         REGULATORY_AREA_NAME == "GOA")

OROX_specs_new<- as.data.frame(rbind(456, OROX_specs$OFL, OROX_specs$ABC, OROX_specs$ABC),
                             row.names = F)
OROX_specs_new2 <- cbind(OROX_specs_new, OROX_specs_new)
OROX_specs_new2 <- rbind(OROX_specs_new2, c(AYR-1, AYR))

#combined OROX specs
OROX_specs_dat <- cbind(T6_quans, OROX_specs_old3, OROX_specs_new2)
names(OROX_specs_dat) <- spec_cols

OROX_specs_table <- flextable(OROX_specs_dat,
                            col_keys = spec_cols,
                            theme_fun = theme_SAFE)
save_as_html(OROX_specs_table, path=paste(tabdir,"/EXEC_summ_OROXspecs.html",sep=""))

# Catch ----
catch_cols <- c("Year", "Western GOA", "Central GOA", "West Yakutat", "E. Yak/Southeast", "Gulfwide Total",
                "Gulfwide ABC", "Gulfwide TAC")

OROX_specs_old <- read_csv(paste(getwd(),"/Data/OROX_specs_history.csv",sep="")) %>% 
  filter(Year %in% c(AYR-1, AYR)) %>% 
  select(Year, ABC, TAC)

catchdat <- read_csv(paste(getwd(),"/Output/", AYR, "/Catch/OROXcomplex_catch", AYR, "_confidential.csv",sep=""))

GOA_sum_catch <- catchdat %>% 
  filter(year%in% c(AYR-1, AYR)) %>% 
  group_by(year) %>% 
  summarise(Catch = round(sum(tot_catch, na.rm=T), 0)) %>% 
  select(!year)

sum_catch <- catchdat %>%
  filter(year%in% c(AYR-1, AYR)) %>% 
  group_by(fmp_subarea, year) %>% 
  summarise(Catch = round(sum(tot_catch, na.rm=T), 0)) %>% 
  pivot_wider(names_from = fmp_subarea, values_from = Catch) %>%
  bind_cols(OROX_specs_old, GOA_sum_catch) %>% 
  select(Year, WG, CG, WY, SE, Catch, ABC, TAC)

names(sum_catch) <- catch_cols

OROX_catchsumm_table <- flextable(sum_catch,
                              col_keys = catch_cols,
                              theme_fun = theme_SAFE)
save_as_html(OROX_catchsumm_table, path=paste(tabdir,"/EXEC_summ_OROXcatch.html",sep=""))

# Tier 4 Apportionment ----
Apport_cols <- c("rowname","WESTCENTRAL", "WYAK", "SOUTHEAST", "Total")

harvest_specs <- read_csv(paste(getwd(),"/Output/", AYR, "/Harvest_Specs/OROX_harvestrecs_", AYR, ".csv",sep="")) %>% 
  mutate(Apport = round(Apport*100, 2)) %>% 
  arrange(Group, REGULATORY_AREA_NAME)

T4OFL <- harvest_specs %>% 
  filter(Group == "SC",
         REGULATORY_AREA_NAME == "GOA") %>% 
  select(OFL)
T4OFL_row <- as.data.frame(c("OFL", NA, NA, NA, T4OFL))
names(T4OFL_row) <- Apport_cols

T4_app_WC <- harvest_specs %>% 
  filter(Group == "SC",
         REGULATORY_AREA_NAME %in% c("CENTRAL GOA", "WESTERN GOA")) %>% 
  group_by(Group) %>% 
  summarise(WCABC = sum(ABC),
            WCOFL = mean(OFL),
            WCapp = sum(Apport)) %>% 
  mutate(REGULATORY_AREA_NAME = "WESTCENTRAL") %>% 
  rename(ABC = WCABC,
         OFL = WCOFL,
         Apport = WCapp)

T4_App <- harvest_specs %>% 
  filter(Group == "SC",
         REGULATORY_AREA_NAME %in% c("WYAK", "SOUTHEAST")) %>%
  select(Group, ABC, OFL, REGULATORY_AREA_NAME, Apport) %>% 
  bind_rows(T4_app_WC) %>% 
  select(Apport, ABC) %>% 
  t() 
T4_total <- as.data.frame(rowSums(T4_App))
T4_App <- cbind(T4_App, T4_total)
colnames(T4_App) <- c("SOUTHEAST", "WYAK", "WESTCENTRAL", "Total") #couldn't figure out how to pipe this in
T4_App <- T4_App %>% 
  select(WESTCENTRAL, WYAK, SOUTHEAST, Total) %>% 
  rownames_to_column() %>% 
  bind_rows(T4OFL_row)

T4_App_table <- flextable(T4_App,
                          col_keys = Apport_cols,
                          theme_fun = theme_SAFE)
save_as_html(T4_App_table, path=paste(tabdir,"/EXEC_summ_T4Apport.html",sep=""))

# Tier 5 Apportionment ----
T5OFL <- harvest_specs %>% 
  filter(Group == "T5",
         REGULATORY_AREA_NAME == "GOA") %>% 
  select(OFL)
T5OFL_row <- as.data.frame(c("OFL", NA, NA, NA, T5OFL))
names(T5OFL_row) <- Apport_cols

T5_app_WC <- harvest_specs %>% 
  filter(Group == "T5",
         REGULATORY_AREA_NAME %in% c("CENTRAL GOA", "WESTERN GOA")) %>% 
  group_by(Group) %>% 
  summarise(WCABC = sum(ABC),
            WCOFL = mean(OFL),
            WCapp = sum(Apport)) %>% 
  mutate(REGULATORY_AREA_NAME = "WESTCENTRAL") %>% 
  rename(ABC = WCABC,
         OFL = WCOFL,
         Apport = WCapp)

T5_App <- harvest_specs %>% 
  filter(Group == "T5",
         REGULATORY_AREA_NAME %in% c("WYAK", "SOUTHEAST")) %>%
  select(Group, ABC, OFL, REGULATORY_AREA_NAME, Apport) %>% 
  bind_rows(T5_app_WC) %>% 
  select(Apport, ABC) %>% 
  t() 
T5_total <- as.data.frame(rowSums(T5_App))
T5_App <- cbind(T5_App, T5_total)
colnames(T5_App) <- c("SOUTHEAST", "WYAK", "WESTCENTRAL", "Total") #couldn't figure out how to pipe this in
T5_App <- T5_App %>% 
  select(WESTCENTRAL, WYAK, SOUTHEAST, Total) %>% 
  rownames_to_column() %>% 
  bind_rows(T5OFL_row)

T5_App_table <- flextable(T5_App,
                          col_keys = Apport_cols,
                          theme_fun = theme_SAFE)
save_as_html(T5_App_table, path=paste(tabdir,"/EXEC_summ_T5Apport.html",sep=""))

# Tier 6 Apportionment ----
T6OFL <- harvest_specs %>% 
  filter(Group == "T6",
         REGULATORY_AREA_NAME == "GOA") %>% 
  select(OFL)
T6OFL_row <- as.data.frame(c("OFL", NA, NA, NA, T6OFL))
names(T6OFL_row) <- Apport_cols

T6_app_WC <- harvest_specs %>% 
  filter(Group == "T6",
         REGULATORY_AREA_NAME %in% c("CENTRAL GOA", "WESTERN GOA")) %>% 
  group_by(Group) %>% 
  summarise(WCABC = sum(ABC),
            WCOFL = mean(OFL),
            WCapp = sum(Apport)) %>% 
  mutate(REGULATORY_AREA_NAME = "WESTCENTRAL") %>% 
  rename(ABC = WCABC,
         OFL = WCOFL,
         Apport = WCapp)

T6_App <- harvest_specs %>% 
  filter(Group == "T6",
         REGULATORY_AREA_NAME %in% c("WYAK", "SOUTHEAST")) %>%
  select(Group, ABC, OFL, REGULATORY_AREA_NAME, Apport) %>% 
  bind_rows(T6_app_WC) %>% 
  select(Apport, ABC) %>% 
  t() 
T6_total <- as.data.frame(rowSums(T6_App))
T6_App <- cbind(T6_App, T6_total)
colnames(T6_App) <- c("SOUTHEAST", "WYAK", "WESTCENTRAL", "Total") #couldn't figure out how to pipe this in
T6_App <- T6_App %>% 
  select(WESTCENTRAL, WYAK, SOUTHEAST, Total) %>% 
  rownames_to_column() %>% 
  bind_rows(T6OFL_row)

T6_App_table <- flextable(T6_App,
                          col_keys = Apport_cols,
                          theme_fun = theme_SAFE)
save_as_html(T6_App_table, path=paste(tabdir,"/EXEC_summ_T6Apport.html",sep=""))

# OROX Apportionment ----
OROXOFL <- harvest_specs %>% 
  filter(Group == "OROX",
         REGULATORY_AREA_NAME == "GOA") %>% 
  select(OFL)
OROXOFL_row <- as.data.frame(c("OFL", NA, NA, NA, OROXOFL))
names(OROXOFL_row) <- Apport_cols

OROX_app_WC <- harvest_specs %>% 
  filter(Group == "OROX",
         REGULATORY_AREA_NAME %in% c("CENTRAL GOA", "WESTERN GOA")) %>% 
  group_by(Group) %>% 
  summarise(WCABC = sum(ABC),
            WCOFL = mean(OFL),
            WCapp = sum(Apport)) %>% 
  mutate(REGULATORY_AREA_NAME = "WESTCENTRAL") %>% 
  rename(ABC = WCABC,
         OFL = WCOFL,
         Apport = WCapp)

OROX_App <- harvest_specs %>% 
  filter(Group == "OROX",
         REGULATORY_AREA_NAME %in% c("WYAK", "SOUTHEAST")) %>%
  select(Group, ABC, OFL, REGULATORY_AREA_NAME, Apport) %>% 
  bind_rows(OROX_app_WC) %>% 
  select(Apport, ABC) %>% 
  t() 
OROX_total <- as.data.frame(rowSums(OROX_App))
OROX_App <- cbind(OROX_App, OROX_total)
colnames(OROX_App) <- c("SOUTHEAST", "WYAK", "WESTCENTRAL", "Total") #couldn't figure out how to pipe this in
OROX_App <- OROX_App %>% 
  select(WESTCENTRAL, WYAK, SOUTHEAST, Total) %>% 
  rownames_to_column() %>% 
  bind_rows(OROXOFL_row)

OROX_App_table <- flextable(OROX_App,
                          col_keys = Apport_cols,
                          theme_fun = theme_SAFE)
save_as_html(OROX_App_table, path=paste(tabdir,"/EXEC_summ_OROXApport.html",sep=""))

# Plan Team Summaries ----
specs_hist <- read_csv(paste(getwd(),"/Data/OROX_specs_history.csv",sep="")) %>% 
  filter(Year >= AYR-1)
Apport_hist <- read_csv(paste(getwd(),"/Data/OROX_ABC_App_historical.csv",sep="")) %>% 
  filter(year >= AYR-1)

Year <- c(specs_hist$Year, AYR+2)

OROX_biom <- c(rep(round(T4_specs_old$Biom_est,0) + round(T5_specs_old$Biom_est,0),2),
               rep(round(T4_specs$Biom_est,0) + round(T5_specs$Biom_est,0), 2))
OROX_OFL <- c(specs_hist$OFL, specs_hist$OFL[3])
OROX_ABC <- c(specs_hist$ABC, specs_hist$ABC[3])
OROX_TAC <- c(specs_hist$TAC, specs_hist$TAC[3])
OROX_catch <- c(sum_catch$'Gulfwide Total', NA, NA)

OROX_PTsumm <- as.data.frame(cbind(Year, OROX_biom, OROX_OFL, OROX_ABC, OROX_TAC, OROX_catch))

OROX_PTsumm_table <- flextable(OROX_PTsumm,
                            col_keys = names(OROX_PTsumm),
                            theme_fun = theme_SAFE)
save_as_html(OROX_PTsumm_table, path=paste(tabdir,"/EXEC_summ_PTsummary1.html",sep=""))

Year2 <- c(paste(c("OFL", "ABC", "TAC", "Catch"), AYR, sep=""), 
           paste(c("OFL", "ABC"), AYR+1, sep=""),
           paste(c("OFL", "ABC"), AYR+2, sep=""))
OFL_AYR <- as.tibble(c(NA, NA, NA, specs_hist$OFL[1]))
OFL_AYR1 <- as.tibble(c(NA, NA, NA, specs_hist$OFL[3]))
ABC_AYR <- Apport_hist %>% 
  filter(year == AYR) %>% 
  select(fmp_subarea, ABC) %>%
  spread(fmp_subarea, ABC) %>% 
  select(WEST_CENTRAL, WYAK, SOUTHEAST) %>% 
  rowwise() %>% 
  mutate(total = sum(c_across())) %>% 
  gather(fmp_subarea, ABC) %>% 
  select(ABC)
ABC_AYR1 <- Apport_hist %>% 
  filter(year == AYR+1) %>% 
  select(fmp_subarea, ABC) %>%
  spread(fmp_subarea, ABC) %>% 
  select(WEST_CENTRAL, WYAK, SOUTHEAST) %>% 
  rowwise() %>% 
  mutate(total = sum(c_across())) %>% 
  gather(fmp_subarea, ABC) %>% 
  select(ABC)
TAC_AYR <- Apport_hist %>% 
  filter(year == AYR) %>% 
  select(fmp_subarea, TAC) %>%
  spread(fmp_subarea, TAC) %>% 
  select(WEST_CENTRAL, WYAK, SOUTHEAST) %>% 
  rowwise() %>% 
  mutate(total = sum(c_across())) %>% 
  gather(fmp_subarea, TAC) %>% 
  select(TAC)

Catch_EG_AYR <- catchdat %>%
  filter(fmp_subarea %in% c("WY", "SE"),
         year == AYR) %>% 
  group_by(fmp_subarea) %>% 
  summarise(Catch = round(sum(tot_catch, na.rm=T), 0)) %>% 
  arrange(desc(fmp_subarea))
Catch_GOA_AYR <- catchdat %>%
  filter(year == AYR) %>% 
  group_by() %>% 
  summarise(Catch = round(sum(tot_catch, na.rm=T), 0)) %>% 
  mutate(fmp_subarea = "GOA")
Catch_AYR <- catchdat %>%
  filter(fmp_subarea %in% c("CG", "WG"),
         year == AYR) %>% 
  group_by() %>% 
  summarise(Catch = round(sum(tot_catch, na.rm=T), 0)) %>% 
  mutate(fmp_subarea = "WC") %>% 
  bind_rows(Catch_EG_AYR, Catch_GOA_AYR) %>% 
  select(Catch) 

OROX_PTsumm2 <- as.data.frame(c(OFL_AYR, ABC_AYR, TAC_AYR, Catch_AYR,
                                OFL_AYR1, ABC_AYR1, OFL_AYR1, ABC_AYR1))
names(OROX_PTsumm2) <- Year2
OROX_PTsumm2_table <- flextable(OROX_PTsumm2,
                               col_keys = names(OROX_PTsumm2),
                               theme_fun = theme_SAFE)
save_as_html(OROX_PTsumm2_table, path=paste(tabdir,"/EXEC_summ_PTsummary2.html",sep=""))
