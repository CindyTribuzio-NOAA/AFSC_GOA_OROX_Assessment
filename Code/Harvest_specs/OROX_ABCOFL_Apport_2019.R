specdir <- paste(getwd(),"/Output/", AYR, "/Harvest_Specs", sep="")
dir.create(specdir)

#####################################################
#####################################################
#Tier 4/5

RFXB<-read_csv(paste(getwd(),"/Output/",AYR,"/RFX/RFX_Biomass_GOA_OROX.csv",sep=""))
###########################################
#Apportionment

#Sharpchin
SCend <- RFXB %>% 
  filter(YEAR == endyr, Group == "OROX_SC", 
         grepl(" GOA", REGULATORY_AREA_NAME)) %>% 
  select(REGULATORY_AREA_NAME, Biom_est)

SCend<-RFXB[RFXB$YEAR==endyr&RFXB$Group=="OROX_SC",c("REG_AREA","Biom_est")]
#SCend<-SCend[SCend$REG_AREA!="GOA",]
Apport<-SCend$Biom_est/sum(SCend[SCend$REG_AREA!="GOA",]$Biom_est)
SCapp<-cbind(SCend,Apport)
SCapp$group<-"Sharpchin"


#all others
T5end<-RFXB[RFXB$YEAR==endyr&RFXB$Group=="OROX_allnoSC",c("REG_AREA","Biom_est","Group")]
#T5end<-T5end[T5end$REG_AREA!="GOA",]
Apport<-T5end$Biom_est/sum(T5end[T5end$REG_AREA!="GOA",]$Biom_est)
T5app<-cbind(T5end,Apport)
T5app$group<-"Tier5"

#############################################
#Weighted M for non-SC
WTMend<-RFXB[(RFXB$YEAR==endyr&RFXB$Group!=c("OROX_all")&RFXB$REG_AREA=="GOA"),c("REG_AREA","Biom_est","Group")]
WTMend<-WTMend[WTMend$Group!=c("OROX_SC"),] #haven't been able to figure out how to combine with above
WTMend<-WTMend[WTMend$Group!=c("OROX_allnoSC"),]
WTMend$M<-c(0.05,0.06,0.07,0.092,0.1)
WTM<-crossprod(WTMend$Biom_est,WTMend$M)/sum(WTMend$Biom_est)

#replace T5 biomass with summed biomass from M groups, this is whats used for ABC
T5app[T5app$REG_AREA=="GOA",]$Biom_est<-sum(WTMend$Biom_est)
##############################################
#Harvest specs

#Sharpchin, FABC and FOFL from Tier 4 methods (need to find files for that)
FABC<-0.065
FOFL<-0.079
SC_exB<-RFXB[RFXB$YEAR==endyr&RFXB$Group=="OROX_SC"&RFXB$REG_AREA=="GOA",c("Biom_est")]
SC_ABC<-SC_exB*FABC
SC_OFL<-SC_exB*FOFL

#Tier 5 species
T5FOFL<-round(WTM,3)
T5FABC<-0.75*T5FOFL
T5expB<-sum(WTMend$Biom_est)
T5ABC<-T5expB*T5FABC
T5OFL<-T5expB*T5FOFL

###########################################
#Apportioned ABC

#Sharpchin
SCAppABC<-as.data.frame(SC_ABC*SCapp$Apport)
SCAppABC<-cbind(SCapp$REG_AREA,SCapp$Biom_est,SCAppABC,SCapp$Apport)
colnames(SCAppABC)<-c("REG_AREA","Biom_est","ABC","Apport")
SCAppABC$Group<-"SC"

#Tier 5 species
T5AppABC<-as.data.frame(T5ABC*T5app$Apport)
T5AppABC<-cbind(T5app$REG_AREA,T5app$Biom_est,T5AppABC,T5app$Apport)
colnames(T5AppABC)<-c("REG_AREA","Biom_est","ABC","Apport")
T5AppABC$Group<-"T5"

################################################
#E Goa split fraction
split<-read.csv("Split_fractions.csv",header=T)
split<-split[(nrow(split)-2):nrow(split),c("Year","Eastern.Fraction")]
EYfrac<-((split[1,2]*4)+(split[2,2]*6)+(split[3,2]*9))/19

SCEYABC<-SCAppABC[SCAppABC$REG_AREA=="EASTERN GOA",c("ABC")]*EYfrac
SCWYABC<-SCAppABC[SCAppABC$REG_AREA=="EASTERN GOA",c("ABC")]-SCEYABC
SCEYBiom<-SCAppABC[SCAppABC$REG_AREA=="EASTERN GOA",c("Biom_est")]*EYfrac
SCWYBiom<-SCAppABC[SCAppABC$REG_AREA=="EASTERN GOA",c("Biom_est")]-SCEYBiom
EYfracbig<-SCAppABC[SCAppABC$REG_AREA=="EASTERN GOA",c("Apport")]*EYfrac #split fractions relative to GOA wide apporitonment for summary tables
WYfracbig<-SCAppABC[SCAppABC$REG_AREA=="EASTERN GOA",c("Apport")]-EYfracbig
SCEGOA<-as.data.frame(cbind(c("EYAK","WYAK"),c(SCEYBiom,SCWYBiom),c(SCEYABC,SCWYABC),c(EYfracbig,WYfracbig),c("SC")))
colnames(SCEGOA)<-c("REG_AREA","Biom_est","ABC","Apport","Group")

T5EYABC<-T5AppABC[T5AppABC$REG_AREA=="EASTERN GOA",c("ABC")]*EYfrac
T5WYABC<-T5AppABC[T5AppABC$REG_AREA=="EASTERN GOA",c("ABC")]-T5EYABC
T5EYBiom<-T5AppABC[T5AppABC$REG_AREA=="EASTERN GOA",c("Biom_est")]*EYfrac
T5WYBiom<-T5AppABC[T5AppABC$REG_AREA=="EASTERN GOA",c("Biom_est")]-T5EYBiom
EYfracbig<-T5AppABC[T5AppABC$REG_AREA=="EASTERN GOA",c("Apport")]*EYfrac #split fractions relative to GOA wide apporitonment for summary tables
WYfracbig<-T5AppABC[T5AppABC$REG_AREA=="EASTERN GOA",c("Apport")]-EYfracbig
T5EGOA<-as.data.frame(cbind(c("EYAK","WYAK"),c(T5EYBiom,T5WYBiom), c(T5EYABC,T5WYABC),c(EYfracbig,WYfracbig),c("T5")))
colnames(T5EGOA)<-c("REG_AREA","Biom_est","ABC","Apport","Group")

###############################
#Tier 4 and 5 summary table
SCT5AppABC<-rbind(SCAppABC,SCEGOA,T5AppABC,T5EGOA)
SCT5AppABC$OFL[SCT5AppABC$Group=="SC"]<-SC_OFL
SCT5AppABC$OFL[SCT5AppABC$Group=="T5"]<-T5OFL


###############################################################
################################################################
#Tier 6 - only the DSR species and only outside of EY/se, ABC by area
OROX_catch<-read.csv(paste("OROXcomplex_catch",AYR,".csv",sep=""),header=T)
OROX_catch<-OROX_catch[OROX_catch$Haul.FMP.Subarea!="SEI",] #inside areas aren't used for ABCs
OROX_catch<-OROX_catch[OROX_catch$Haul.FMP.Subarea!="PWSI",]
begyr<-2013
endyr<-2016  #2016 is used as per 2017 assessment

T6_DSR<-c("CANARY ROCKFISH","CHINA ROCKFISH","QUILLBACK ROCKFISH","ROSETHORN ROCKFISH",
           "TIGER ROCKFISH","YELLOWEYE ROCKFISH","COPPER ROCKFISH")

T6dat_DSR<-OROX_catch[OROX_catch$Sample.Species.Name%in%T6_DSR,]
T6dat_DSR<-T6dat_DSR[T6dat_DSR$Haul.Year<=endyr&T6dat_DSR$Haul.Year>=begyr,]

T6dat2_DSR<-ddply(T6dat_DSR[T6dat_DSR$Haul.FMP.Subarea!="SE",],c("Haul.FMP.Subarea","Sample.Species.Name"),summarize,Catch=max(tot_sum))

T6OFL_DSR<-sum(T6dat2_DSR$Catch)
T6ABC_DSR<-ddply(T6dat2_DSR,c("Haul.FMP.Subarea"),summarize,ABC=sum(Catch)*0.75)
colnames(T6ABC_DSR)[1]<-"REG_AREA"

T6ABC_DSR$REG_AREA<-gsub("CG","CENTRAL GOA",T6ABC_DSR$REG_AREA)
T6ABC_DSR$REG_AREA<-gsub("WG","WESTERN GOA",T6ABC_DSR$REG_AREA)
T6ABC_DSR$REG_AREA<-gsub("SE","EYAK",T6ABC_DSR$REG_AREA)
T6ABC_DSR$REG_AREA<-gsub("WY","WYAK",T6ABC_DSR$REG_AREA)

T6ABC_DSR$Apport<-0
T6ABC_DSR$Group<-"T6"
T6ABC_DSR$OFL<-T6OFL_DSR

EGOA<-c("EASTERN GOA",T6ABC_DSR[T6ABC_DSR$REG_AREA=="WYAK",c("ABC")],0,"T6",T6OFL_DSR)
T6ABC_DSR<-rbind(T6ABC_DSR,EGOA)

#Tier 6 - new species switched from Tier 5 to 6 in 2019 assessment
T6_OR<-c("SHORTBELLY ROCKFISH","AURORA ROCKFISH")
T6dat_OR<-OROX_catch[OROX_catch$Sample.Species.Name%in%T6_OR,]
T6dat_OR<-T6dat_OR[T6dat_OR$Haul.Year<=endyr&T6dat_OR$Haul.Year>=begyr,]

T6dat2_OR<-ddply(T6dat_OR,c("Haul.FMP.Subarea","Sample.Species.Name"),summarize,Catch=max(tot_sum))

T6OFL_OR<-sum(T6dat2_OR$Catch)
T6ABC_OR<-ddply(T6dat2_OR,c("Haul.FMP.Subarea"),summarize,ABC=sum(Catch)*0.75)
colnames(T6ABC_OR)[1]<-"REG_AREA"

T6ABC_OR$REG_AREA<-gsub("CG","CENTRAL GOA",T6ABC_OR$REG_AREA)
T6ABC_OR$REG_AREA<-gsub("WG","WESTERN GOA",T6ABC_OR$REG_AREA)
T6ABC_OR$REG_AREA<-gsub("SE","EYAK",T6ABC_OR$REG_AREA)
T6ABC_OR$REG_AREA<-gsub("WY","WYAK",T6ABC_OR$REG_AREA)

T6ABC_OR$Apport<-0
T6ABC_OR$Group<-"T6"
T6ABC_OR$OFL<-T6OFL_OR

WGOA<-c("WESTERN GOA",0,0,"T6",T6OFL_OR) #this is necessary because there is no catch in the WGOA
EGOA<-c("EASTERN GOA",0,0,"T6",T6OFL_OR) 
EY<-c("EYAK",0,0,"T6",T6OFL_OR)
WY<-c("WYAK",0,0,"T6",T6OFL_OR)
#EGOA<-c("EASTERN GOA", #save this for if years gets expanded and a few more catches come into the equation
#          as.numeric(T6ABC_OR[T6ABC_OR$REG_AREA=="WYAK",c("ABC")]),
#        0,"T6",T6OFL_OR) #there is not EY, so EGOA=WY

T6ABC_OR<-rbind(T6ABC_OR,WGOA,EGOA,EY,WY)

#combined T6
T6all<-rbind(T6ABC_OR,T6ABC_DSR)
T6all$ABC<-as.numeric(T6all$ABC)
T6<-ddply(T6all[,c("REG_AREA","ABC")],c("REG_AREA"),numcolwise(sum))
T6$Apport<-0
T6$Group<-"T6"
T6$OFL<-mean(as.numeric(T6ABC_DSR$OFL))+mean(as.numeric(T6ABC_OR$OFL))

T6$Biom_est<-0
T6<-T6[,c("REG_AREA","Biom_est","ABC","Apport","Group","OFL")]
T6GOA<-c("GOA",0,sum(T6[T6$REG_AREA!="EYAK"&T6$REG_AREA!="WYAK",]$ABC),
              0,"T6",
              mean(T6[T6$REG_AREA!="EYAK"&T6$REG_AREA!="WYAK",]$OFL))
T6<-rbind(T6,T6GOA)
T6$OFL<-as.numeric(T6$OFL)

#################################################
###############################################
#Summary table for whole complex
OROX_recs<-rbind(SCT5AppABC,T6)

OROX_OFL<-sum(unique(as.numeric(as.character(OROX_recs$OFL))))
OROX_ABC<-ddply(OROX_recs[OROX_recs$REG_AREA!="GOA"&OROX_recs$REG_AREA!="EYAK"&OROX_recs$REG_AREA!="WYAK",],
                c("REG_AREA"),summarize,Biom_est=sum(as.numeric(as.character(Biom_est))),
                ABC=sum(as.numeric(as.character(ABC))))


OROX_ABC$Apport<-0
OROX_ABC$Group<-"Complex"
OROX_ABC$OFL<-OROX_OFL
OROX_GOA<-c("GOA",sum(SCend[SCend$REG_AREA=="GOA","Biom_est"],sum(WTMend$Biom_est)),
            sum(OROX_ABC$ABC),0,"Complex",unique(OROX_ABC$OFL))
OROX_ABC<-rbind(OROX_ABC,OROX_GOA)

OROX_recs<-rbind(OROX_recs,OROX_ABC)

write.csv(OROX_recs, paste("OROX_harvestrecs_",AYR,".csv",sep=""),row.names=F)
