# Setup ----

#regional is for if you want the RFX model to run on the sub areas in the AI and GOA, if not, set to F

datadir<-paste(getwd(),"/Output/",AYR,"/RACE_Biomass/",sep="")
outdir<-paste(getwd(),"/Output/",AYR,"/RFX/",sep="")
codedir<-paste(getwd(),"/Code/RFX",sep="")
admboutdir<-paste(outdir,"ADMB_outputs",sep="")

#test for presence of correct folder and create it if it doesn't exist
dir.create(outdir, showWarnings = T)
dir.create(admboutdir, showWarnings = T)

source(paste(codedir,"/RFX_functions.R",sep=""))

RFX_fx(outname="GOA_OROX",AYR,endyr,datadir,outdir,admboutdir,regional=T)

RFX_fx(outname="GOA_OROX_SPECIES",AYR,endyr,datadir,outdir,admboutdir,regional=T)

RFX_fx(outname="GOA_SC",AYR,endyr,datadir,outdir,admboutdir,regional=T)

RFX_fx(outname="GOA_M0092",AYR,endyr,datadir,outdir,admboutdir,regional=T)
