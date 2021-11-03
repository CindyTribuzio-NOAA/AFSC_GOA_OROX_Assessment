# Compute RACE Trawl Survey Biomass ----
#Updated 10/5/2021 by C. Tribuzio

#Setup ----
codedir<-datadir<-paste(getwd(),"/Code/RACE_Biomass/",sep="")
source(paste(codedir,"RACE_biomass_functions.R",sep=""))

#SYR<-2021 #survey year
#AYR<-2021 #assessment year
datadir<-paste(getwd(),"/Data/Annual_updates/",AYR,sep="")
outdir<-paste(getwd(),"/Output/",AYR,"/RACE_Biomass/",sep="")


#test for presence of correct folder and create it if it doesn't exist
dir.create(outdir, showWarnings = T)

# M-group Biomass ----
#makes a nested list that the biomass for loop will run through
#if a DSR species is ever added to this, will need to fix the biomass code to remove EGOA biomass

bgroups<-list("species"=list(spec1=list(30100,30170,30190,30200,30220,30240,30260,30340,30350,30400,30430,30475,30490,30535,30550,30600),
                             spec2=30560,
                             spec3=30430,
                             spec4=30535,
                             spec5=list(30170,30240,30200),
                             spec6=list(30190,30260,30340,30350,30400,30475,30490,30550,30600),
                             spec7=list(30100,30220),
                             spec8=list(30100,30170,30190,30200,30220,30240,30260,30340,30350,30400,30430,30475,30490,30535,30550,30560,30600)),
     "outname"=list(out1="OROX_allnoSC",
                    out2="OROX_SC",
                    out3="OROX_M01",
                    out4="OROX_M0092",
                    out5="OROX_M007",
                    out6="OROX_M006",
                    out7="OROX_M005",
                    out8="OROX_all"))

RACE_BIOMASS(Species=bgroups,outname="GOA_OROX",SYR=SYR,datadir=datadir,outdir=outdir)

# Biomass by species ----
#when run separately like this, it can take a lot of time because each run reloads the data
# need to fix so DSR species "GOA" wide estimates do not include SE, matters for CV calcs

bgroups_spec<-list("species"=list(spec1=30100,
                                  spec2=30170,
                                  spec3=30190,
                                  spec4=30200,
                                  spec5=30220,
                                  spec6=30240,
                                  spec7=30260,
                                  spec8=30340,
                                  spec9=30350,
                                  spec10=30400,
                                  spec11=30430,
                                  spec12=30475,
                                  spec13=30490,
                                  spec14=30535,
                                  spec15=30550,
                                  spec16=30560,
                                  spec17=30600,
                                  spec18=30470,
                                  spec19=30090,
                                  spec20=30410,
                                  spec21=30370,
                                  spec22=30120,
                                  spec23=30320,
                                  spec24=30380,
                                  spec25=30290,
                                  spec26=30270),
                   "outname"=list(out1="Silvergray_Rockfish",
                                  out2="Darkblotched_Rockfish",
                                  out3="Splitnose_Rockfish",
                                  out4="Greenstriped_Rockfish",
                                  out5="Widow_Rockfish",
                                  out6="Yellowtail_Rockfish",
                                  out7="Chilipepper_Rockfish",
                                  out8="Blackgill_Rockfish",
                                  out9="Vermilion_Rockfish",
                                  out10="Bocaccio",
                                  out11="Redstripe_Rockfish",
                                  out12="Redbanded_Rockfish",
                                  out13="Stripetail_Rockfish",
                                  out14="Harlequin_Rockfish",
                                  out15="Pygmy_Rockfish",
                                  out16="Sharpchin_Rockfish",
                                  out17="Yellowmouth_Rockfish",
                                  out18="Yelloweye_Rockfish",
                                  out19="Aurora_Rockfish",
                                  out20="Canary_Rockfish",
                                  out21="China_Rockfish",
                                  out22="Copper_Rockfish",
                                  out23="Quillback_Rockfish",
                                  out24="Tiger_Rockfish",
                                  out25="Shortbelly_Rockfish",
                                  out26="Rosethorn_Rockfish"))
RACE_BIOMASS(Species=bgroups_spec,outname="GOA_OROX_SPECIES",SYR=SYR,datadir=datadir,outdir=outdir)

#GOA DSR Rockfish ----
bgroups_DSR<-list("species"=list(spec1=30120,
                                  spec2=30270,
                                  spec3=30320,
                                  spec4=30380,
                                  spec5=30410,
                                  spec6=30470),
                   "outname"=list(out1="Copper_Rockfish",
                                  out2="Rosethorn_Rockfish",
                                  out3="Quillback_Rockfish",
                                  out4="Tiger_Rockfish",
                                  out5="Canary_Rockfish",
                                  out6="Yelloweye_Rockfish"))

RACE_BIOMASS(Species=bgroups_DSR,outname="GOA_DSR_SPECIES",SYR=SYR,datadir=datadir,outdir=outdir)


#GOA Sharpchin ----
bgroups_SC<-list("species"=list(spec1=30560),
                 "outname"=list(out1="Sharpchin_Rockfish"))

RACE_BIOMASS(Species=bgroups_SC,outname="GOA_SC",SYR=SYR,datadir=datadir,outdir=outdir)

# GOA M=0.0092 ---- 
#just harlequin
bgroups_M92<-list("species"=list(spec1=30535),
              "outname"=list(out1="OROX_M0092"))
RACE_BIOMASS(Species=bgroups_M92,outname="GOA_M0092",SYR=SYR,datadir=datadir,outdir=outdir)

# Detach plyr package, need to fix in next iteration to not even use plyr
detach(package:plyr)
