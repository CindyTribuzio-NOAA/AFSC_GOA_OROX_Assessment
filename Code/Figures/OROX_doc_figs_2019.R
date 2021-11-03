#setwd("\\\\nmfs.local/AKC-ABL/Users/cindy.tribuzio/My Documents/SAFE/2017/OROX/R work")
library(ggplot2)
library(reshape2)
library(reshape)
library(RColorBrewer)
library(plyr)
library(gridExtra)
library(gtable)
library(grid)

pdir<-pdir<-dirname(getwd())

#######################################
#set up themes for ggplot
theme_pres<- function(base_size = 12, base_family = "Helvetica") { #this function sets the theme for the whole figure
  theme_bw(base_size = base_size, base_family = base_family) %+replace% #also note that this creates a bunch of font warnings that are not a real problem, I just haven't dealt with it yet
    theme(
      axis.line.x=element_line(size=1,color = '#FFFFCC'),
      axis.line.y=element_line(size=1,color = '#FFFFCC'),
      axis.text=element_text(size=rel(1),colour='#FFFFCC',face="bold"),
      axis.ticks=element_line(colour='#FFFFCC'),
      axis.title.y=element_text(colour='#FFFFCC',face="bold",angle=90),
      axis.title.x=element_text(colour='#FFFFCC',face="bold"),
      plot.title=element_text(size=rel(2),colour='#FFFFCC',face="bold",hjust = 0.5),
      plot.background=element_rect(fill="transparent",color=NA),
      panel.grid.major=element_blank(),
      panel.grid.minor=element_blank(),
      panel.border = element_blank(),
      panel.background = element_blank(),
      legend.background=element_blank(),
      legend.text=element_text(colour='#FFFFCC',size=12),
      legend.title=element_text(colour='#FFFFCC',size=12),
      strip.background=element_blank(),
      strip.text=element_text(size=12,colour='#FFFFCC',face="bold")
    )
}

theme_doc<- function(base_size = 12, base_family = "Helvetica") { #this function sets the theme for the whole figure
  theme_bw(base_size = base_size, base_family = base_family) %+replace% #also note that this creates a bunch of font warnings that are not a real problem, I just haven't dealt with it yet
    theme(
      axis.line.x = element_line(),
      axis.line.y = element_line(),
      axis.text=element_text(size=rel(1),colour='black'),
      axis.ticks=element_line(colour='black'),
      axis.title.y=element_text(colour='black',face="bold",angle=90),
      axis.title.x=element_text(colour='black',face="bold"),
      plot.title=element_text(size=rel(2),colour='black',face="bold",hjust = 0.5),
      plot.background=element_blank(),
      panel.grid.major=element_blank(),
      panel.grid.minor=element_blank(),
      panel.border = element_blank(),
      panel.background = element_blank(),
      legend.background=element_blank(),
      legend.text=element_text(colour='black',size=12),
      legend.title=element_text(colour='black',size=12),
      strip.background=element_blank(),
      strip.text=element_text(size=12,colour='black',face="bold")
    )
}

SYR<-2019
#########################################################################
########################################################################
#Catch FIGS
#bringing catch in
catchdat<-read.csv(paste(getwd(),"/OROXcomplex_catch",SYR,".csv",sep=""), header=T)
colnames(catchdat)
#[1] "Haul.FMP.Subarea"    "Haul.Year"           "Sample.Species.Name" "discard_wt"          "retained_wt"         "tot_sum"
codes<-read.csv(paste(getwd(),"/ORocks_codes.csv",sep=""), header=T)

catchdat<-merge(catchdat,codes[,c("NORPAC_name","RACE_code")],by.x="Sample.Species.Name",by.y="NORPAC_name")

levels(catchdat$Haul.FMP.Subarea)[levels(catchdat$Haul.FMP.Subarea)=="CG"]<-"CENTRAL GOA"
levels(catchdat$Haul.FMP.Subarea)[levels(catchdat$Haul.FMP.Subarea)=="WG"]<-"WESTERN GOA"
levels(catchdat$Haul.FMP.Subarea)[levels(catchdat$Haul.FMP.Subarea)=="SE"]<-"SOUTHEAST"
levels(catchdat$Haul.FMP.Subarea)[levels(catchdat$Haul.FMP.Subarea)=="WY"]<-"WYAK"
levels(catchdat$Haul.FMP.Subarea)[levels(catchdat$Haul.FMP.Subarea)=="PWSI"]<-"PWSI"
levels(catchdat$Haul.FMP.Subarea)[levels(catchdat$Haul.FMP.Subarea)=="SEI"]<-"SEI"

catchdat$Haul.FMP.Subarea<-factor(catchdat$Haul.FMP.Subarea,levels=c("WESTERN GOA","CENTRAL GOA","WYAK","SOUTHEAST","PWSI","SEI"))
cd2<-ddply(catchdat,c("Haul.FMP.Subarea","Haul.Year"),summarize,Catch=sum(tot_sum))
cd2<-cd2[cd2$Haul.FMP.Subarea!="PWSI"&cd2$Haul.FMP.Subarea!="SEI",]

cd2$Haul.FMP.Subarea<-factor(cd2$Haul.FMP.Subarea,levels=c("WESTERN GOA","CENTRAL GOA","WYAK","SOUTHEAST","PWSI","SEI"))

cd2$fakeorder<-ifelse(cd2$Haul.FMP.Subarea=="WESTERN GOA",4,
                          ifelse(cd2$Haul.FMP.Subarea=="CENTRAL GOA",3,
                                 ifelse(cd2$Haul.FMP.Subarea=="WYAK",2,1)))
cd2<-cd2[order(cd2$fakeorder),]

catch_area_doc<-ggplot(cd2, aes(x=Haul.Year,y=Catch,fill=Haul.FMP.Subarea,order=-as.numeric(Haul.FMP.Subarea)))+ 
  geom_bar(stat="identity",position="stack")+
  scale_fill_manual(values=rev(brewer.pal(n=length(unique(cd2$Haul.FMP.Subarea)),"Greens")),name="Area")+
  scale_y_continuous(expand=c(0,0))+
  coord_cartesian(ylim=c(0,1400))+
  labs(y="",x="")+
  theme_doc()+theme(plot.margin=unit(c(0,2.15,0,0.2), "cm"),
                    axis.line.x = element_line(),
                    axis.line.y = element_line())
catch_area_pres<-ggplot(cd2, aes(x=Haul.Year,y=Catch,fill=Haul.FMP.Subarea,order=-as.numeric(Haul.FMP.Subarea)))+ 
  geom_bar(stat="identity",position="stack")+
  scale_fill_manual(values=rev(brewer.pal(n=length(unique(catchdat$Haul.FMP.Subarea)),"Greens")),name="Area")+
  scale_y_continuous(expand=c(0,0))+
  coord_cartesian(ylim=c(0,1400))+
  labs(y="",x="")+
  theme_pres()+theme(plot.margin=unit(c(0,2.7,0,0), "cm"))

#catch by species with top species
top_spec<-catchdat[which(catchdat$RACE_code==30100|catchdat$RACE_code==30560|catchdat$RACE_code==30535
                        |catchdat$RACE_code==30430|catchdat$RACE_code==30470|catchdat$RACE_code==30475),]
nottop<-catchdat[-which(catchdat$RACE_code==30100|catchdat$RACE_code==30560|catchdat$RACE_code==30535
                       |catchdat$RACE_code==30430|catchdat$RACE_code==30470|catchdat$RACE_code==30475),]
nottop1<-cbind(nottop[,1:3],nottop[,6])
colnames(nottop1)<-c("Species","Area","Year","Catch" )
nottop1a<-melt(nottop1,id=c("Year","Area","Species"))
nottop2<-as.data.frame(cast(nottop1a,Year+Area~variable,sum))
top_spec1<-cbind(top_spec[,1:3],top_spec[,6])
colnames(top_spec1)<-c("Species","Area","Year","Catch" )
top_spec2<-top_spec1[order(top_spec1$Year,top_spec1$Area,top_spec1$Species),]

nottop3<-as.data.frame(cbind("MINORS",as.character(nottop2[,2]),nottop2[,1],nottop2[,3]))
colnames(nottop3)<-c("Species","Area","Year","Catch")
top_spec3<-rbind(top_spec2,nottop3)
top_spec3$Catch<-as.numeric(top_spec3$Catch)
top_spec3$Year<-as.numeric(top_spec3$Year)

ts4<-ddply(top_spec3,c("Species","Year"),summarize,catch=sum(Catch))

ts4$fakeorder<-ifelse(ts4$Species=="HARLEQUIN ROCKFISH",7,
                      ifelse(ts4$Species=="RED BANDED ROCKFISH",6,
                             ifelse(ts4$Species=="REDSTRIPE ROCKFISH",5,
                                    ifelse(ts4$Species=="SHARPCHIN ROCKFISH",4,
                                           ifelse(ts4$Species=="SILVERGRAY ROCKFISH",3,
                                                  ifelse(ts4$Species=="YELLOWEYE ROCKFISH",2,1))))))
ts4<-ts4[order(ts4$fakeorder),]

#Catch by species for doc
catch_species_doc<-ggplot(ts4, aes(x=Year, y=catch,fill=Species,order=-as.numeric(Species))) + 
  geom_bar(stat="identity")+
  scale_fill_manual(values=rev(brewer.pal(n=length(unique(top_spec3$Species)),"Blues")),name="Species")+
  scale_y_continuous(expand=c(0,0))+
  coord_cartesian(ylim=c(0,1400))+
  labs(y="",x="")+
  theme_doc()
catch_species_pres<-ggplot(ts4, aes(x=Year, y=catch,fill=Species,order=-as.numeric(Species))) + 
  geom_bar(stat="identity")+
  scale_fill_manual(values=rev(brewer.pal(n=length(unique(top_spec3$Species)),"Blues")),name="Species")+
  scale_y_continuous(expand=c(0,0))+
  coord_cartesian(ylim=c(0,1400))+
  labs(y="",x="")+
  theme_pres()

catchfig_doc<-grid.arrange(arrangeGrob(catch_area_doc, catch_species_doc, nrow = 2,
                                left = textGrob("Catch (t)", rot = 90, vjust = 1.5,gp=gpar(col="black", fontsize=20)),
                                bottom = textGrob("Year", vjust=0,gp=gpar(col="black", fontsize=20))))
ggsave("OROX_catch2_doc.png",plot=catchfig_doc,dpi=600, bg="transparent",width=15,height=9)

catchfig_pres<-grid.arrange(arrangeGrob(catch_area_pres, catch_species_pres, nrow = 2,
                                       left = textGrob("Catch (t)", rot = 90, vjust = 1.5,gp=gpar(col="#FFFFCC", fontsize=20)),
                                       bottom = textGrob("Year", vjust=0,gp=gpar(col="#FFFFCC", fontsize=20))))
ggsave("OROX_catch2_pres.png",plot=catchfig_pres,dpi=600, bg="transparent",width=15,height=9)


#Catch by species and area, W/CGOA only
ABC<-as.data.frame(matrix(c("WESTERN GOA","CENTRAL GOA",55,1479),nrow=2))
colnames(ABC)<-c("Area","ABC")
ABC$ABC<-as.numeric(as.character(ABC$ABC))
ann_text <- data.frame(Year = 2011,Catch = 991,lab = "2014 Apportioned ABC", Area = factor("WESTERN GOA",levels = c("WESTERN GOA","CENTRAL GOA")))
catch_species_area<-ggplot(top_spec3[top_spec3$Area=="WESTERN GOA"|top_spec3$Area=="CENTRAL GOA",], 
                      aes(x=Year, y=Catch,fill=Species,order=-as.numeric(Species))) + 
  geom_bar(stat="identity")+
  geom_hline(data=ABC,aes(yintercept=ABC,color="red"),linetype="dashed",size=2,show_guide=F)+
  scale_fill_manual(values=rev(brewer.pal(n=length(unique(top_spec3$Species)),"Blues")),name="Species")+
  scale_y_continuous(expand=c(0,0))+
  #coord_cartesian(ylim=c(0,1500))+
  facet_wrap(c("Area"))+
  labs(y="",x="")+
  theme_doc()+theme(legend.position=c(0.1,0.75))
ggsave("OROX_WCGOA.png",plot=catch_species_area,dpi=600, bg="transparent",width=15,height=6)







#catch by area for top 6 species
Ctop_spec<-top_spec
Ctop_spec$Sample.Species.Name<-gsub( " ROCKFISH", "",top_spec$Sample.Species.Name)
#don't forget to remove PWSI and SEI areas
Ctop_spec<-Ctop_spec[Ctop_spec$Haul.FMP.Subarea!="PWSI",]
Ctop_spec<-Ctop_spec[Ctop_spec$Haul.FMP.Subarea!="SEI",]

catch_area_doc<-ggplot(Ctop_spec, aes(x=Haul.Year,y=tot_sum,fill=Haul.FMP.Subarea,order=-as.numeric(Haul.FMP.Subarea)))+ 
  geom_bar(stat="identity",position="fill")+
  scale_fill_manual(values=rev(brewer.pal(n=length(unique(catchdat$Haul.FMP.Subarea)),"Greens")),name="Area")+
  coord_cartesian(ylim=c(0,1),expand=c(0,0))+
  labs(y="Catch (t)",x="Year")+
  facet_grid(Sample.Species.Name~.)+
  scale_y_continuous(breaks=c(0.25,0.5,0.75,1.00))+
  theme_bw()+
  theme(plot.background=element_blank(),panel.grid.major=element_blank(),panel.grid.minor=element_blank()
        ,panel.border = element_blank(),panel.background = element_blank(),axis.title.y=element_text(colour='black',face="bold"),
        axis.title.x=element_text(colour='black',face="bold"),axis.text.y=element_text(colour='black'),axis.text.x=element_text(colour='black'),
        plot.title=element_text(size=rel(2),colour='black',face="bold"),axis.ticks=element_line(colour='black'),axis.line=element_line(size=1),
        axis.text=element_text(size=rel(1.2)),strip.text.x=element_text(colour="black",face="bold",size=15),
        strip.text.y=element_text(colour="black",size=12,face="bold"),legend.background=element_blank(),
        legend.text=element_text(colour='black'),legend.title=element_text(colour='black')) +
  theme(axis.line = element_line(color = 'black'))
ggsave("OROX_catch_area_doc.png",plot=catch_area_doc,dpi=600, bg="transparent",width=7,height=8)

#######################################################################
##########Biomass Figs
#good website for colors: http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/
#biomass figs

#data downloaded from AKFIN RACE survey Biomass by regulatory area
datafile<-read.csv(paste(getwd(),"/Biomass by Regulatory Area - GOA.csv",sep=""), header=T) #from access file, if R sql doesn't work
dat2<-melt(as.data.frame(datafile[,c("Year","Regulatory.Area.Name","Species.Code","Area.Biomass","Area.Biomass.Var")]),id=c("Year","Regulatory.Area.Name","Species.Code" ),value="Area.Biomass")
colnames(dat2)
#[1] "Year"                 "Regulatory.Area.Name" "Species.Code"         "variable"             "value"


areadat<-as.data.frame(dcast(dat2,Year+Regulatory.Area.Name~variable,sum))


cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7") #colorblind friendly palette

#biomass by area
areadat$Regulatory.Area.Name<-factor(areadat$Regulatory.Area.Name,levels=c("WESTERN GOA","CENTRAL GOA","EASTERN GOA"))

areadat$fakeorder<-ifelse(areadat$Regulatory.Area.Name=="WESTERN GOA",3,
                          ifelse(areadat$Regulatory.Area.Name=="CENTRAL GOA",2,1))
areadat<-areadat[order(areadat$fakeorder),]

Bareadoc<-ggplot(areadat, aes(x=Year,y=Area.Biomass/1000,fill=Regulatory.Area.Name,order=-as.numeric(Regulatory.Area.Name)))+ 
  geom_bar(stat="identity",position="stack")+
  scale_fill_manual(values=rev(brewer.pal(n=length(unique(dat2$Regulatory.Area.Name)),"Greens")),name="Area")+
  scale_y_continuous(expand=c(0,0),limits=c(0,200))+
  labs(y="",x="")+
  theme_doc()
Bareapres<-ggplot(areadat, aes(x=Year,y=Area.Biomass/1000,fill=Regulatory.Area.Name,order=-as.numeric(Regulatory.Area.Name)))+ 
  geom_bar(stat="identity",position="stack")+
  scale_fill_manual(values=rev(brewer.pal(n=length(unique(dat2$Regulatory.Area.Name)),"Greens")),name="Area")+
  scale_y_continuous(expand=c(0,0),limits=c(0,200))+
  labs(y="",x="")+
  theme_pres()

#biomass by species
codes<-read.csv(paste(getwd(),"/ORocks_codes.csv",sep=""), header=T)
#by species with top species
top_spec<-dat2[which(dat2$Species.Code==30100|dat2$Species.Code==30560|dat2$Species.Code==30535
                         |dat2$Species.Code==30430|dat2$Species.Code==30470|dat2$Species.Code==30475),]
nottop<-dat2[-which(dat2$Species.Code==30100|dat2$Species.Code==30560|dat2$Species.Code==30535
                        |dat2$Species.Code==30430|dat2$Species.Code==30470|dat2$Species.Code==30475),]
top_spec<-merge(top_spec,codes[,c("RACE_code","NORPAC_name")],by.x="Species.Code",by.y="RACE_code")
top_spec$NORPAC_name<-gsub( " ROCKFISH", "",top_spec$NORPAC_name)
top_spec2<-ddply(top_spec[top_spec$variable=="Area.Biomass",], c("Year","NORPAC_name"),summarize,biom=sum(value))

nottop<-merge(nottop,codes[,c("RACE_code","NORPAC_name")],by.x="Species.Code",by.y="RACE_code")
nottop2<-ddply(nottop[nottop$variable=="Area.Biomass",], c("Year"),summarize,biom=sum(value))
nottop2$NORPAC_name<-"Minors"
nottop2<-nottop2[,c("Year","NORPAC_name","biom")]
top_spec3<-rbind(top_spec2,nottop2)

#biomass by species for doc
top_spec3$NORPAC_name<-factor(top_spec3$NORPAC_name,levels=c("HARLEQUIN","RED BANDED","REDSTRIPE","SHARPCHIN","SILVERGRAY","YELLOWEYE","Minors"))
#have to fake order the data so that the bars stack in the correct order, data has to be last first
top_spec3$fake<-ifelse(top_spec3$NORPAC_name=="HARLEQUIN",7,
                       ifelse(top_spec3$NORPAC_name=="RED BANDED",6,
                              ifelse(top_spec3$NORPAC_name=="REDSTRIPE",5,
                                     ifelse(top_spec3$NORPAC_name=="SHARPCHIN",4,
                                            ifelse(top_spec3$NORPAC_name=="SILVERGRAY",3,
                                                   ifelse(top_spec3$NORPAC_name=="YELLOWEYE",2,1))))))
top_spec3<-top_spec3[order(top_spec3$fake),]

Bspecdoc<-ggplot(top_spec3, aes(x=Year, y=biom/1000,fill=NORPAC_name,order=-as.numeric(NORPAC_name))) + 
  geom_bar(stat="identity")+
  scale_fill_manual(values=rev(brewer.pal(n=length(unique(top_spec3$NORPAC_name)),"Blues")),name="Species")+
  scale_y_continuous(expand=c(0,0),limits=c(0,200))+
  labs(y="",x="")+
  theme_doc()
Bspecpres<-ggplot(top_spec3, aes(x=Year, y=biom/1000,fill=NORPAC_name,order=-as.numeric(NORPAC_name))) + 
  geom_bar(stat="identity")+
  scale_fill_manual(values=rev(brewer.pal(n=length(unique(top_spec3$NORPAC_name)),"Blues")),name="Species")+
  scale_y_continuous(expand=c(0,0),limits=c(0,200))+
  labs(y="",x="")+
  theme_pres()

biomfig_doc<-grid.arrange(arrangeGrob(Bareadoc, Bspecdoc, nrow = 2,
                                   left = textGrob("Biomass (1000s t)", rot = 90, vjust = 1,gp=gpar(col="black", fontsize=20)),
                                   bottom = textGrob("Year", vjust=0,gp=gpar(col="black", fontsize=20))))
ggsave("OROX_biomass_doc.png",plot=biomfig_doc,dpi=600, bg="transparent",width=15,height=9)

biomfig_pres<-grid.arrange(arrangeGrob(Bareapres, Bspecpres, nrow = 2,
                                      left = textGrob("Biomass (1000s t)", rot = 90, vjust = 1,gp=gpar(col="#FFFFCC", fontsize=20)),
                                      bottom = textGrob("Year", vjust=0,gp=gpar(col="#FFFFCC", fontsize=20))))
ggsave("OROX_biomass_pres.png",plot=biomfig_pres,dpi=600, bg="transparent",width=15,height=9)


#Exploitation Rates, leave this out because catch is so much greater than biomass

#biomass 
Btop_specarea<-ddply(top_spec[top_spec$variable=="Area.Biomass",],
                     c("Year","NORPAC_name","Regulatory.Area.Name"),
                     summarize,biom=sum(value))
#Btop_specarea$Source<-"Biomass"
Btop_specarea<-Btop_specarea[Btop_specarea$NORPAC_name!="YELLOWEYE",]

#take out yelloweye and build it from different data set
YEbiom<-read.csv("Biomass by NMFS Reporting Area - GOA_YE.csv",header=T)
STRAT<-read.csv("STRATUM2017.csv",header=T)
Yb<-merge(YEbiom,unique(STRAT[STRAT$Summary.Area.Code>900&STRAT$Summary.Area.Code<1000,
                              c("Summary.Area.Code","Stratum.Regulatory.Area.Name")]),
          by.x="Summary.Area",by.y="Summary.Area.Code",all.x=T)
Yb2<-ddply(Yb,c("Year","Stratum.Regulatory.Area.Name"),summarize,biom=sum(Area.Biomass))
Yb2$NORPAC_name<-"YELLOWEYE"
#Yb2$Source<-"Biomass"

Yb3<-Yb2[,c("Year","NORPAC_name","Stratum.Regulatory.Area.Name","biom")]
colnames(Yb3)<-colnames(Btop_specarea)

Ball<-rbind(Btop_specarea,Yb3)

#catch dat = Ctop_spec
Ctop_specarea<-Ctop_spec[,c("Haul.Year","Sample.Species.Name","Haul.FMP.Subarea","tot_sum")]
#Ctop_specarea$Source<-"Catch"
colnames(Ctop_specarea)<-colnames(Btop_specarea)
colnames(Ctop_specarea)[4]<-"Catch_t"
EGOA<-ddply(Ctop_specarea[Ctop_specarea$Regulatory.Area.Name=="SOUTHEAST"|Ctop_specarea$Regulatory.Area.Name=="WYAK",],
      c("Year","NORPAC_name"),summarize,Catch_t=sum(Catch_t))
EGOA$Regulatory.Area.Name<-"EASTERN GOA"
EGOA$Source<-"Catch"
EG2<-EGOA[,c("Year","NORPAC_name","Regulatory.Area.Name","Catch_t")]

Cts<-Ctop_specarea[Ctop_specarea$Regulatory.Area.Name!="SOUTHEAST",]
Cts<-Cts[Cts$Regulatory.Area.Name!="WYAK",]

Cts2<-rbind(Cts,EG2)

#combine sources
BCdat<-merge(Cts2,Ball,by=c("Year","NORPAC_name","Regulatory.Area.Name"))
BCdat$exploit<-BCdat$Catch_t/(BCdat$biom/1000)

#I want this fig, but with Catch on LEft and Biomass on Right
ggplot(BCdat, aes(x=as.factor(Year),y=biom,fill=Regulatory.Area.Name,order=-as.numeric(Regulatory.Area.Name)))+ 
  geom_bar(stat="identity",position="fill")+
  scale_fill_manual(values=rev(brewer.pal(n=length(unique(BCdat$Regulatory.Area.Name)),"Greens")),name="Area")+
  coord_cartesian(ylim=c(0,1))+
  labs(y="Proportion",x="Year")+
  facet_grid(NORPAC_name~Source,scales="free")+
  scale_y_continuous(breaks=c(0.25,0.5,0.75,1.00))+
  theme_bw()+
  theme(plot.background=element_blank(),panel.grid.major=element_blank(),panel.grid.minor=element_blank()
        ,panel.border = element_blank(),panel.background = element_blank(),axis.title.y=element_text(colour='black',face="bold"),
        axis.title.x=element_text(colour='black',face="bold"),axis.text.y=element_text(colour='black'),axis.text.x=element_text(colour='black'),
        plot.title=element_text(size=rel(2),colour='black',face="bold"),axis.ticks=element_line(colour='black'),axis.line=element_line(size=1),
        axis.text=element_text(size=rel(1.2)),strip.text.x=element_text(colour="black",face="bold",size=15),
        strip.text.y=element_text(colour="black",size=12,face="bold"),legend.background=element_blank(),
        legend.text=element_text(colour='black'),legend.title=element_text(colour='black')) +
  theme(axis.line = element_line(color = 'black'))


#Random FX figures
RFX<-read.csv(paste(pdir,"/OROX_RFX_",SYR,"/RFX_Biomass.csv",sep=""),header=T)
SCRFX<-RFX[RFX$Group=="OROX_SC",]
SCRFX<-SCRFX[SCRFX$REG_AREA!="GOA",]

datafile<-read.csv(paste(getwd(),"/Biomass by Regulatory Area - GOA.csv",sep=""), header=T) #from access file, if R sql doesn't work
dat2<-melt(as.data.frame(datafile[,c("Year","Regulatory.Area.Name","Common.Name","Area.Biomass","Area.Biomass.Var")]),id=c("Year","Regulatory.Area.Name","Common.Name" ),value="Area.Biomass")
colnames(dat2)
biolist<-c("greenstriped rockfish", "harlequin rockfish","pygmy rockfish","redbanded rockfish",
           "redstripe rockfish","sharpchin rockfish","silvergray rockfish","splitnose rockfish ",  
            "vermilion rockfish","widow rockfish","yellowmouth rockfish","yellowtail rockfish")
dat2<-dat2[dat2$Common.Name%in%biolist,]
dat2<-droplevels(dat2)
areadat<-as.data.frame(dcast(dat2,Year+Regulatory.Area.Name+Common.Name~variable,sum))
areadat$stdev<-sqrt(areadat$Area.Biomass.Var)
areadat$UL<-areadat$Area.Biomass+1.96*sqrt(areadat$Area.Biomass.Var)
areadat$LL<-areadat$Area.Biomass-1.96*sqrt(areadat$Area.Biomass.Var)
twlB_SC<-areadat[areadat$Common.Name=="sharpchin rockfish",]
colnames(twlB_SC)[2]<-"REG_AREA"

#need to put data into dorder so it projects correctly
twlB_SC$REG_AREA<-factor(twlB_SC$REG_AREA,levels=c("WESTERN GOA","CENTRAL GOA","EASTERN GOA"))
twlB_SC$fakeorder<-ifelse(twlB_SC$REG_AREA=="WESTERN GOA",3,
                          ifelse(twlB_SC$REG_AREA=="CENTRAL GOA",2,1))
twlB_SC<-twlB_SC[order(twlB_SC$fakeorder),]

#get rid of outrageous values for the sake of graphing
twlB_SC[twlB_SC$LL<0,]$LL<-0 #turns all LL values below zero into zero for graph
#twlB_SC[twlB_SC$REG_AREA=="WESTERN GOA"&twlB_SC$Year==1987,]$UL<-5000

#SCRFX[SCRFX$REG_AREA=="WESTERN GOA"&SCRFX$Biom_UL>6000,]$Biom_UL<-NA #changes early super high UL in WGOA

cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7") #colorblind friendly palette
SCRFX$REG_AREA<-factor(SCRFX$REG_AREA,levels=c("WESTERN GOA","CENTRAL GOA","EASTERN GOA"))

annotation_custom2 <- function (grob, xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf, data) 
{
  layer(data = data, stat = StatIdentity, position = PositionIdentity, 
        geom = ggplot2:::GeomCustomAnn,
        inherit.aes = TRUE, params = list(grob = grob, 
                                          xmin = xmin, xmax = xmax, 
                                          ymin = ymin, ymax = ymax))
}

SC_inset<-ggplot(data=twlB_SC[twlB_SC$REG_AREA=="WESTERN GOA",],aes(x=Year,y=Area.Biomass/1000))+
  geom_point(size=3,color="black")+
  geom_errorbar(data=twlB_SC[twlB_SC$REG_AREA=="WESTERN GOA"&twlB_SC$Year>1987,],aes(x=Year,ymax=UL/1000,ymin=LL/1000), width=0.5,color="black")+
  geom_line(data=SCRFX[SCRFX$REG_AREA=="WESTERN GOA",],aes(x=YEAR,y=Biom_UL/1000),linetype="dashed",color="orange",size=1)+
  geom_line(data=SCRFX[SCRFX$REG_AREA=="WESTERN GOA",],aes(x=YEAR,y=Biom_est/1000),color="orange",size=1)+
  geom_line(data=SCRFX[SCRFX$REG_AREA=="WESTERN GOA",],aes(x=YEAR,y=Biom_LL/1000),linetype="dashed",color="orange",size=1)+
  facet_grid(REG_AREA~.,scales="free")+
  scale_y_continuous(expand=c(0,0),breaks=c(0,0.4))+
  scale_x_continuous(limits=c(1984,2020),expand=c(0,0),breaks=c(1985,1990,2000,2010))+
  coord_cartesian(ylim=c(0,0.65))+
  labs(y="",x="")+
  theme(
    axis.line.x = element_line(),
    axis.line.y = element_line(),
    axis.text=element_text(size=rel(1),colour='black'),
    axis.ticks=element_line(colour='black'),
    axis.title.y=element_text(colour='black',face="bold",angle=90,size=15),
    axis.title.x=element_text(colour='black',face="bold"),
    plot.title=element_text(size=rel(2),colour='black',face="bold",hjust = 0.5),
    plot.background=element_blank(),
    panel.grid.major=element_blank(),
    panel.grid.minor=element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    legend.background=element_blank(),
    legend.text=element_text(colour='black',size=12),
    legend.title=element_text(colour='black',size=12),
    strip.background=element_blank(),
    strip.text=element_blank()
  )

SC_doc<-ggplot(data=twlB_SC,aes(x=Year,y=Area.Biomass/1000))+
  geom_point(size=3,color="black")+
  geom_errorbar(data=twlB_SC,aes(x=Year,ymax=UL/1000,ymin=LL/1000), width=0.5,color="black")+
  geom_line(data=SCRFX,aes(x=YEAR,y=Biom_UL/1000),linetype="dashed",color="orange",size=1)+
  geom_line(data=SCRFX,aes(x=YEAR,y=Biom_est/1000),color="orange",size=1)+
  geom_line(data=SCRFX,aes(x=YEAR,y=Biom_LL/1000),linetype="dashed",color="orange",size=1)+
  facet_grid(REG_AREA~.,scales="free")+
  labs(y="Biomass (1000s t)",x="")+
  scale_y_continuous(expand=c(0,0))+
  scale_x_continuous(limits=c(1984,2020),expand=c(0,0),breaks=c(1985,1990,1995,2000,2005,2010,2015,2020))+
  ggtitle("Sharpchin")+
  annotation_custom2(grob = ggplotGrob(SC_inset), data=data.frame(REG_AREA="WESTERN GOA",Year=1, Area.Biomass=1),xmin=1990, xmax=2017, ymin=5, ymax=60)+
  theme(
    axis.line.x = element_line(),
    axis.line.y = element_line(),
    axis.text=element_text(size=rel(1),colour='black'),
    axis.ticks=element_line(colour='black'),
    axis.title.y=element_text(colour='black',face="bold",angle=90,size=15),
    axis.title.x=element_text(colour='black',face="bold"),
    plot.title=element_text(size=rel(2),colour='black',face="bold",hjust = 0.5),
    plot.background=element_blank(),
    panel.grid.major=element_blank(),
    panel.grid.minor=element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    legend.background=element_blank(),
    legend.text=element_text(colour='black',size=12),
    legend.title=element_text(colour='black',size=12),
    strip.background=element_blank(),
    strip.text=element_blank()
  )



allspecdat<-read.csv("GOA_Biomass_OROX_allnoSC.csv",header=T)
all2<-allspecdat[allspecdat$REGULATORY_AREA_NAME!="GOA",]
colnames(all2)[2]<-"REG_AREA"
all2$LL<-all2$Biomass-1.96*sqrt(all2$Variance)
all2$UL<-all2$Biomass+1.96*sqrt(all2$Variance)
all2$REG_AREA<-factor(all2$REG_AREA,levels=c("WESTERN GOA","CENTRAL GOA","EASTERN GOA"))
all2$fakeorder<-ifelse(all2$REG_AREA=="WESTERN GOA",3,
                          ifelse(all2$REG_AREA=="CENTRAL GOA",2,1))
all2<-all2[order(all2$fakeorder),]
all2[all2$LL<0,]$LL<-0 #turns all LL values below zero into zero for graph

allRFX<-RFX[RFX$Group=="OROX_allnoSC",]
allRFX<-allRFX[allRFX$REG_AREA!="GOA",]


#https://stackoverflow.com/questions/37867758/insetting-on-facet-grided-and-grid-arrangeed-plot

ORdoc<-ggplot(data=all2,aes(x=YEAR,y=Biomass/1000))+
  geom_point(size=3,color="black")+
  geom_errorbar(data=all2,aes(x=YEAR,ymax=UL/1000,ymin=LL/1000), width=0.5,color="black")+
  geom_line(data=allRFX,aes(x=YEAR,y=Biom_UL/1000),linetype="dashed",color="orange",size=1)+
  geom_line(data=allRFX,aes(x=YEAR,y=Biom_est/1000,colour=Group),color="orange",size=1)+
  geom_line(data=allRFX,aes(x=YEAR,y=Biom_LL/1000),linetype="dashed",color="orange",size=1)+
  facet_grid(REG_AREA~.,scales="free")+
  labs(y="",x="")+
  scale_y_continuous(expand=c(0,0))+
  scale_x_continuous(limits=c(1984,2020),expand=c(0,0),breaks=c(1985,1990,1995,2000,2005,2010,2015,2020))+
  ggtitle("Tier 5 Other Rockfish")+
  #annotation_custom2(grob = legend, data=data.frame(REG_AREA="WESTERN GOA",YEAR=1, Biomass=1),xmin=2010, xmax=Inf, ymin=40, ymax=Inf)+
  theme_doc()



fakedat<-data.frame(YEAR=c(1984,1984,1987,1987),Biomass=c(0,5,0,10),Group=c(1,2,1,2))
RFX_leg<-ggplot(data=fakedat,aes(x=YEAR,y=Biomass/1000))+
  geom_point(data=fakedat[fakedat$YEAR==1984,],aes(x=YEAR,y=Biomass/1000,color=as.factor(Group)),size=3,show.legend=T)+
  geom_line(data=fakedat[fakedat$YEAR==1987,],aes(x=YEAR,y=Biomass/1000,color=as.factor(Group)),size=1,linetype="solid",color="orange",show.legend = T)+
  scale_colour_manual(values = c("black","orange"),
                      guide = guide_legend(override.aes = list(linetype = c("blank", "solid"),shape = c(16,NA))),
                      labels=c("Survey","Rand FX"))+
  theme(legend.title=element_blank(),
        legend.key = element_blank(),
        legend.background=element_blank())
legend = gtable_filter(ggplot_gtable(ggplot_build(RFX_leg)), "guide-box") 

RFX_doc<-grid.arrange(arrangeGrob(SC_doc, ORdoc, ncol = 2,
                                       bottom = textGrob("Year", vjust=0,gp=gpar(col="black", fontsize=15,face="bold"))))
ggsave("RFX_doc.png",plot=RFX_doc,dpi=600, bg="transparent",width=15,height=9)

SC_inset_pres<-ggplot(data=twlB_SC[twlB_SC$REG_AREA=="WESTERN GOA",],aes(x=Year,y=Area.Biomass/1000))+
  geom_point(size=3,color="cornsilk")+
  geom_errorbar(data=twlB_SC[twlB_SC$REG_AREA=="WESTERN GOA"&twlB_SC$Year>1987,],aes(x=Year,ymax=UL/1000,ymin=LL/1000), width=0.5,color="cornsilk")+
  geom_line(data=SCRFX[SCRFX$REG_AREA=="WESTERN GOA",],aes(x=YEAR,y=Biom_UL/1000),linetype="dashed",color="orange",size=1)+
  geom_line(data=SCRFX[SCRFX$REG_AREA=="WESTERN GOA",],aes(x=YEAR,y=Biom_est/1000),color="orange",size=1)+
  geom_line(data=SCRFX[SCRFX$REG_AREA=="WESTERN GOA",],aes(x=YEAR,y=Biom_LL/1000),linetype="dashed",color="orange",size=1)+
  facet_grid(REG_AREA~.,scales="free")+
  scale_y_continuous(expand=c(0,0),breaks=c(0,0.4))+
  scale_x_continuous(limits=c(1984,2020),expand=c(0,0),breaks=c(1985,1990,2000,2010))+
  coord_cartesian(ylim=c(0,0.65))+
  labs(y="",x="")+
  theme(
    axis.line.x = element_line(color="cornsilk"),
    axis.line.y = element_line(color="cornsilk"),
    axis.text=element_text(size=rel(1),colour='cornsilk'),
    axis.ticks=element_line(colour='cornsilk'),
    axis.title.y=element_text(colour='cornsilk',face="bold",angle=90,size=15),
    axis.title.x=element_text(colour='cornsilk',face="bold"),
    plot.title=element_text(size=rel(2),colour='cornsilk',face="bold",hjust = 0.5),
    plot.background=element_blank(),
    panel.grid.major=element_blank(),
    panel.grid.minor=element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    legend.background=element_blank(),
    legend.text=element_text(colour='cornsilk',size=12),
    legend.title=element_text(colour='cornsilk',size=12),
    strip.background=element_blank(),
    strip.text=element_blank()
  )

SC_pres<-ggplot(data=twlB_SC,aes(x=Year,y=Area.Biomass/1000))+
  geom_point(size=3,color="cornsilk")+
  geom_errorbar(data=twlB_SC,aes(x=Year,ymax=UL/1000,ymin=LL/1000), width=0.5,color="cornsilk")+
  geom_line(data=SCRFX,aes(x=YEAR,y=Biom_UL/1000),linetype="dashed",color="orange",size=1)+
  geom_line(data=SCRFX,aes(x=YEAR,y=Biom_est/1000),color="orange",size=1)+
  geom_line(data=SCRFX,aes(x=YEAR,y=Biom_LL/1000),linetype="dashed",color="orange",size=1)+
  facet_grid(REG_AREA~.,scales="free")+
  labs(y="Biomass (1000s t)",x="")+
  scale_y_continuous(expand=c(0,0))+
  scale_x_continuous(limits=c(1984,2020),expand=c(0,0),breaks=c(1985,1990,1995,2000,2005,2010,2015))+
  ggtitle("Sharpchin")+
  annotation_custom2(grob = ggplotGrob(SC_inset_pres), data=data.frame(REG_AREA="WESTERN GOA",Year=1, Area.Biomass=1),xmin=1990, xmax=2017, ymin=5, ymax=60)+
  theme(
    axis.line.x = element_line(color="cornsilk"),
    axis.line.y = element_line(color="cornsilk"),
    axis.text=element_text(size=rel(1),colour='cornsilk'),
    axis.ticks=element_line(colour='cornsilk'),
    axis.title.y=element_text(colour='cornsilk',face="bold",angle=90,size=15),
    axis.title.x=element_text(colour='cornsilk',face="bold"),
    plot.title=element_text(size=rel(2),colour='cornsilk',face="bold",hjust = 0.5),
    plot.background=element_blank(),
    panel.grid.major=element_blank(),
    panel.grid.minor=element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    legend.background=element_blank(),
    legend.text=element_text(colour='cornsilk',size=12),
    legend.title=element_text(colour='cornsilk',size=12),
    strip.background=element_blank(),
    strip.text=element_blank()
  )

fakedat<-data.frame(YEAR=c(1984,1984,1987,1987),Biomass=c(0,5,0,10),Group=c(1,2,1,2))
RFX_leg_pres<-ggplot(data=fakedat,aes(x=YEAR,y=Biomass/1000))+
  geom_point(data=fakedat[fakedat$YEAR==1984,],aes(x=YEAR,y=Biomass/1000,color=as.factor(Group)),size=3,show.legend=T)+
  geom_line(data=fakedat[fakedat$YEAR==1987,],aes(x=YEAR,y=Biomass/1000,color=as.factor(Group)),size=1,linetype="solid",color="orange",show.legend = T)+
  scale_colour_manual(values = c("cornsilk","orange"),
                      guide = guide_legend(override.aes = list(linetype = c("blank", "solid"),shape = c(16,NA))),
                      labels=c("Survey","Rand FX"))+
  theme(legend.title=element_blank(),
        legend.key = element_blank(),
        legend.background=element_blank(),
        legend.text=element_text(color="cornsilk"))
legend_pres = gtable_filter(ggplot_gtable(ggplot_build(RFX_leg_pres)), "guide-box") 

ORpres<-ggplot(data=all2,aes(x=YEAR,y=Biomass/1000))+
  geom_point(size=3,color="cornsilk")+
  geom_errorbar(data=all2,aes(x=YEAR,ymax=UL/1000,ymin=LL/1000), width=0.5,color="cornsilk")+
  geom_line(data=allRFX,aes(x=YEAR,y=Biom_UL/1000),linetype="dashed",color="orange",size=1)+
  geom_line(data=allRFX,aes(x=YEAR,y=Biom_est/1000,colour=Group),color="orange",size=1)+
  geom_line(data=allRFX,aes(x=YEAR,y=Biom_LL/1000),linetype="dashed",color="orange",size=1)+
  facet_grid(REG_AREA~.,scales="free")+
  labs(y="",x="")+
  scale_y_continuous(expand=c(0,0))+
  scale_x_continuous(limits=c(1984,2020),expand=c(0,0),breaks=c(1985,1990,1995,2000,2005,2010,2015))+
  ggtitle("Tier 5 Other Rockfish")+
  annotation_custom2(grob = legend_pres, data=data.frame(REG_AREA="WESTERN GOA",YEAR=1, Biomass=1),xmin=2010, xmax=Inf, ymin=40, ymax=Inf)+
  theme_pres()

RFX_pres<-grid.arrange(arrangeGrob(SC_pres, ORpres, ncol = 2,
                                  bottom = textGrob("Year", vjust=0,gp=gpar(col="cornsilk", fontsize=15,face="bold"))))
ggsave("RFX_pres.png",plot=RFX_pres,dpi=600, bg="transparent",width=15,height=9)

#biomass by species for reference
datafile<-read.csv(paste(getwd(),"/Biomass by Regulatory Area - GOA.csv",sep=""), header=T) #from access file, if R sql doesn't work
dat2<-melt(as.data.frame(datafile[,c("Year","Regulatory.Area.Name","Species.Code","Area.Biomass","Area.Biomass.Var")]),id=c("Year","Regulatory.Area.Name","Species.Code" ),value="Area.Biomass")
colnames(dat2)

d3<-merge(dat2,codes[,c("RACE_code","NORPAC_name")],by.x="Species.Code",by.y="RACE_code")
sadat<-as.data.frame(dcast(d3[,c("Year","Regulatory.Area.Name","variable","value","NORPAC_name")],
                           Year+Regulatory.Area.Name+NORPAC_name~variable,sum))
colnames(sadat)<-c("Year","FMP_subarea","Common_Name","Biomass","Variance")
sadat$stdev<-sqrt(sadat$Biomass)
sadat$UL<-sadat$Biomass+1.96*sqrt(sadat$Variance)
sadat$LL<-sadat$Biomass-1.96*sqrt(sadat$Variance)

spec<-unique(sadat$Common_Name)


pdf(paste("OROX_Biom_Species",SYR,".pdf",sep=""))






#############################################
################################################
#AGE COMPS
#data downloaded from AKFIN, RACE survey, GOA Age Composisition Totals
acdat<-read.csv(paste(getwd(),"/GOA Age Composition Totals.csv",sep=""), header=T)
colnames(acdat)

#need to hard code in sample size, this was copied by hand from website (years in parantheses)
#harlequin: 641, 97, 255, 23 (96,99,11,15)
#redstrip: 362, 126 (96, 13)
#sharpchin: 38, 529 (90, 96)
#silvergray: 132, 364,349, 82 (93,96,99,05)

acdat2<-ddply(acdat[acdat$Age..years.>-1,],.(Year,Common.Name),transform,prop=Age.Pop/sum(Age.Pop))

acdat2$names<-ifelse(acdat2$Common.Name=="silvergray rockfish","Silvergray",
                     ifelse(acdat2$Common.Name=="harlequin rockfish","Harlequin",
                            ifelse(acdat2$Common.Name=="sharpchin rockfish","Sharpchin","Redstripe")))
acdat2$names<-as.factor(acdat2$names)
acdat3<-melt(dcast(acdat2,Year+names~Age..years.,sum,value.var="prop"),id.vars=c("Year","names"))
colnames(acdat3)<-c("Year","names","Age..years.","prop")
acdat3$Age..years.<-as.numeric(as.character(acdat3$Age..years.))

mage<-ddply(acdat3,.(Year,names),summarize,mean_age=weighted.mean(Age..years.,prop),max_prop=max(prop))
mage$sampn<-c("n=38","n=132","n=641","n=362","n=529","n=364","n=97","n=349","n=82","n=255","n=126","n=23")
mage$mean<-paste("Mean=",round(mage$mean_age,1),sep="")
mage$cohort<-ddply(acdat3,.(Year,names),function(x){x[which.max(x$prop),1]-x[which.max(x$prop),3]})[,3]
mage$age_class<-ddply(acdat3,.(Year,names),function(x){x[which.max(x$prop),3]})[,3]

cbPalette <- c("#999999", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

#makes a fake data for the 2013 redstrip which is off the chart
ac<-c(rep(0,10),7,0)
mp<-c(rep(0,10),0.4,0)
coh<-c(rep(NA,10),2006,NA)
rs<-as.data.frame(cbind(mage$age_class,mage$max_prop,coh))
colnames(rs)<-c("ac","mp","coh")

rs<-mage
rs$age_class<-c(rep(0,10),7,0) 
rs$max_prop<-c(rep(0,10),0.4,0)
rs$cohort<-c(rep(NA,10),2006,NA)
  
acdoc<-ggplot(acdat2[acdat2$Age..years.>-1,], aes(x=Age..years., y=prop,fill=Common.Name)) +   
  geom_bar(stat="identity")+
  facet_grid(Year~names)+
  coord_cartesian(ylim=c(0,0.45))+
  labs(y="Proportion",x="Age (years)")+
  scale_fill_manual(values=cbPalette)+
  scale_y_continuous(breaks=c(0,0.2,0.4),expand=c(0,0))+
  scale_x_continuous(expand=c(0,0))+
  geom_text(data=mage, aes(x=40, y=0.35, label=sampn), size=6,colour="#666666", inherit.aes=FALSE, parse=FALSE,hjust=1)+
  geom_text(data=mage, aes(x=40, y=0.25, label=mean), size=6,colour="#666666", inherit.aes=FALSE, parse=FALSE,hjust=1)+
  geom_text(data=mage, aes(x=age_class, y=max_prop+0.05, label=cohort), size=3,colour="#666666", inherit.aes=FALSE, parse=FALSE,hjust=1)+
  geom_text(data=rs,aes(x=age_class, y=max_prop, label=cohort), size=3,colour="#666666", inherit.aes=FALSE, parse=FALSE,hjust=1)+
  theme_bw()+
  theme(plot.background=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.title.y=element_text(colour='black',face="bold"),
        axis.title.x=element_text(colour='black',face="bold"),
        axis.text.y=element_text(colour='black'),
        axis.text.x=element_text(colour='black'),
        plot.title=element_text(size=rel(2),colour='black',face="bold"),
        axis.ticks=element_line(colour='black'),
        axis.line.x=element_line(size=1,colour="black"),
        axis.line.y=element_line(size=1,colour="black"),
        axis.text=element_text(size=rel(1.2)),
        strip.text.x=element_text(colour="black",face="bold",size=15),
        strip.text.y=element_text(colour="black",size=15,face="bold"),
        legend.background=element_blank(),
        legend.text=element_text(colour='black'),
        legend.title=element_text(colour='black'),
        legend.position="none")

ggsave("OROX_agecomp_doc.png",plot=acdoc,dpi=300, bg="transparent",width=15,height=9)

#for presentation
acpres<-ggplot(acdat2[acdat2$Age..years.>-1,], aes(x=Age..years., y=prop,fill=Common.Name)) +   
  geom_bar(stat="identity")+
  facet_grid(Year~names)+
  coord_cartesian(ylim=c(0,0.45))+
  labs(y="Proportion",x="Age (years)")+
  scale_fill_manual(values=cbPalette)+
  scale_y_continuous(breaks=c(0,0.2,0.4),expand=c(0,0))+
  scale_x_continuous(expand=c(0,0))+
  geom_text(data=mage, aes(x=40, y=0.35, label=sampn), size=6,colour="cornsilk", inherit.aes=FALSE, parse=FALSE,hjust=1)+
  geom_text(data=mage, aes(x=40, y=0.25, label=mean), size=6,colour="cornsilk", inherit.aes=FALSE, parse=FALSE,hjust=1)+
  geom_text(data=mage, aes(x=age_class, y=max_prop+0.05, label=cohort), size=3,colour="cornsilk", inherit.aes=FALSE, parse=FALSE,hjust=1)+
  geom_text(data=rs,aes(x=age_class, y=max_prop, label=cohort), size=3,colour="cornsilk", inherit.aes=FALSE, parse=FALSE,hjust=1)+
  theme_bw()+
  theme(plot.background=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.title.y=element_text(colour='cornsilk',face="bold"),
        axis.title.x=element_text(colour='cornsilk',face="bold"),
        axis.text.y=element_text(colour='cornsilk'),axis.text.x=element_text(colour='cornsilk'),
        axis.line.x=element_line(size=1,colour="cornsilk"),
        axis.line.y=element_line(size=1,colour="cornsilk"),
        plot.title=element_text(size=rel(2),colour='cornsilk',face="bold"),
        axis.ticks=element_line(colour='cornsilk'),axis.line=element_line(size=1),
        axis.text=element_text(size=rel(1.2)),
        strip.text.x=element_text(colour="black",face="bold",size=15),
        strip.text.y=element_text(colour="black",size=15,face="bold"),
        legend.background=element_blank(),
        legend.text=element_text(colour='cornsilk'),
        legend.title=element_text(colour='cornsilk'),
        legend.position="none") 
ggsave("OROX_agecomp_pres.png",plot=acpres,dpi=300, bg="transparent",width=15,height=9)

################################################
#SIZE COMPS
#downloaded from AKFIN RACE survey size composition by haul table
scdat<-read.csv(paste(getwd(),"/race_length_by_haul.csv",sep=""), header=T,skip=7)
colnames(scdat)

sc<-as.data.frame(melt(dcast(scdat,Common.Name+Length..mm.~Year,sum,value.var="Frequency"),id.vars=c("Common.Name","Length..mm.")))
colnames(sc)[3]<-"Year"

msize<-ddply(sc,.(Year,Common.Name),summarize,
             n=sum(value),
             meansc=ifelse(n==0,0,sum(as.numeric(Length..mm.)*as.numeric(value))/sum(value)),
             sdsc=ifelse(n==0,0,sqrt(sum((value*(Length..mm.-meansc)^2)/sum(value)))),
             UL=meansc+1.96*sdsc,
             LL=meansc-1.96*sdsc)
msize$sampn<-paste("n=",msize$n,sep="")
msize$mean2<-paste("Mean=",round(msize$meansc),sep="")
msize$names<-ifelse(msize$Common.Name=="silvergray rockfish","Silvergray",
                    ifelse(msize$Common.Name=="harlequin rockfish","Harlequin",
                           ifelse(msize$Common.Name=="sharpchin rockfish","Sharpchin",
                                  ifelse(msize$Common.Name=="redstripe rockfish","Redstripe",
                                         ifelse(msize$Common.Name=="redbanded rockfish","Redbanded","Yelloweye")))))

sc2<-merge(sc,msize,by=c("Year","Common.Name"))
sc2$prop<-sc2$value/sc2$n
sc2$names<-ifelse(sc2$Common.Name=="silvergray rockfish","Silvergray",
                  ifelse(sc2$Common.Name=="harlequin rockfish","Harlequin",
                         ifelse(sc2$Common.Name=="sharpchin rockfish","Sharpchin",
                                ifelse(sc2$Common.Name=="redstripe rockfish","Redstripe",
                                       ifelse(sc2$Common.Name=="redbanded rockfish","Redbanded","Yelloweye")))))
sc2[is.na(sc2)] <- 0

cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

scdoc<-ggplot(sc2, aes(x=Length..mm., y=prop,fill=names)) +   
  geom_bar(stat="identity") +
  #geom_rect(data=msize,aes(x=NULL,y=NULL,xmin=LL,xmax=UL, ymin=-Inf, ymax=Inf),fill="black",alpha=1/10)+
  geom_vline(data=msize,aes(xintercept=meansc))+
  facet_grid(Year~names)+
  coord_cartesian(ylim=c(0,0.5))+
  labs(y="Proportion",x="Length (mm)")+
  scale_y_continuous(breaks=c(0.1,0.3),expand=c(0,0))+
  scale_fill_manual(values=cbPalette)+
  geom_text(data=msize, aes(x=800, y=0.4, label=sampn), size=4,colour="#666666", inherit.aes=FALSE, parse=FALSE,hjust=1)+
  #geom_text(data=msize, aes(x=800, y=0.25, label=mean2), size=4,colour="#666666", inherit.aes=FALSE, parse=FALSE,hjust=1)+
  theme_bw()+
  theme(plot.background=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.title.y=element_text(colour='black',face="bold"),
        axis.title.x=element_text(colour='black',face="bold"),
        axis.text.y=element_text(colour='black'),
        axis.text.x=element_text(colour='black'),
        plot.title=element_text(size=rel(2),colour='black',face="bold"),
        axis.ticks=element_line(colour='black'),
        axis.line.x=element_line(size=1,colour="black"),
        axis.line.y=element_line(size=1,colour="black"),
        axis.text=element_text(size=rel(1.2)),
        strip.text.x=element_text(colour="black",face="bold",size=15),
        strip.text.y=element_text(colour="black",size=15,face="bold"),
        legend.background=element_blank(),
        legend.text=element_text(colour='black'),
        legend.title=element_text(colour='black'),
        legend.position="none")

ggsave("OROX_sizecomp_doc.png",plot=scdoc,dpi=300, bg="transparent",width=15,height=10)

scpres<-ggplot(sc2, aes(x=Length..mm., y=prop,fill=names)) +   
  geom_bar(stat="identity") +
  #geom_rect(data=msize,aes(x=NULL,y=NULL,xmin=LL,xmax=UL, ymin=-Inf, ymax=Inf),fill="black",alpha=1/10)+
  geom_vline(data=msize,aes(xintercept=meansc))+
  facet_grid(Year~names)+
  coord_cartesian(ylim=c(0,0.5))+
  labs(y="Proportion",x="Length (mm)")+
  scale_y_continuous(breaks=c(0.1,0.3),expand=c(0,0))+
  scale_fill_manual(values=cbPalette)+
  geom_text(data=msize, aes(x=800, y=0.4, label=sampn), size=4,colour="#666666", inherit.aes=FALSE, parse=FALSE,hjust=1)+
  #geom_text(data=msize, aes(x=800, y=0.25, label=mean2), size=4,colour="#666666", inherit.aes=FALSE, parse=FALSE,hjust=1)+
  theme_bw()+
  theme(plot.background=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.title.y=element_text(colour='cornsilk',face="bold"),
        axis.title.x=element_text(colour='cornsilk',face="bold"),
        axis.text.y=element_text(colour='cornsilk'),axis.text.x=element_text(colour='cornsilk'),
        axis.line.x=element_line(size=1,colour="cornsilk"),
        axis.line.y=element_line(size=1,colour="cornsilk"),
        plot.title=element_text(size=rel(2),colour='cornsilk',face="bold"),
        axis.ticks=element_line(colour='cornsilk'),axis.line=element_line(size=1),
        axis.text=element_text(size=rel(1.2)),
        strip.text.x=element_text(colour="black",face="bold",size=15),
        strip.text.y=element_text(colour="black",size=15,face="bold"),
        legend.background=element_blank(),
        legend.text=element_text(colour='cornsilk'),
        legend.title=element_text(colour='cornsilk'),
        legend.position="none") 

ggsave("OROX_sizecomp_pres.png",plot=scpres,dpi=300, bg="transparent",width=15,height=9)























