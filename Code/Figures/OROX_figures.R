# Directory Setup ----
figdir <- paste(getwd(),"/Output/", AYR, "/Figures", sep="")
dir.create(figdir)
#datadir <- paste(getwd(),"/Data/Annual_updates/", AYR, sep="")

# Themes Setup ----

cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7") #colorblind friendly palette
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

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

# CATCH FIGS ----
#bringing catch in
catchdat<-read_csv(paste(getwd(),"/Output/", AYR, "/Catch/OROXcomplex_catch",AYR,"_confidential.csv",sep=""))
colnames(catchdat)
#[1] "year"         "fmp_subarea"  "NORPAC_name"  "Retained"     "Discarded"    "tot_catch"    "OROXsubgroup"
codes<-read_csv(paste(getwd(),"/Data/Static/GOA_OROX_codes.csv",sep=""))

catchdat <- catchdat %>% 
  left_join(codes) %>% 
  mutate(fmp_subarea = if_else(fmp_subarea == "CG", "CENTRAL",
                               if_else(fmp_subarea == "WG", "WESTERN",
                                       if_else(fmp_subarea == "SE", "SOUTHEAST", "WYAK"))),
         fmp_subarea = factor(fmp_subarea, levels = c("WESTERN","CENTRAL","WYAK","SOUTHEAST")))

# Figure 4 ----
cd2 <- catchdat %>% 
  group_by(fmp_subarea, year) %>% 
  summarise(Catch = sum(tot_catch)) %>% 
  mutate(fmp_subarea = factor(fmp_subarea, levels = c("WESTERN","CENTRAL","WYAK","SOUTHEAST")),
         fakeorder = if_else(fmp_subarea == "WESTERN",4,
                             if_else(fmp_subarea == "CENTRAL",3,
                                     if_else(fmp_subarea == "WYAK",2,1)))) %>% 
  arrange(fmp_subarea)

catch_area_doc<-ggplot(cd2, aes(x=year,y=Catch,fill=fmp_subarea,order=-as.numeric(fmp_subarea)))+ 
  geom_bar(stat="identity",position="stack")+
  scale_fill_manual(values=rev(brewer.pal(n=length(unique(cd2$fmp_subarea)),"Greens")),name="Area")+
  scale_y_continuous(expand=c(0,0))+
  coord_cartesian(ylim=c(0,1400))+
  labs(y="",x="")+
  theme_doc()+theme(plot.margin=unit(c(0,0.5,0,0.2), "cm"),
                    axis.line.x = element_line(),
                    axis.line.y = element_line())
catch_area_pres<-ggplot(cd2, aes(x=year,y=Catch,fill=fmp_subarea,order=-as.numeric(fmp_subarea)))+ 
  geom_bar(stat="identity",position="stack")+
  scale_fill_manual(values=rev(brewer.pal(n=length(unique(catchdat$fmp_subarea)),"Greens")),name="Area")+
  scale_y_continuous(expand=c(0,0))+
  coord_cartesian(ylim=c(0,1400))+
  labs(y="",x="")+
  theme_pres()+theme(plot.margin=unit(c(0,0.5,0,0), "cm"))

#catch by species with top species
top_codes <- c(30100, 30560, 30535, 30430, 30470, 30475)
top_spec <- catchdat %>% 
  filter(RACE_code %in% top_codes) %>% 
  group_by(year, NORPAC_name) %>% 
  summarise(Catch = sum(tot_catch, na.rm=T))

spec_catch <- catchdat %>% 
  filter(RACE_code %nin% top_codes) %>% 
  group_by(year) %>% 
  summarise(Catch = sum(tot_catch, na.rm=T)) %>% 
  mutate(NORPAC_name = "MINORS") %>% 
  bind_rows(top_spec) %>% 
  mutate(Species = gsub(" ROCKFISH","",NORPAC_name),
         Species = factor(Species, levels = c("HARLEQUIN","RED BANDED","REDSTRIPE","SHARPCHIN","SILVERGRAY","YELLOWEYE","MINORS")),
         fakeorder = if_else(Species=="HARLEQUIN",7,
                           if_else(Species=="RED BANDED",6,
                                  if_else(Species=="REDSTRIPE",5,
                                         if_else(Species=="SHARPCHIN",4,
                                                ifelse(Species=="SILVERGRAY",3,
                                                       ifelse(Species=="YELLOWEYE",2,1))))))) %>% 
  select(-NORPAC_name) %>% 
  arrange(desc(fakeorder))

#Catch by species for doc
catch_species_doc<-ggplot(spec_catch, aes(x=year, y=Catch,fill=Species,order=-as.numeric(Species))) + 
  geom_bar(stat="identity")+
  scale_fill_manual(values=rev(brewer.pal(n=length(unique(spec_catch$Species)),"Blues")),name="Species")+
  scale_y_continuous(expand=c(0,0))+
  coord_cartesian(ylim=c(0,1400))+
  labs(y="",x="")+
  theme_doc()
catch_species_pres<-ggplot(spec_catch, aes(x=year, y=Catch,fill=Species,order=-as.numeric(Species))) + 
  geom_bar(stat="identity")+
  scale_fill_manual(values=rev(brewer.pal(n=length(unique(spec_catch$Species)),"Blues")),name="Species")+
  scale_y_continuous(expand=c(0,0))+
  coord_cartesian(ylim=c(0,1400))+
  labs(y="",x="")+
  theme_pres()

catchfig_doc<-grid.arrange(arrangeGrob(catch_area_doc, catch_species_doc, nrow = 2,
                                left = textGrob("Catch (t)", rot = 90, vjust = 1.5,gp=gpar(col="black", fontsize=20)),
                                bottom = textGrob("Year", vjust=0,gp=gpar(col="black", fontsize=20))))
ggsave("GOAOROX_Fig4_doc.png", path = figdir, plot=catchfig_doc,dpi=600, bg="transparent",width=15,height=9)

catchfig_pres<-grid.arrange(arrangeGrob(catch_area_pres, catch_species_pres, nrow = 2,
                                       left = textGrob("Catch (t)", rot = 90, vjust = 1.5,gp=gpar(col="#FFFFCC", fontsize=20)),
                                       bottom = textGrob("Year", vjust=0,gp=gpar(col="#FFFFCC", fontsize=20))))
ggsave("OROX_catch2_pres.png", path = figdir, plot=catchfig_pres,dpi=600, bg="transparent",width=15,height=9)

#Catch by species and area, W/CGOA only (not in assessment)
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
ggsave("OROX_WCGOA.png",path = figdir, plot=catch_species_area,dpi=600, bg="transparent",width=15,height=6)

# Figure 5 ----
#catch by area for top 6 species
Ctop_spec<-catchdat %>% 
  filter(RACE_code %in% top_codes) %>% 
  group_by(year, fmp_subarea, NORPAC_name) %>% 
  summarise(Catch = sum(tot_catch, na.rm=T)) %>% 
  mutate(NORPAC_name = gsub(" ROCKFISH","",NORPAC_name))

catch_area_doc<-ggplot(Ctop_spec, aes(x=year,y=Catch,fill=fmp_subarea,order=-as.numeric(fmp_subarea)))+ 
  geom_bar(stat="identity",position="fill")+
  scale_fill_manual(values=rev(brewer.pal(n=length(unique(Ctop_spec$fmp_subarea)),"Greens")),name="Area")+
  coord_cartesian(ylim=c(0,1),expand=c(0,0))+
  labs(y="Catch (t)",x="Year")+
  facet_grid(NORPAC_name~.)+
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
ggsave("GOAOROX_Fig5_doc.png",path = figdir, plot=catch_area_doc,dpi=600, bg="transparent",width=7,height=8)

# Catch:ABC ----
ABC_dat <- read_csv(paste(getwd(),"/Data/OROX_ABC_App_historical.csv",sep=""))
AYR_specs <- read_csv(paste(getwd(),"/Output/", AYR, "/Harvest_specs/OROX_harvestrecs_",AYR,".csv",sep=""))

AYR_appt_ABC_EGOA <- AYR_specs %>% 
  filter(Group == "OROX", 
         REGULATORY_AREA_NAME %in% c("WY", "EYSEO")) %>% 
  mutate(REGULATORY_AREA_NAME = if_else(REGULATORY_AREA_NAME == "WY", "WYAK", "SOUTHEAST")) %>% 
  select(REGULATORY_AREA_NAME, ABC) %>% 
  rename(fmp_subarea = REGULATORY_AREA_NAME)

AYR_appt_ABC <- AYR_specs %>% 
  filter(Group == "OROX", 
         REGULATORY_AREA_NAME %in% c("WESTERN GOA", "CENTRAL GOA")) %>% 
  select(ABC) %>% 
  group_by() %>% 
  summarise(A2 = sum(ABC, na.rm = T)) %>% 
  rename(ABC = A2) %>% 
  mutate(fmp_subarea = "WEST_CENTRAL") %>% 
  bind_rows(AYR_appt_ABC_EGOA) %>% 
  mutate(year = 2022,
         fmp_subarea = factor(fmp_subarea, levels = c("WEST_CENTRAL","WYAK","SOUTHEAST")),
         fakeorder = if_else(fmp_subarea == "WEST_CENTRAL",3,
                             if_else(fmp_subarea == "WYAK",2,1))) %>% 
  arrange(desc(fakeorder))
  

WCcatch <- cd2 %>% 
  filter(fmp_subarea %in% c("WESTERN", "CENTRAL")) %>% 
  group_by(year) %>% 
  summarise(catch = sum(Catch, na.rm = T)) %>% 
  mutate(fmp_subarea = "WEST_CENTRAL",
         fakeorder = 3) %>% 
  rename(Catch = catch) #this might be a plyr error, because it should be the opposite
  
cd3 <- cd2 %>% 
  filter(fmp_subarea %nin% c("WESTERN", "CENTRAL")) %>% 
  bind_rows(WCcatch) %>% 
  left_join(ABC_dat) %>% 
  mutate(fmp_subarea = factor(fmp_subarea, levels = c("WEST_CENTRAL","WYAK","SOUTHEAST"))) %>% 
  arrange(fmp_subarea)

# Changes in ABC
fmpsub_specs_hist <- read_csv(paste(getwd(),"/Data/OROX_ABC_App_historical.csv",sep=""))

ABC_change <- fmpsub_specs_hist %>% 
  filter(year >= AYR) %>% 
  pivot_wider(names_from = year, values_from = ABC) %>%
  rename(ABC_AYR = 2, ABC_next = 3) %>% 
  mutate(Pchange = paste(round(((ABC_next - ABC_AYR)/ABC_AYR)*100,0),"%",sep=""),
         fmp_subarea = factor(fmp_subarea, levels = c("WEST_CENTRAL","WYAK","SOUTHEAST")),
         fakeorder = if_else(fmp_subarea == "WEST_CENTRAL",3,
                             if_else(fmp_subarea == "WYAK",2,1))) %>% 
  arrange(fakeorder)

C_ABC <- ggplot(cd3, aes(x=year, y=Catch, fill=fmp_subarea, order=-as.numeric(fmp_subarea)))+ 
   geom_bar(stat="identity",position="stack", show.legend = F)+
   geom_line(aes(x = year, y = ABC))+
   geom_point(data = AYR_appt_ABC, aes(x = year, y = ABC), shape = 8, show.legend = F)+
   geom_text(data = ABC_change, aes(x = AYR+1, y = ABC_AYR, label = Pchange), color = "black")+
   scale_fill_manual(values=rev(brewer.pal(n=length(unique(cd2$fmp_subarea)),"Greens")),name="Area")+
   facet_grid(fmp_subarea~., scales = "free")+
   labs(y="Catch (mt)",x="Year")+
   theme_doc()+theme(plot.margin=unit(c(0,0.5,0,0.2), "cm"),
                     axis.line.x = element_line(),
                     axis.line.y = element_line())
 
 ggsave("GOAOROX_FigX_doc.png",path = figdir, plot= C_ABC,dpi=600, bg="transparent",width=7,height=8)
 
 C_ABC_pres <- ggplot(cd3, aes(x=year, y=Catch, fill=fmp_subarea, order=-as.numeric(fmp_subarea)))+ 
   geom_bar(stat="identity",position="stack", show.legend = F)+
   geom_line(aes(x = year, y = ABC), color = "cornsilk")+
   geom_point(data = AYR_appt_ABC, aes(x = year, y = ABC), shape = 8, show.legend = F, color = "cornsilk")+
   geom_text(data = ABC_change, aes(x = AYR+1, y = ABC_AYR, label = Pchange), color = "cornsilk")+
   scale_fill_manual(values=rev(brewer.pal(n=length(unique(cd2$fmp_subarea)),"Greens")),name="Area")+
   facet_grid(fmp_subarea~., scales = "free")+
   labs(y="Catch (mt)",x="Year")+
   theme_pres()+theme(plot.margin=unit(c(0,0.5,0,0.2), "cm"),
                     axis.line.x = element_line(),
                     axis.line.y = element_line())
 
 ggsave("GOAOROX_FigX_pres.png",path = figdir, plot= C_ABC_pres,dpi=600, bg="transparent",width=7,height=8)

# BIOMASS FIGS ----
#good website for colors: http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/

# Figure 3 ----
biomass_dat <- read_csv(paste(getwd(),"/Output/", AYR, "/RACE_Biomass/RACE_Biomass_GOA_OROX_SPECIES.csv",sep=""))

biom_dat <- biomass_dat %>% 
  filter(REGULATORY_AREA_NAME %in% c("WESTERN GOA", "CENTRAL GOA", "EASTERN GOA")) %>% 
  mutate(REGULATORY_AREA_NAME = gsub(" GOA","",REGULATORY_AREA_NAME),
         Group = gsub("_"," ", Group),
         Group = tolower(Group)) %>% 
  rename(RACE_name = Group) %>% 
  left_join(codes) %>% 
  select(YEAR, REGULATORY_AREA_NAME, Biomass, NORPAC_name, RACE_code) %>% 
  mutate(NORPAC_name = gsub(" ROCKFISH", "", NORPAC_name))

#biomass by area
area_biom <- biom_dat %>% 
  filter(RACE_code %in% top_codes) %>% 
  group_by(YEAR, REGULATORY_AREA_NAME) %>% 
  summarise(Biom_mt = sum(Biomass, na.rm=T)) %>% 
  mutate(REGULATORY_AREA_NAME = factor(REGULATORY_AREA_NAME, levels = c("WESTERN", "CENTRAL", "EASTERN")),
         fakeorder = if_else(REGULATORY_AREA_NAME == "WESTERN",3,
                             if_else(REGULATORY_AREA_NAME == "CENTRAL", 2, 1))) %>% 
  arrange(fakeorder)

Bareadoc<-ggplot(area_biom, aes(x=YEAR, y=Biom_mt/1000, fill=REGULATORY_AREA_NAME, order=-as.numeric(REGULATORY_AREA_NAME)))+ 
  geom_bar(stat="identity",position="stack")+
  scale_fill_manual(values=rev(brewer.pal(n=length(unique(area_biom$REGULATORY_AREA_NAME)),"Greens")),name="Area")+
  scale_y_continuous(expand=c(0,0),limits=c(0,200))+
  labs(y="",x="")+
  theme_doc()
Bareapres<-ggplot(area_biom, aes(x=YEAR, y=Biom_mt/1000, fill=REGULATORY_AREA_NAME,order=-as.numeric(REGULATORY_AREA_NAME)))+ 
  geom_bar(stat="identity",position="stack")+
  scale_fill_manual(values=rev(brewer.pal(n=length(unique(area_biom$REGULATORY_AREA_NAME)),"Greens")),name="Area")+
  scale_y_continuous(expand=c(0,0),limits=c(0,200))+
  labs(y="",x="")+
  theme_pres()

#biomass by species
#by species with top species
top_spec_biom <- biom_dat %>% 
  filter(RACE_code %in% top_codes) %>% 
  group_by(YEAR, NORPAC_name) %>% 
  summarise(Biom_mt = sum(Biomass, na.rm=T))

spec_biom <- biom_dat %>% 
  filter(RACE_code %nin% top_codes) %>% 
  group_by(YEAR) %>% 
  summarise(Biom_mt = sum(Biomass, na.rm=T)) %>% 
  mutate(NORPAC_name = "MINORS") %>% 
  bind_rows(top_spec_biom) %>% 
  mutate(NORPAC_name = factor(NORPAC_name, levels = c("HARLEQUIN","RED BANDED","REDSTRIPE","SHARPCHIN","SILVERGRAY","YELLOWEYE","MINORS")),
         fakeorder = if_else(NORPAC_name=="HARLEQUIN",7,
                             if_else(NORPAC_name=="RED BANDED",6,
                                     if_else(NORPAC_name=="REDSTRIPE",5,
                                             if_else(NORPAC_name=="SHARPCHIN",4,
                                                     ifelse(NORPAC_name=="SILVERGRAY",3,
                                                            ifelse(NORPAC_name=="YELLOWEYE",2,1))))))) %>% 
  arrange(desc(fakeorder))

Bspecdoc<-ggplot(spec_biom, aes(x=YEAR, y=Biom_mt/1000,fill=NORPAC_name,order=-as.numeric(NORPAC_name))) + 
  geom_bar(stat="identity")+
  scale_fill_manual(values=rev(brewer.pal(n=length(unique(spec_biom$NORPAC_name)),"Blues")),name="Species")+
  scale_y_continuous(expand=c(0,0),limits=c(0,200))+
  labs(y="",x="")+
  theme_doc()
Bspecpres<-ggplot(spec_biom, aes(x=YEAR, y=Biom_mt/1000,fill=NORPAC_name,order=-as.numeric(NORPAC_name))) + 
  geom_bar(stat="identity")+
  scale_fill_manual(values=rev(brewer.pal(n=length(unique(spec_biom$NORPAC_name)),"Blues")),name="Species")+
  scale_y_continuous(expand=c(0,0),limits=c(0,200))+
  labs(y="",x="")+
  theme_pres()

biomfig_doc<-grid.arrange(arrangeGrob(Bareadoc, Bspecdoc, nrow = 2,
                                   left = textGrob("Biomass (1000s t)", rot = 90, vjust = 1,gp=gpar(col="black", fontsize=20)),
                                   bottom = textGrob("Year", vjust=0,gp=gpar(col="black", fontsize=20))))
ggsave("GOAOROX_Fig3_doc.png", path = figdir, plot=biomfig_doc,dpi=600, bg="transparent",width=15,height=9)

biomfig_pres<-grid.arrange(arrangeGrob(Bareapres, Bspecpres, nrow = 2,
                                      left = textGrob("Biomass (1000s t)", rot = 90, vjust = 1,gp=gpar(col="#FFFFCC", fontsize=20)),
                                      bottom = textGrob("Year", vjust=0,gp=gpar(col="#FFFFCC", fontsize=20))))
ggsave("GOAOROX_Fig3_pres.png", path = figdir, plot=biomfig_pres,dpi=600, bg="transparent",width=15,height=9)



# RFX FIGS ----
# Figure 9 ----
# Combine RFX and RACE Biomass datasets
OROXbiomass_dat <- read_csv(paste(getwd(),"/Output/", AYR, "/RACE_Biomass/RACE_Biomass_GOA_OROX.csv",sep=""))

RFX<-read_csv(paste(getwd(),"/Output/",AYR,"/RFX/RFX_Biomass_GOA_OROX.csv",sep=""))
RFX <- RFX %>% 
  rename(RFX_biom = Biom_est,
         RFX_LL = Biom_LL,
         RFX_UL = Biom_UL,
         RFX_CV = Biom_CV)

RFX_fig_dat <- RFX %>% 
  left_join(OROXbiomass_dat) %>% 
  filter(REGULATORY_AREA_NAME %in% c("WESTERN GOA", "CENTRAL GOA", "EASTERN GOA")) %>% 
  mutate(REGULATORY_AREA_NAME = gsub(" GOA","",REGULATORY_AREA_NAME),
         Biom_LL = if_else((Biomass - SE*1.96) < 0, 0, Biomass - SE*1.96),
         Biom_UL = if_else((Biomass + SE*1.96) < 0, 0, Biomass + SE*1.96),
         REGULATORY_AREA_NAME = factor(REGULATORY_AREA_NAME, levels=c("WESTERN","CENTRAL","EASTERN")),
         fakeorder = if_else(REGULATORY_AREA_NAME == "WESTERN", 3,
                             if_else(REGULATORY_AREA_NAME == "CENTRAL", 2, 1))) %>% 
  #replace(is.na(.),0) %>% 
  arrange(fakeorder)

# Group data
SC_biom <- RFX_fig_dat %>% 
  filter(Group == "OROX_SC") %>% 
  mutate(Group = "Sharpchin")

T5_biom <- RFX_fig_dat %>% 
  filter(Group == "OROX_allnoSC") %>% 
  mutate(Group = "Tier5")

#Figures
annotation_custom2 <- function (grob, xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf, data) 
{
  layer(data = data, stat = StatIdentity, position = PositionIdentity, 
        geom = ggplot2:::GeomCustomAnn,
        inherit.aes = TRUE, params = list(grob = grob, 
                                          xmin = xmin, xmax = xmax, 
                                          ymin = ymin, ymax = ymax))
}

# Sharpchin inset first
SCB_in_dat <- SC_biom %>% 
  filter(REGULATORY_AREA_NAME == "WESTERN")

SC_inset<-ggplot(data=SCB_in_dat,aes(x=YEAR,y=Biomass/1000))+
  geom_point(size=3,color="black")+
  geom_errorbar(aes(x=YEAR,ymax=Biom_UL/1000,ymin=Biom_LL/1000), width=0.5,color="black")+
  geom_line(aes(x=YEAR,y=RFX_UL/1000),linetype="dashed",color="orange",size=1)+
  geom_line(aes(x=YEAR,y=RFX_biom/1000),color="orange",size=1)+
  geom_line(aes(x=YEAR,y=RFX_LL/1000),linetype="dashed",color="orange",size=1)+
  facet_grid(REGULATORY_AREA_NAME~.,scales="free")+
  scale_y_continuous(expand=c(0,0),breaks=c(0,0.4))+
  scale_x_continuous(limits=c(1983,AYR+1),expand=c(0,0),breaks=c(1980,1990,2000,2010,2020))+
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

#Sharpchin Figure
SC_doc<-ggplot(data=SC_biom,aes(x=YEAR,y=Biomass/1000))+
  geom_point(size=3,color="black")+
  geom_errorbar(aes(x=YEAR,ymax=Biom_UL/1000,ymin=Biom_LL/1000), width=0.5,color="black")+
  geom_line(aes(x=YEAR,y=RFX_UL/1000),linetype="dashed",color="orange",size=1)+
  geom_line(aes(x=YEAR,y=RFX_biom/1000),color="orange",size=1)+
  geom_line(aes(x=YEAR,y=RFX_LL/1000),linetype="dashed",color="orange",size=1)+
  facet_grid(REGULATORY_AREA_NAME~.,scales="free")+
  labs(y="Biomass (1000s t)",x="")+
  scale_y_continuous(expand=c(0,0))+
  scale_x_continuous(limits=c(1983,AYR+1),expand=c(0,0),breaks=c(1985,1990,1995,2000,2005,2010,2015,2020))+
  ggtitle("Sharpchin")+
  annotation_custom2(grob = ggplotGrob(SC_inset), 
                     data=data.frame(REGULATORY_AREA_NAME="WESTERN",YEAR=1, Biomass=1),xmin=1990, xmax=2021, ymin=5, ymax=60)+
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

# Tier 5 panels
ORdoc<-ggplot(data=T5_biom,aes(x=YEAR,y=Biomass/1000))+
  geom_point(size=3,color="black")+
  geom_errorbar(aes(x=YEAR,ymax=Biom_UL/1000,ymin=Biom_LL/1000), width=0.5,color="black")+
  geom_line(aes(x=YEAR,y=RFX_UL/1000),linetype="dashed",color="orange",size=1)+
  geom_line(aes(x=YEAR,y=RFX_biom/1000,colour=Group),color="orange",size=1)+
  geom_line(aes(x=YEAR,y=RFX_LL/1000),linetype="dashed",color="orange",size=1)+
  facet_grid(REGULATORY_AREA_NAME~.,scales="free")+
  labs(y="",x="")+
  scale_y_continuous(expand=c(0,0))+
  scale_x_continuous(limits=c(1983,AYR+1),expand=c(0,0),breaks=c(1985,1990,1995,2000,2005,2010,2015,2020))+
  ggtitle("Tier 5 Other Rockfish")+
  #annotation_custom2(grob = legend, data=data.frame(REG_AREA="WESTERN GOA",YEAR=1, Biomass=1),xmin=2010, xmax=Inf, ymin=40, ymax=Inf)+
  theme_doc()

# Putting them together
#https://stackoverflow.com/questions/37867758/insetting-on-facet-grided-and-grid-arrangeed-plot
RFX_doc<-grid.arrange(arrangeGrob(SC_doc, ORdoc, ncol = 2,
                                       bottom = textGrob("Year", vjust=0,gp=gpar(col="black", fontsize=15,face="bold"))))
ggsave("GOAOROX_Fig9_doc.png", path = figdir, plot=RFX_doc,dpi=600, bg="transparent",width=15,height=9)

SC_inset_pres<-ggplot(data=SCB_in_dat,aes(x=YEAR,y=Biomass/1000))+
  geom_point(size=3,color="cornsilk")+
  geom_errorbar(aes(x=YEAR,ymax=Biom_UL/1000,ymin=Biom_LL/1000), width=0.5,color="cornsilk")+
  geom_line(aes(x=YEAR,y=RFX_UL/1000),linetype="dashed",color="orange",size=1)+
  geom_line(aes(x=YEAR,y=RFX_biom/1000),color="orange",size=1)+
  geom_line(aes(x=YEAR,y=RFX_LL/1000),linetype="dashed",color="orange",size=1)+
  facet_grid(REGULATORY_AREA_NAME~.,scales="free")+
  scale_y_continuous(expand=c(0,0),breaks=c(0,0.4))+
  scale_x_continuous(limits=c(1983,AYR+1),expand=c(0,0),breaks=c(1980,1990,2000,2010,2020))+
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

SC_pres<-ggplot(data=SC_biom,aes(x=YEAR,y=Biomass/1000))+
  geom_point(size=3,color="cornsilk")+
  geom_errorbar(aes(x=YEAR,ymax=Biom_UL/1000,ymin=Biom_LL/1000), width=0.5,color="cornsilk")+
  geom_line(aes(x=YEAR,y=RFX_UL/1000),linetype="dashed",color="orange",size=1)+
  geom_line(aes(x=YEAR,y=RFX_biom/1000),color="orange",size=1)+
  geom_line(aes(x=YEAR,y=RFX_LL/1000),linetype="dashed",color="orange",size=1)+
  facet_grid(REGULATORY_AREA_NAME~.,scales="free")+
  labs(y="Biomass (1000s t)",x="")+
  scale_y_continuous(expand=c(0,0))+
  scale_x_continuous(limits=c(1983,AYR+1),expand=c(0,0),breaks=c(1985,1990,1995,2000,2005,2010,2015,2020))+
  ggtitle("Sharpchin")+
  annotation_custom2(grob = ggplotGrob(SC_inset_pres), 
                     data=data.frame(REGULATORY_AREA_NAME="WESTERN",YEAR=1, Biomass=1),xmin=1990, xmax=2021, ymin=5, ymax=60)+
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

ORpres<-ggplot(data=T5_biom,aes(x=YEAR,y=Biomass/1000))+
  geom_point(size=3,color="cornsilk")+
  geom_errorbar(aes(x=YEAR,ymax=Biom_UL/1000,ymin=Biom_LL/1000), width=0.5,color="cornsilk")+
  geom_line(aes(x=YEAR,y=RFX_UL/1000),linetype="dashed",color="orange",size=1)+
  geom_line(aes(x=YEAR,y=RFX_biom/1000,colour=Group),color="orange",size=1)+
  geom_line(aes(x=YEAR,y=RFX_LL/1000),linetype="dashed",color="orange",size=1)+
  facet_grid(REGULATORY_AREA_NAME~.,scales="free")+
  labs(y="",x="")+
  scale_y_continuous(expand=c(0,0))+
  scale_x_continuous(limits=c(1983,AYR+1),expand=c(0,0),breaks=c(1985,1990,1995,2000,2005,2010,2015,2020))+
  ggtitle("Tier 5 Other Rockfish")+
  theme_pres()

RFX_pres<-grid.arrange(arrangeGrob(SC_pres, ORpres, ncol = 2,
                                  bottom = textGrob("Year", vjust=0,gp=gpar(col="cornsilk", fontsize=15,face="bold"))))
ggsave("GOAOROX_Fig9_pres.png", path = figdir, plot=RFX_pres,dpi=600, bg="transparent",width=15,height=9)

# LENGTH COMPS ----
# Figure 6 ----
#downloaded from AKFIN RACE survey size composition by haul table
RACE_L_dat<-read_csv(paste(getwd(),"/Data/Annual_updates/", AYR, "/RACE_GOAOROX_Lengths.csv",sep=""), skip=7)

L_yr_spec <- RACE_L_dat %>% 
  clean_names() %>% 
  group_by(year, common_name, length_mm) %>% 
  summarise(freq = sum(frequency, na.rm = T)) %>%
  rename(frequency = freq) %>% 
  #select(common_name, year, length_mm, frequency) %>% 
  #pivot_wider(names_from = year, values_from = freq, values_fill = 0) %>% 
  #pivot_longer(!c(common_name, length_mm), names_to = "year", values_to = "frequency") %>% 
  group_by(year, common_name) %>% 
  summarise(n = sum(frequency),
            meanL = if_else(n == 0, 0, sum(length_mm*frequency)/sum(frequency)),
            sdL = if_else(n == 0, 0, sqrt(sum((frequency*(length_mm-meanL)^2)/sum(frequency)))),
            UL = meanL+1.96*sdL,
            LL = meanL-1.96*sdL) %>% 
  mutate(sampn = paste("n=", n, sep=""),
         Lmean = paste("Mean=", round(meanL), sep=""),
         common_name = gsub(" rockfish","", common_name),
         common_name = toupper(common_name))

L_yr_spec_freq <- RACE_L_dat %>% 
  clean_names() %>% 
  group_by(year, common_name, length_mm) %>% 
  summarise(freq = sum(frequency, na.rm = T)) %>%
  rename(frequency = freq) %>% 
  #select(common_name, year, length_mm, frequency) %>% 
  #pivot_wider(names_from = year, values_from = freq, values_fill = 0) %>% 
  #pivot_longer(!c(common_name, length_mm), names_to = "year", values_to = "frequency") %>% 
  mutate(common_name = gsub(" rockfish","", common_name),
         common_name = toupper(common_name)) %>% 
  left_join(L_yr_spec) %>% 
  mutate(prop = frequency/n)

Lcomp_top_spec <- L_yr_spec_freq %>% 
  filter(common_name %in% c("HARLEQUIN", "SILVERGRAY", "SHARPCHIN", "REDSTRIPE", "REDBANDED", "YELLOWEYE"))
Lcomp_top_spec_summed <- L_yr_spec %>% 
  filter(common_name %in% c("HARLEQUIN", "SILVERGRAY", "SHARPCHIN", "REDSTRIPE", "REDBANDED", "YELLOWEYE"))

scdoc<-ggplot(Lcomp_top_spec, aes(y=length_mm, x=year,color=common_name, size = prop)) +
  geom_point(show.legend = F)+
  geom_point(data = Lcomp_top_spec_summed, aes(x = year, y = meanL), colour = "black", size = 6, shape = 95)+
  facet_grid(.~common_name, scales = "free")+
  labs(y="Length (mm)",x="Year")+
  scale_fill_manual(values=cbPalette)+
  geom_text(data= Lcomp_top_spec_summed, aes(x= year, y= 50, label=n), size=3,colour="black", inherit.aes=FALSE, parse=FALSE,hjust=1, angle = 90)+
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

ggsave("GOAOROX_Fig6_doc.png", path = figdir, plot=scdoc,dpi=600, bg="transparent",width=15,height=10)

scpres<-ggplot(Lcomp_top_spec, aes(y=length_mm, x=year,color=common_name, size = prop)) +
  geom_point(show.legend = F)+
  geom_point(data = Lcomp_top_spec_summed, aes(x = year, y = meanL), colour = "cornsilk", size = 6, shape = 95)+
  facet_grid(.~common_name, scales = "free")+
  labs(y="Length (mm)",x="Year")+
  scale_fill_manual(values=cbPalette)+
  geom_text(data= Lcomp_top_spec_summed, aes(x= year, y= 50, label=n), size=3,colour="cornsilk", inherit.aes=FALSE, parse=FALSE,hjust=1, angle = 90)+
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

ggsave("GOAOROX_Fig6_pres.png", path = figdir, plot=scpres,dpi=600, bg="transparent",width=15,height=9)


# Exploitation Rates ----
#Exploitation Rates = Catch over RFX biomass for T4 and T5 by area, then by species?
T6spec <- c("CANARY ROCKFISH","CHINA ROCKFISH","QUILLBACK ROCKFISH","ROSETHORN ROCKFISH",
            "TIGER ROCKFISH","YELLOWEYE ROCKFISH","COPPER ROCKFISH", "AURORA ROCKFISH",
            "SHORTBELLY ROCKFISH")
EXP_catch_GOA <- catchdat %>% 
  select(year, fmp_subarea, NORPAC_name, tot_catch) %>% 
  filter(NORPAC_name %nin% T6spec) %>% 
  mutate(NORPAC_name = gsub(" ROCKFISH","",NORPAC_name),
         Group = if_else(NORPAC_name == "SHARPCHIN", "Tier_4", "Tier_5"),
         REGULATORY_AREA_NAME = if_else(fmp_subarea == "WESTERN", "WESTERN",
                                        if_else(fmp_subarea == "CENTRAL", "CENTRAL", "EASTERN"))) %>% 
  group_by(year, Group) %>% 
  summarise(Catch_mt = sum(tot_catch, na.rm=T)) %>% 
  rename(YEAR = year) %>% 
  mutate(REGULATORY_AREA_NAME = "GOA")

EXP_catch <- catchdat %>% 
  select(year, fmp_subarea, NORPAC_name, tot_catch) %>% 
  filter(NORPAC_name %nin% T6spec) %>% 
  mutate(NORPAC_name = gsub(" ROCKFISH","",NORPAC_name),
    Group = if_else(NORPAC_name == "SHARPCHIN", "Tier_4", "Tier_5"),
    REGULATORY_AREA_NAME = if_else(fmp_subarea == "WESTERN", "WESTERN",
                                   if_else(fmp_subarea == "CENTRAL", "CENTRAL", "EASTERN"))) %>% 
  group_by(year, REGULATORY_AREA_NAME, Group) %>% 
  summarise(Catch_mt = sum(tot_catch, na.rm=T)) %>% 
  rename(YEAR = year) %>% 
  bind_rows(EXP_catch_GOA)

EXP_dat <- RFX %>% 
  select(YEAR, REGULATORY_AREA_NAME, RFX_biom, Group) %>% 
  filter(Group %in% c("OROX_SC", "OROX_allnoSC"), 
         REGULATORY_AREA_NAME %in% c("CENTRAL GOA", "EASTERN GOA", "GOA", "WESTERN GOA"),
         YEAR >= 2003) %>% 
  mutate(REGULATORY_AREA_NAME = gsub(" GOA", "", REGULATORY_AREA_NAME),
         Group = if_else(Group == "OROX_SC", "Tier_4", "Tier_5")) %>% 
  left_join(EXP_catch) %>% 
  mutate(EXP_rate = Catch_mt/RFX_biom,
         REGULATORY_AREA_NAME = factor(REGULATORY_AREA_NAME, levels = c("GOA", "WESTERN", "CENTRAL", "EASTERN")),
         fakeorder = if_else(REGULATORY_AREA_NAME == "GOA",4,
                             if_else(REGULATORY_AREA_NAME == "WESTERN",3,
                                     if_else(REGULATORY_AREA_NAME == "CENTRAL",2,1)))) %>% 
  arrange(fakeorder)

EXP_fig <- ggplot(EXP_dat, aes(x = YEAR, y= EXP_rate, color = REGULATORY_AREA_NAME))+
  geom_line(show.legend = F)+
  #scale_fill_manual(values=rev(brewer.pal(n=length(unique(EXP_dat$REGULATORY_AREA_NAME)),"Greens")),name="Area")+
  facet_grid(REGULATORY_AREA_NAME~Group, scales = "free")+
  labs(y="Catch/RFX Biomass",x="Year")+
  theme_doc()+theme(plot.margin=unit(c(0,0.5,0,0.2), "cm"),
                    axis.line.x = element_line(),
                    axis.line.y = element_line())

ggsave("GOAOROX_FigXX_doc.png", path = figdir, plot=EXP_fig,dpi=600, bg="transparent",width=15,height=9)

# Interesting Things to Check ----

#Check out Weighted M through time and how it relates to biomass

# WTM appears to track combined biomass of M=0.05 (silvergray) and M=0.06 
# (9 species, of which redbanded is far and away the greatest) groups
scaledM <- WTMend %>% 
  mutate(M_scale = (WTM-min(WTM))/(max(WTM)-min(WTM)))
T5_Mbiom <- RFXB %>% 
  filter(grepl("M0", Group), # | Group == "OROX_SC"
         REGULATORY_AREA_NAME == "GOA")

# This years Wt_M changes from last assessment
M_change <- WTMend %>% 
  #filter(YEAR == (AYR-2) | YEAR == AYR) #use this normally, but for 2021 assessment fixed an error in M code, 
  filter(YEAR == AYR) %>% #use for 2021 assessment only
  mutate(Pchange = paste(round(((WTM-0.07)/0.07)*100, 0), "%", sep = "")) #use for 2021 asse4ssment only, 0.07 was value used in 2019, but technically it was wrong (but not by much)

Wt_M_plot_doc<- ggplot(T5_Mbiom, aes(x=YEAR, y= Biom_est))+
  geom_bar(position = "fill", stat = "identity", aes(fill = Group))+
  geom_point(data = scaledM, mapping = aes(x=YEAR, y=M_scale), size = 6)+
  geom_line(data = scaledM, mapping = aes(x=YEAR, y=M_scale))+
  geom_text(data = M_change, aes(x = 2018, y = 0.05, label = Pchange), color = "black", size = 10)+
  labs(y="Biomass Proportion",x="Year")+
  scale_x_continuous(expand = c(0,0))+
  scale_y_continuous(expand = c(0,0))+
  theme_doc()
ggsave("GOAOROX_Wt_M_doc.png", path = figdir, plot=Wt_M_plot_doc,dpi=600, bg="transparent",width=10,height=8)

Wt_M_plot_pres<- ggplot(T5_Mbiom, aes(x=YEAR, y= Biom_est))+
  geom_bar(position = "fill", stat = "identity", aes(fill = Group))+
  geom_point(data = scaledM, mapping = aes(x=YEAR, y=M_scale), size = 6)+
  geom_line(data = scaledM, mapping = aes(x=YEAR, y=M_scale))+
  geom_text(data = M_change, aes(x = 2018, y = 0.05, label = Pchange), color = "cornsilk", size = 10)+
  labs(y="Biomass Proportion",x="Year")+
  scale_x_continuous(expand = c(0,0))+
  scale_y_continuous(expand = c(0,0))+
  theme_pres()
ggsave("GOAOROX_Wt_M_pres.png", path = figdir, plot=Wt_M_plot_pres,dpi=600, bg="transparent",width=10,height=8)

# Changes in Survey Biomass
# dominant species: harlequin, silvergray, redstripe, redbanded, sharpchin, yelloweye
RACE_B <- read_csv(paste(getwd(),"/Output/",AYR,"/RACE_Biomass/RACE_Biomass_GOA_OROX_SPECIES.csv",sep=""))
majors <- c("Silvergray_Rockfish", "Harlequin_Rockfish", "Redstripe_Rockfish", "Redbanded_Rockfish", 
            "Sharpchin_Rockfish", "Yelloweye_Rockfish")
RACE_B2 <- RACE_B %>% 
  filter(SURVEY == "GOA") %>% 
  mutate(Group = replace(Group, Group %nin% majors, "Minors"),
         REGULATORY_AREA_NAME = factor(REGULATORY_AREA_NAME, levels = c("WESTERN GOA", "CENTRAL GOA", "EASTERN GOA", "GOA"))) %>% 
  select(YEAR, REGULATORY_AREA_NAME, Biomass, Group) %>% 
  group_by(YEAR, REGULATORY_AREA_NAME, Group) %>% 
  summarise(Group_Biom = sum(Biomass, na.rm = T)) %>% 
  mutate(Group = factor(Group, levels = c("Sharpchin_Rockfish","Harlequin_Rockfish", "Silvergray_Rockfish", "Redstripe_Rockfish",
                                          "Redbanded_Rockfish", "Minors", "Yelloweye_Rockfish")),
         Species = gsub("_Rockfish","",Group),
         Species = factor(Species, levels = c("Sharpchin","Harlequin", "Silvergray", "Redstripe",
                                              "Redbanded", "Minors", "Yelloweye")))
maxB <- RACE_B %>% 
  filter(REGULATORY_AREA_NAME == "GOA") %>%
  mutate(Group = replace(Group, Group %nin% majors, "Minors")) %>% 
  group_by(YEAR, Group) %>% 
  summarise(totB = sum(Biomass,na.rm=T)) %>% 
  group_by(Group) %>% 
  summarise(maxBiom = max(totB))

GOA_change <- RACE_B %>% 
  filter(REGULATORY_AREA_NAME == "GOA",
         YEAR == AYR | YEAR == AYR-2) %>% 
  mutate(Group = replace(Group, Group %nin% majors, "Minors")) %>% 
  group_by(YEAR, Group) %>% 
  summarise(Group_Biom = sum(Biomass, na.rm = T)) %>% 
  pivot_wider(names_from = YEAR, values_from = Group_Biom) %>% 
  rename(AYRB = 3, LAYR = 2) %>% 
  mutate(Pchange = paste(round(((AYRB - LAYR)/LAYR)*100,0),"%",sep=""),
         REGULATORY_AREA_NAME = "GOA",
         Group = factor(Group, levels = c("Sharpchin_Rockfish","Harlequin_Rockfish", "Silvergray_Rockfish", "Redstripe_Rockfish",
                                          "Redbanded_Rockfish", "Minors", "Yelloweye_Rockfish")),
         Species = gsub("_Rockfish","",Group),
         Species = factor(Species, levels = c("Sharpchin","Harlequin", "Silvergray", "Redstripe",
                                              "Redbanded", "Minors", "Yelloweye")),
         REGULATORY_AREA_NAME = factor(REGULATORY_AREA_NAME, levels = c("GOA"))) %>% 
  left_join(maxB)

biom_change <- ggplot(RACE_B2, aes(x=YEAR, y=Group_Biom/1000, color = Species, fill = Species, shape = Species))+
  geom_point(show.legend = F)+
  geom_line(show.legend = F)+
  geom_text(data = GOA_change, aes(x = 2015, y = maxBiom/1000, label = Pchange), color = "black")+
  labs(y="Survey Biomass (1,000s mt)",x="Year")+
  facet_grid(Species~REGULATORY_AREA_NAME, scales = "free")+
  theme_doc()
ggsave("GOAOROX_biom_change_doc.png", path = figdir, plot=biom_change,dpi=600, bg="transparent",width=15,height=9)

TierB <- read_csv(paste(getwd(),"/Output/",AYR,"/RACE_Biomass/RACE_Biomass_GOA_OROX.csv",sep=""))
TierB2 <- TierB %>% 
  filter(Group == "OROX_allnoSC" | Group == "OROX_SC",
         SURVEY == "GOA") %>% 
  mutate(Tier = ifelse(Group == "OROX_SC", "Tier4", "Tier5"),
         REGULATORY_AREA_NAME = factor(REGULATORY_AREA_NAME, levels = c("WESTERN GOA", "CENTRAL GOA", "EASTERN GOA", "GOA")))

maxTB <- TierB %>% 
  filter(Group == "OROX_allnoSC" | Group == "OROX_SC",
         REGULATORY_AREA_NAME == "GOA") %>%
  mutate(Tier = ifelse(Group == "OROX_SC", "Tier4", "Tier5")) %>% 
  group_by(YEAR, Tier) %>% 
  summarise(totB = sum(Biomass,na.rm=T)) %>% 
  group_by(Tier) %>% 
  summarise(maxBiom = max(totB))

GOA_Tchange <- TierB %>% 
  filter(Group == "OROX_allnoSC" | Group == "OROX_SC",
         REGULATORY_AREA_NAME == "GOA",
         YEAR == AYR | YEAR == AYR-2) %>% 
  mutate(Tier = ifelse(Group == "OROX_SC", "Tier4", "Tier5")) %>% 
  group_by(YEAR, Tier) %>% 
  summarise(Group_Biom = sum(Biomass, na.rm = T)) %>% 
  pivot_wider(names_from = YEAR, values_from = Group_Biom) %>% 
  rename(AYRB = 3, LAYR = 2) %>% 
  mutate(Pchange = paste(round(((AYRB - LAYR)/LAYR)*100,0),"%",sep=""),
         REGULATORY_AREA_NAME = "GOA",
         REGULATORY_AREA_NAME = factor(REGULATORY_AREA_NAME, levels = c("GOA"))) %>% 
  left_join(maxTB) 

Tier_b_change <- ggplot(TierB2, aes(x=YEAR, y=Biomass/1000, color = Tier, fill = Tier, shape = Tier))+
  geom_point(show.legend = F)+
  geom_line(show.legend = F)+
  geom_text(data = GOA_Tchange, aes(x = 2015, y = (maxBiom/1000)-(maxBiom/1000)*0.1, label = Pchange), color = "black")+
  facet_grid(Tier~REGULATORY_AREA_NAME, scales = "free") +
  labs(y="Survey Biomass (1,000s mt)",x="Year")+
  scale_x_continuous(expand = c(0,0))+
  scale_y_continuous(expand = c(0,0))+
  theme_doc()
ggsave("GOAOROX_Tierbiom_change_doc.png", path = figdir, plot=Tier_b_change,dpi=600, bg="transparent",width=15,height=9)

# Changes in Catch
majors <- c("SILVERGRAY ROCKFISH", "HARLEQUIN ROCKFISH", "REDSTRIPE ROCKFISH", "RED BANDED ROCKFISH", 
            "SHARPCHIN ROCKFISH", "YELLOWEYE ROCKFISH")
OROX_catch <- read_csv(paste(getwd(),"/Output/",AYR,"/Catch/OROXcomplex_catch",AYR,"_confidential.csv",sep=""))

GOA_spec_catch <- OROX_catch %>% 
  mutate(NORPAC_name = replace(NORPAC_name, NORPAC_name %nin% majors, "MINOR ROCKFISH")) %>% 
  group_by(year, NORPAC_name) %>% 
  summarise(Catch_mt = sum(tot_catch, na.rm = T)) %>% 
  mutate(Species = gsub(" ROCKFISH","", NORPAC_name),
         Species = factor(Species, levels = c("SHARPCHIN","HARLEQUIN", "SILVERGRAY", "REDSTRIPE",
                                              "RED BANDED", "YELLOWEYE", "MINOR")),
         fmp_subarea = "GOA")

spec_catch <- OROX_catch %>% 
  mutate(NORPAC_name = replace(NORPAC_name, NORPAC_name %nin% majors, "MINOR ROCKFISH")) %>% 
  group_by(year, fmp_subarea, NORPAC_name) %>% 
  summarise(Catch_mt = sum(tot_catch, na.rm = T)) %>% 
  mutate(Species = gsub(" ROCKFISH","", NORPAC_name),
         Species = factor(Species, levels = c("SHARPCHIN","HARLEQUIN", "SILVERGRAY", "REDSTRIPE",
                                              "RED BANDED", "YELLOWEYE", "MINOR"))) %>% 
  bind_rows(GOA_spec_catch) %>% 
  mutate(fmp_subarea = factor(fmp_subarea, levels = c("WG", "CG", "WY", "SE", "GOA")))

ggplot(spec_catch, aes(x = year, y = Catch_mt, color = Species, fill = Species))+
  geom_point(show.legend = F)+
  geom_line(show.legend = F)+
  facet_grid(Species~fmp_subarea, scales = "free")

GOA_minor_catch <- OROX_catch %>% 
  filter(NORPAC_name %nin% majors) %>% 
  group_by(year, NORPAC_name) %>% 
  summarise(Catch_mt = sum(tot_catch, na.rm = T)) %>% 
  mutate(Species = gsub(" ROCKFISH","", NORPAC_name),
         fmp_subarea = "GOA")
minor_spec_catch <- OROX_catch %>% 
  filter(NORPAC_name %nin% majors) %>% 
  mutate(Species = gsub(" ROCKFISH","", NORPAC_name)) %>% 
  select(year, NORPAC_name, tot_catch, Species, fmp_subarea) %>% 
  rename(Catch_mt = tot_catch) %>% 
  bind_rows(GOA_minor_catch) %>% 
  mutate(fmp_subarea = factor(fmp_subarea, levels = c("WG", "CG", "WY", "SE", "GOA")))
ggplot(minor_spec_catch, aes(x = year, y = Catch_mt, color = Species, fill = Species))+
  geom_point(show.legend = F)+
  geom_line(show.legend = F)+
  facet_grid(Species~fmp_subarea, scales = "free")
#note that shortbelly shows up as missing for 2010 to present becaus there hasn't been any catch and I didn't bother putting in zeros

# Discards ----
OROX_disc <- read_csv(paste(getwd(),"/Output/",AYR,"/Catch/OROX_discards",AYR,"_confidential.csv",sep=""))

OROX_disc <- OROX_disc %>% 
  mutate(fmp_subarea = factor(fmp_subarea, levels = c("WG", "CG", "WY", "SE", "GOA")),
         OROXsubgroup = factor(OROXsubgroup, levels = c("OROX", "Slope", "DSR"))) %>% 
  filter(year > 2009)
ggplot(OROX_disc, aes(x = year, y = disc_rate, color = OROXsubgroup, shape = OROXsubgroup))+
  geom_point(show.legend = F)+
  geom_line(show.legend = F)+
  facet_grid(OROXsubgroup~fmp_subarea, scales = "free")

OROXgear_disc <- read_csv(paste(getwd(),"/Output/",AYR,"/Catch/OROX_gear_discards",AYR,"_confidential.csv",sep=""))

OROXgear_disc <- OROXgear_disc %>% 
  mutate(fmp_subarea = factor(fmp_subarea, levels = c("WG", "CG", "WY", "SE", "GOA")),
         OROXsubgroup = factor(OROXsubgroup, levels = c("OROX", "Slope", "DSR"))) %>% 
  filter(year > 2009,
         gear != c("unk"), gear != c("JIG"))
# need to take out artifical zeros
ggplot(OROXgear_disc, aes(x = year, y = disc_rate, color = OROXsubgroup, shape = OROXsubgroup))+
  geom_point(show.legend = F)+
  geom_line(show.legend = F)+
  facet_grid(OROXsubgroup+gear~fmp_subarea)

#Discards by harvest sector, not in the discard summary made in catch code
OROX_catch_dat<-read_csv(paste(getwd(), "/Data/Annual_updates/", AYR, "/CAS_GFtotfishery_confidential.csv", sep=""))
OROX_codes<-read.csv(paste(getwd(),"/Data/Static/GOA_OROX_codes.csv",sep=""), header=T)

NROX_disc <- OROX_catch_dat %>% 
  clean_names() %>% 
  mutate(species = gsub('[\"]', '', species),
         species = as.character(noquote(species))) %>% 
  rename(CAS_name = species) %>% 
  filter(CAS_name == "rockfish, northern",
         fmp_subarea != "WG" & fmp_subarea != "CG") %>% 
  select(fmp_subarea, year, CAS_name, retained_discarded, catch_mt, gear, harvest_sector) 

DSRspec <- c("rockfish, canary", "rockfish, china", "rockfish, copper", "rockfish, quillback",
             "rockfish, rosethorn", "rockfish, tiger", "rockfish, yelloweye")
DSR_disc <- OROX_catch_dat %>% 
  clean_names() %>% 
  mutate(species = gsub('[\"]', '', species),
         species = as.character(noquote(species))) %>% 
  rename(CAS_name = species) %>% 
  filter(CAS_name %in% DSRspec,
         fmp_subarea != "SE") %>% 
  select(fmp_subarea, year, CAS_name, retained_discarded, catch_mt, gear, harvest_sector)

OROX_catch_dat2 <- OROX_catch_dat %>% 
  clean_names() %>% 
  select(fmp_subarea, year, species, retained_discarded, catch_mt, gear, harvest_sector) %>% 
  mutate(species = gsub('[\"]', '', species),
         species = as.character(noquote(species))) %>% 
  rename(CAS_name = species) %>% 
  filter(CAS_name != "rockfish, northern",
         CAS_name != "rockfish, dusky",
         CAS_name %nin% DSRspec) %>% # SE/WY catch will be added back in
  bind_rows(NROX_disc, DSR_disc) %>% 
  mutate(gear = if_else(gear == "PTR", "TWL", 
                        if_else(gear == "NPT", "TWL", gear))) %>% 
  filter(gear %in% c("HAL", "TWL")) %>% 
  group_by(fmp_subarea, year, retained_discarded, gear, harvest_sector) %>% 
  summarise(catch_sum = sum(catch_mt)) %>% 
  pivot_wider(names_from = retained_discarded, values_from = catch_sum) %>% 
  replace(is.na(.),0) %>% 
  mutate(tot_catch = Retained+Discarded,
         disc_rate = Discarded/tot_catch,
         fmp_subarea = if_else(fmp_subarea == "WG", "WESTERN",
                               if_else(fmp_subarea == "CG", "CENTRAL",
                                       if_else(fmp_subarea == "WY", "WYAK", "SOUTHEAST"))),
         fmp_subarea = factor(fmp_subarea, levels = c("WESTERN", "CENTRAL", "WYAK", "SOUTHEAST")),
         fakeorder = if_else(fmp_subarea == "WESTERN",4,
                             if_else(fmp_subarea == "CENTRAL",3,
                                     if_else(fmp_subarea == "WYAK",2,1)))) %>% 
  arrange(fakeorder)

sector_disc <- ggplot(OROX_catch_dat2, aes(x = year, y = disc_rate, color = gear, shape = gear))+
  geom_point(show.legend = F)+
  geom_line(show.legend = F)+
  facet_grid(gear+harvest_sector~fmp_subarea)+
  labs(x = "Year", y = "Discard Rate")+
  theme_doc()+
  theme(panel.grid.major = element_line(color = "grey90"))
ggsave("GOAOROX_sect_disc_doc.png", path = figdir, plot=sector_disc,dpi=600, bg="transparent",width=15,height=9)

















