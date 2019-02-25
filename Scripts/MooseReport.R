
###########
 # for mapping

OSR <- readOGR("C:/Users/erneilso/Documents/MooseReport/OSRPoly","OilsandRegionDissolve10TM")
AB <- readOGR("W:/EDM/Fire/projects/Caribou_ADM_IF/Caribou_ADM_IF_Projected data/Admin_Basemaps/Provinces","AB_Outline")

AB=spTransform(AB,proj4string(OSR))
WMU=spTransform(WMU,proj4string(OSR))

WMU$OSR <- over(WMU,OSR)[,1]
plot(WMU)
plot(WMU[which(!is.na(WMU$OSR)),],add=T,col="red")
plot(OSR,add=T,col="black")
plot(AB,add=T)


# # Next the shapefile has to be converted to a dataframe for use in ggplot2
# WMUdf <- fortify(WMU)
# 
# map <- ggplot() +
#   geom_path(data = WMUdf, 
#             aes(x = long, y = lat, group = group),
#             color = 'gray', fill = 'white', size = .2)


# Moose density
# here I will summarize the density for all years it was collected using the GoA data
# I will also caluculate lambda using all data and within methods (distance vs gasaway)
# the point here is to have estimates for comparison with dave huggard

# Additoinal notes, WMUs with populaiton estimates for reserve AND non-resever land: 500

## 
View(dat%>% filter(WMU==726,Response=="Density"))
table(dat$Method)


# density data
GDens <- dat%>%    # moose density
  mutate(WMU = recode(WMU,'530 Full' = '530'))%>%
  filter(Species == "Moose",
         Response %in% c("Density"),
         WMU !="",
         nchar(as.character(WMU))==3,
         MetricNum != 0,
         StartYear>1990,
         Method %in% c("CensusFlights"),
         DataCollected=="Distance"
         )%>%
  select(Response,WMU,MetricNum,CIL,CIU,SampleSize,StartYear)

GDens$WMU=factor(GDens$WMU,levels=unique(GDens$WMU))
GDens$Source <-"GOA"
GDens$Density=GDens$MetricNum
GDens$Density[GDens$DensityUnit=="1000km2"]=GDens$MetricNum[GDens$DensityUnit=="1000km2"]/1000
GDens$Prec=((GDens$CIU-GDens$CIL)/GDens$Density)*100



##
# ABMI densities
ADens <- read.csv("ABMIMetrics.csv")
ADens$WMU=as.factor(ADens$WMU)
ADens$Prec=((ADens$CIU-ADens$CIL)/ADens$Density)*100

# output precision against power
# ggplot(ADens,aes(SampleSize,Prec))+
#   geom_point()+
#   geom_smooth(method="lm",level=0.95)+
#   annotate("text",x=80,y=250,size=5,label=
#              paste("italic(R) ^ 2 ==",
#                    round(summary(lm(Prec~SampleSize, ADens))$adj.r.squared,2),sep=""),parse=T)+
#   theme_bw(base_size=15)+
#   ylab("CI Width as % of Density Estimate ")+
#   xlab("Number of Cameras")+
#   theme(panel.border = element_blank(),
#         panel.grid.minor = element_blank(),
#         axis.line.x = element_line(color="black", size = 1),
#         axis.line.y = element_line(color="black", size = 1),
#         axis.text.x=element_text(size=15,angle = 45,hjust=1),
#         axis.text.y=element_text(size=15))

###
# Method Comparison
DensFull = rbind.fill(ADens,GDens)
table(DensFull$WMU,DensFull$Source)

pd <- position_dodge(0.45)


# plotting both along the x axis 
tiff(file="C:/Users/erneilso/Documents/MooseReport/Figures/DensCompWMU.tiff",width = 1200, height = 1050, units = "px")
DensFull%>%
  filter(WMU %in% unique(GDens$WMU),WMU %in% unique(ADens$WMU))%>%
  ggplot(aes(WMU,Density,color=Source))+
  geom_errorbar(aes(ymin=CIL,ymax=CIU),width=0.5,position=pd,size=1.25) +
  geom_point(size=4,position=pd)+
  scale_color_manual(values=c("blue4","chocolate4"),name="")+
  theme_bw(base_size=25)+
  theme(panel.grid.minor = element_blank(),
        axis.title=element_text(size=25,face="bold"),
        legend.text = element_text(size=25),
        axis.text.x=element_text(size=25,angle = 45,hjust=1,face="bold"),
        axis.text.y=element_text(size=25,face="bold"))
dev.off()

# output precision against power
dat_text <- data.frame(
  label = c(paste("R2 = ",round(summary(lm(Prec~SampleSize, GDens))$adj.r.squared,2),sep=""),
            paste("R2 = ",round(summary(lm(Prec~SampleSize, ADens))$adj.r.squared,2),sep="")),
  Source=c("GOA","ABMI"),
  x     = c(400, 60),
  y     = c(250, 250)
)

# plotting the effect of sampling effort
tiff(file="C:/Users/erneilso/Documents/MooseReport/Figures/EffortPrec.tiff",width = 1200, height = 1050, units = "px")
ggplot(DensFull,aes(SampleSize,Prec))+
  geom_point(size=3)+
  geom_smooth(method="lm",level=0.95)+
  facet_grid(.~Source,scales="free")+
  geom_text(data=dat_text,mapping = aes(x = x, y = y, label = label),size=10)+
  theme_bw(base_size=25)+
  ylab("CI Width as % of Density Estimate ")+
  xlab("Sample Size")+
  theme(panel.grid.minor = element_blank(),
        axis.title=element_text(size=25,face="bold"),
        axis.text.x=element_text(size=25,angle = 45,hjust=1),
        axis.text.y=element_text(size=25))
dev.off()


# wide format
AD <- ADens
names(AD)[-which(names(AD)=="WMU")] <-
  paste("A",names(AD)[-which(names(AD)=="WMU")],sep="")
AD <- merge(AD,GDens,by="WMU")
AD$Difference <- AD$Density-AD$ADensity
plot(ADensity~Density,AD)

# comparison with regression
AD$ABMIWeights <- 1 / ((AD$ACIU-AD$ACIL)^2)
AD$ABMIWeights[which(is.na(AD$ABMIWeights))] <- mean(AD$ABMIWeights[which(AD$ABMIWeights<1000)],na.rm=T)
plot(AD$ABMIWeights[which(AD$ABMIWeights<1000)])

summary(lm(ADensity~Density,AD))$adj.r.squared
BFit = lm(ADensity~Density-1,AD,weights = ABMIWeights)

tiff(file="C:/Users/erneilso/Documents/MooseReport/Figures/DensCompRegress_NoBestFit.tiff",width = 1200, height = 1050, units = "px")
ggplot(AD,aes(Density,ADensity))+
  geom_point(size=4)+
  theme_bw(base_size=25)+
  geom_errorbar(aes(ymin=ACIL, ymax=ACIU)) +
  # annotate(x=0.2,y=5,"text",size=10,
  #          label=paste("R2 = ",round(summary(BFit)$adj.r.squared,2),sep=""))+
  # geom_smooth(formula = lm(ADensity~Density-1,AD,weights = AD$ABMIWeights),level=0.95)+
  geom_abline(intercept = 0, slope=coef(summary(BFit))[1])+
  ylab(bquote('ABMI Camera CRT Density ( '*km^2*')'))+
  xlab(bquote('GoA Distance Sampline Density  ( '*km^2*')'))+
  theme(panel.grid.minor = element_blank(),
        axis.title=element_text(size=25,face="bold"),
        legend.text = element_text(size=25),
        axis.text.x=element_text(size=25,angle = 45,hjust=1,face="bold"),
        axis.text.y=element_text(size=25,face="bold"))
dev.off()


# mapping difference between ABMI and GOA
WMU=merge(WMU,AD[,c("WMU","Difference")],by="WMU")
row.names(WMU@data)=row.names(WMU) # fix row names (because this is a shapefile)
spplot(WMU,"Difference")


############
# plotting density in space and time
GD <- dat%>%    
  mutate(WMU = recode(WMU,'530 Full' = '530'))%>%
  filter(Species == "Moose",
         Response %in% c("Density"),
         WMU !="",
         nchar(as.character(WMU))==3,
         MetricNum != 0,
         StartYear>1990,
         Method =="CensusFlights")%>%
  mutate(Density=MetricNum)%>%
  mutate(Density = ifelse(DensityUnit=="1000km2", Density/1000, Density))%>%
  group_by(WMU)%>%  # either WMU or StartYear
  summarise(D = mean(Density,na.rm=T),
            N=length(Density),
            DSE=sd(Density)/sqrt(N))

tiff(file="C:/Users/erneilso/Documents/MooseReport/Figures/DensityYear.tiff",width = 1200, height = 1050, units = "px")
print(
  ggplot(GD, aes(x=StartYear, y=D)) +
        geom_errorbar(aes(ymin=D-DSE, ymax=D+DSE), width=.1) +
        geom_point(size=4)+
        scale_x_continuous(breaks = c(1995,2000,2005,2010,2015)) +
        theme_bw(base_size=25)+
        ylab(bquote('Mean Annual Moose Density ( '*km^2*')'))+
        xlab("")+
        theme(panel.border = element_blank(),
              panel.grid.minor = element_blank(),
              axis.line.x = element_line(color="black", size = 1),
              axis.line.y = element_line(color="black", size = 1),
              axis.text.x=element_text(size=25,angle = 45,hjust=1),
              axis.text.y=element_text(size=25)))
dev.off()

WMU=merge(WMU,GD[,c("WMU","D")],by="WMU")
row.names(WMU@data)=row.names(WMU) # fix row names (because this is a shapefile)
spplot(WMU,"D")


##############
# Abundance
#  rmember to remove the second abundance value for 258 from teh strip width survey.....?
GA <- dat%>%    # moose density
  mutate(WMU = recode(WMU,'530 Full' = '530'))%>%
  filter(Species == "Moose",
         Response %in% c("Abundance"),
         WMU !="",
         nchar(as.character(WMU))==3,
         MetricNum != 0,
         StartYear>1990,
         Method %in% c("CensusFlights")
  )%>%
  select(Response,WMU,MetricNum,CIL,CIU,SampleSize,StartYear,Strata,DataCollected)%>%
  mutate(WMU=factor(WMU,levels=unique(WMU)),
         Prec=((CIU-CIL)/MetricNum)*100,
         Year=cut(StartYear,4),
         Method = ifelse(DataCollected=="Distance","Distance",
                         ifelse(Strata=="Transects" & DataCollected!="Distance","Transect Density",
                                ifelse(Strata =="Classified","Classified",
                                       ifelse(Strata=="Stratified", "Stratified",
                                              ifelse(Strata=="StratifiedRandomQuadrats","Random Block",
                                                     ifelse(Strata=="RandomBlock","Random Block",
                                                            ifelse(Strata=="StratifiedRandomQuadrats;Transects","Modified Gasaway",NA))))))))
                                
##
# population goals
unique(GA$WMU)
GA$Goal=as.numeric(NA)
GA$Goal[which(GA$WMU=="519")]=1877
GA$Goal[which(GA$WMU=="531")]=2512
GA$Goal[which(GA$WMU=="503")]=1100 # it was 400 up until 2014 from the 2015 report
GA$Goal[which(GA$WMU=="512")]=2000
GA$Goal[which(GA$WMU=="515")]=1000
GA$Goal[which(GA$WMU=="517")]=800
GA$Goal[which(GA$WMU=="516")]=500
GA$Goal[which(GA$WMU=="504")]=800
GA$Goal[which(GA$WMU=="509")]=1000
GA$Goal[which(GA$WMU=="518")]=2100


##
# goal success
tiff(file="C:/Users/erneilso/Documents/MooseReport/Figures/Success.tiff",width = 1200, height = 1050, units = "px")
GA%>%filter(!is.na(MetricNum),!is.na(Goal))%>%
  ggplot(aes(Goal,MetricNum,color=Year,group=Year))+
  geom_point(size=4,aes(shape=Method),position=position_dodge(width=50))+
  geom_errorbar(aes(x=Goal,ymin=CIL,ymax=CIU),width=70,size=1.25,position=position_dodge(width=50)) +
  xlim(0,2700)+
  ylim(0,3500)+
  geom_abline(slope=1,intercept=0)+
  ylab("WMU Estimated Population")+
  xlab("WMU Population Goal")+
  theme_bw(base_size=25)+
  theme(panel.grid.minor = element_blank(),
        axis.title=element_text(size=25,face="bold"),
        legend.text = element_text(size=20),legend.title = element_blank(),
        axis.text.x=element_text(size=25,angle = 45,hjust=1,face="bold"),
        axis.text.y=element_text(size=25,face="bold"))
dev.off()

# summary and mapping of goal success
Success <-GA%>%
  group_by(WMU)%>%
  summarise(WMUMean = mean(MetricNum,na.rm=T),
            WMUGoal = mean(Goal,na.rm=T))%>%
  mutate(Success = WMUMean/WMUGoal*100)%>%
  filter(!is.nan(Success))

WMU=merge(WMU,Success[,c("WMU","Success")],by="WMU")
row.names(WMU@data)=row.names(WMU) # fix row names (because this is a shapefile)
spplot(WMU,"Success")

#############


#######
# for mapping in Q

writeOGR(WMU,paste(getwd(),"/RShapes",sep=""),"WMU_Full",driver="ESRI Shapefile",overwrite_layer = T)
