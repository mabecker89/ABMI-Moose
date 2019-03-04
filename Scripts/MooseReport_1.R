library(plyr)
library(rgdal)
library(sp)
library(ggplot2)
library(tidyverse)
library(ggmap)


#############
#### Data ###
#############
# review data
dat=read.csv("Metrics.csv")

unique(dat$Title)
dat=dat[dat$Title!="Helminths in moose of Alberta",]



################
# spatial data
LAPR=readOGR("C:/Users/erneilso/Documents/MooseReport/PDF/MooseReview/Administrative/LAPR","LAPR_Proj")
WMU=readOGR("C:/Users/erneilso/Documents/MooseReport/PDF/MooseReview/Administrative","BF_WMU_POLYGON")

# projection
WMU=spTransform(WMU,
                CRS=CRS("+proj=tmerc +lat_0=0 +lon_0=-115 +k=0.9992 +x_0=500000 +y_0=0 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0"))

identicalCRS(LAPR,WMU)

plot(WMU)
plot(LAPR, add=T, col="Red")

WMU$WMU=as.numeric(as.character(WMU$WMUNIT_COD))

# summarizes
table(dat$Response)
ddply(dat, c("StartYear"),summarize,
      Resp=unique(Response))



#############
# Lambda ###
#############

# old calculations
MooseLam=dat[dat$Species=="Moose" & dat$Response=="Lambda",]


# from the random block/Gasaway reported values
MooseDens=dat[dat$Species=="Moose"  &
                dat$Response=="Density" &
                dat$WMU!="" &
                !is.na(dat$MetricNum) &
                dat$MetricNum!=0 &
                dat$StartYear>1990 &
                #dat$DataCollected %in% c("ArealCount","Distance")  # do not combine these, different detectability methods
                dat$DataCollected=="ArealCount" & !is.na(dat$DataCollected)
                ,]
MooseDens$StDens=MooseDens$MetricNum
MooseDens$StDens[MooseDens$DensityUnit=="1000km2"]=MooseDens$MetricNum[MooseDens$DensityUnit=="1000km2"]/1000
MooseDens[MooseDens==""]=NA
MooseDens=MooseDens[!is.na(MooseDens$WMU),]

# summary
length(unique(MooseDens$Title)) # fewer than density becuase it doesn't include any distance sampling
length(unique(MooseDens$WMU)) # fewer than WMU becuase 531 has two reports
table(MooseDens$StartYear)

# calculate# change
MooseDens$Ndelta=NA
MooseDens$Tdelta=NA
MooseDens$Adelta=NA
MooseDens$Delta=NA
MooseDens$Lambda=NA

MooseDens=MooseDens[order(MooseDens$WMU,MooseDens$StartYear),]
for (i in 2:nrow(MooseDens)){
  if(MooseDens$WMU[i]==MooseDens$WMU[i-1]){
    MooseDens$Ndelta[i]=MooseDens$StDens[i]-MooseDens$StDens[i-1]
    MooseDens$Tdelta[i]=MooseDens$StartYear[i]-MooseDens$StartYear[i-1]
    MooseDens$Adelta[i]=MooseDens$Ndelta[i]/MooseDens$Tdelta[i]
    MooseDens$Delta[i]=MooseDens$StDens[i-1]+MooseDens$Adelta[i]
    MooseDens$Lambda[i]=MooseDens$Delta[i]/MooseDens$StDens[i-1]
  }}

# Lambda by WMU
LambdaTab=ddply(MooseDens,"WMU",summarize,
                  N=length(Lambda),
                  Lambda=mean(Lambda,na.rm=T))
LambdaTab=LambdaTab[!is.na(LambdaTab$Lambda),]  # leaves only 9 WMUs
write.csv(LambdaTab,file="C:/Users/Eric/Google Drive/PDF/MooseReview/Figures/Lambda.csv")

# join to WMU
WMU_Join=merge(WMU,LambdaTab[,c("WMU","Lambda")],by="WMU")
spplot(WMU_Join,"Lambda")

# stats
mean(MooseDens$Lambda,na.rm=T)
sd(MooseDens$Lambda,na.rm=T)/sqrt(length(MooseDens$Lambda[!is.na(MooseDens$Lambda)]))


#############
# Denstiy ###
#############

############
# Density
# this table should be GoA WMU reports only, but including all density estimates
# not sure if this makes sense as the estimates are so spotty across space for each year. What does the annual average
# actually tell us? The within esimate of Lambda is much better I think
Trend=dat[dat$Species=="Moose"  &
                dat$Response=="Density" &
                dat$WMU!="" &  # this removes the density disturbance aeffects paper, which is ok because that paper cover huge areas, density - 0.27
                !is.na(dat$MetricNum) &
                dat$MetricNum!=0 &
                dat$StartYear>1990 
                #dat$DataCollected=="ArealCount" & !is.na(dat$DataCollected)
              ,]
# with pipes
Trend <- dat%>%
  filter(Species == "Moose",
         Response == "Density",
         WMU !="",
         MetricNum != 0,
         StartYear>1990)

Trend$StDens=Trend$MetricNum
Trend$StDens[Trend$DensityUnit=="1000km2"]=Trend$MetricNum[Trend$DensityUnit=="1000km2"]/1000
Trend[Trend==""]=NA
Trend[Trend==0]=NA
Trend=Trend[!is.na(Trend$WMU),]

# summary
length(unique(Trend$Title))/length(unique(dat$Title))
table(Trend$StartYear)

# remove WMU years with more than one density estimate
Trend$WMUYear=paste(Trend$WMU,Trend$StartYear,sep="_")
table(Trend$WMUYear)
#View(Trend[duplicated(Trend$WMUYear),])
#View(Trend[Trend$WMUYear=="258_2016",])
Trend=Trend[!(Trend$WMUYear=="258_2016" & Trend$DataCollected=="ArealObs"),]

# linear model
# summary(lm(StDens~StartYear+DataCollected,data=Trend))
     
# mean by year 
TrendTab=ddply(Trend,"StartYear",summarize,
      N=length(StDens),
      D=mean(StDens),
      DSE=sd(StDens)/sqrt(N))
TrendTab$Year=as.factor(TrendTab$StartYear)

# number of WMU reported on average per year
mean(TrendTab$N)
sd(TrendTab$N) # this should be sd because it is the deviation across number of reports per year -ie the whole population

png(file="C:/Users/Eric/Google Drive/PDF/MooseReview/Figures/Trend.png",width=700,height=500)
print(ggplot(TrendTab, aes(x=StartYear, y=D)) +
        geom_errorbar(aes(ymin=D-DSE, ymax=D+DSE), width=.1) +
        geom_point(size=4)+
        scale_x_continuous(breaks = c(1995,2000,2005,2010,2015)) +
        theme_bw(base_size=15)+
        ylab(bquote('Mean Annual Density  ( '*km^2*')'))+
        xlab("")+
        theme(panel.border = element_blank(),
              panel.grid.minor = element_blank(),
              axis.line.x = element_line(color="black", size = 1),
              axis.line.y = element_line(color="black", size = 1),
              axis.text.x=element_text(size=15,angle = 45,hjust=1),
              axis.text.y=element_text(size=15)))
dev.off()

###
# current densities
CTrend=Trend[Trend$DataCollected=="Distance" & !is.na(Trend$DataCollected),]
hist(CTrend$StDens,breaks=10)
mean(CTrend$StDens)
sd(CTrend$StDens)/sqrt(nrow(CTrend))

# interval width
CTrend$Interval=CTrend$CIU-CTrend$CIL
mean(CTrend$Interval)

# compare to block surveys
BTrend=Trend[Trend$DataCollected!="Distance" & !is.na(Trend$DataCollected),]
hist(BTrend$StDens,breaks=10)
mean(BTrend$StDens)
sd(BTrend$StDens)/sqrt(nrow(BTrend))

# interval width
BTrend$Interval=BTrend$CIU-BTrend$CIL
mean(BTrend$Interval,na.rm=T)


# join to WMU
WMU_Join=merge(WMU_Join,CTrend[,c("WMU","StDens")],by="WMU")
spplot(WMU_Join,"StDens")




###################
## Demographics ###
###################

Ratios=dat[(dat$Species=="Moose" & dat$Response=="CalfCow") |
             (dat$Species=="Moose" & dat$Response=="MaleFemale"),]
length(unique(Ratios$Title))/length(unique(dat$Title))  # percentage of papers with ratio demographics
Ratios[Ratios==""]=NA

# standarize ratios
Ratios$R=NA
for (i in 1:nrow(Ratios)){
  if(!is.na(Ratios$MetricText[i])){
    denom=as.numeric(unlist(strsplit(as.character(Ratios$MetricText[i]),":"))[2])
    numer=as.numeric(unlist(strsplit(as.character(Ratios$MetricText[i]),":"))[1])
    Ratios$R[i]=numer/denom}
  else{
    Ratios$R[i]=Ratios$MetricNum[i]}
}

# calf by year 
View(Ratios[Ratios$Response=="CalfCow",])
CalfTab=ddply(Ratios[Ratios$Response=="CalfCow",],"StartYear",summarize,
              N=length(R),
              Calf=mean(R),
              SE=sd(R)/sqrt(N))

png(file="C:/Users/Eric/Google Drive/PDF/MooseReview/Figures/Calf.png",width=500,height=500)
print(ggplot(CalfTab[CalfTab$StartYear>1990,], aes(x=StartYear, y=Calf)) +
        geom_errorbar(aes(ymin=Calf-SE, ymax=Calf+SE), width=.1) +
        geom_point(size=4)+
        scale_x_continuous(breaks = c(1995,2000,2005,2010,2015)) +
        theme_bw(base_size=15)+
        ylab(bquote('Mean Annual Calf:Cow Ratio'))+
        xlab("")+
        theme(panel.border = element_blank(),
              panel.grid.minor = element_blank(),
              axis.line.x = element_line(color="black", size = 1),
              axis.line.y = element_line(color="black", size = 1),
              axis.text.x=element_text(size=15,angle = 45,hjust=1),
              axis.text.y=element_text(size=15)))
dev.off()

# bull by year 
BullTab=ddply(Ratios[Ratios$Response=="MaleFemale",],"StartYear",summarize,
               N=length(R),
               Bull=mean(R),
               SE=sd(R)/sqrt(N))

png(file="C:/Users/Eric/Google Drive/PDF/MooseReview/Figures/Bull.png",width=500,height=500)
print(ggplot(BullTab[BullTab$StartYear>1990,], aes(x=StartYear, y=Bull)) +
        geom_errorbar(aes(ymin=Bull-SE, ymax=Bull+SE), width=.1) +
        geom_point(size=4)+
        scale_x_continuous(breaks = c(1995,2000,2005,2010,2015)) +
        theme_bw(base_size=15)+
        ylab(bquote('Mean Annual Bull:Cow Ratio'))+
        xlab("")+
        theme(panel.border = element_blank(),
              panel.grid.minor = element_blank(),
              axis.line.x = element_line(color="black", size = 1),
              axis.line.y = element_line(color="black", size = 1),
              axis.text.x=element_text(size=15,angle = 45,hjust=1),
              axis.text.y=element_text(size=15)))
dev.off()


###
# full ratios ######

# Calves
CalfWMU=Ratios[Ratios$Response=="CalfCow",c("WMU","R")] 

# do these summarizes before breaking apart the rows with multiple WMUs to avoid psuedo replication
hist(CalfWMU$R,breaks=10)
mean(CalfWMU$R)
sd(CalfWMU$R)/sqrt(nrow(CalfWMU))

i=1
End=nrow(CalfWMU)
while(i < End){
  X=unlist(strsplit(as.character(CalfWMU$WMU[i]),";"))
  if(length(X)>1){
    for(j in 1:length(X)){
      NewRow=data.frame(WMU=X[j],R=CalfWMU$R[i])
      CalfWMU=rbind(CalfWMU,NewRow)}
    CalfWMU=CalfWMU[-i,]
    i=i}
  else{i=i+1}
  End=nrow(CalfWMU)
}

# join to WMU
CalfWMUTab=ddply(CalfWMU,"WMU",summarize,
                 N=length(R),
                 Calf=mean(R),
                 SE=sd(R)/sqrt(N))
WMU_Join=merge(WMU_Join,CalfWMUTab,by="WMU")
spplot(WMU_Join,"Calf")

# Bulls 
BullWMU=Ratios[Ratios$Response=="MaleFemale",c("WMU","R")] 

# do these summarizes before breaking apart the rows with multiple WMUs to avoid psuedo replication
hist(BullWMU$R,breaks=10)
mean(BullWMU$R)
sd(BullWMU$R)/sqrt(nrow(BullWMU))

i=1
End=nrow(BullWMU)
while(i < End){
  X=unlist(strsplit(as.character(BullWMU$WMU[i]),";"))
  if(length(X)>1){
    for(j in 1:length(X)){
      NewRow=data.frame(WMU=X[j],R=BullWMU$R[i])
      BullWMU=rbind(BullWMU,NewRow)}
    BullWMU=BullWMU[-i,]
    i=i}
  else{i=i+1}
  End=nrow(BullWMU)
}

# join to WMU
BullWMUTab=ddply(BullWMU,"WMU",summarize,
                 N=length(R),
                 Bull=mean(R),
                 SE=sd(R)/sqrt(N))
WMU_Join=merge(WMU_Join,BullWMUTab,by="WMU")
spplot(WMU_Join,"Bull")



######
# survival

Surv=dat[dat$Response=="CalfSurvival" | dat$Response=="AdultSurvival" | dat$Response=="Survival",]

# review stats
length(unique(Surv$Title))/length(unique(dat$Title))  # percentage of papers 


# join to WMU
SurvTab=ddply(Surv,c("Response","StudyArea","TelemType"),summarize,
                 N=length(MetricNum),
                 Survival=mean(MetricNum),
                 SE=sd(MetricNum)/sqrt(N))
SurvTab=SurvTab[order(SurvTab$Response,SurvTab$StudyArea),]
write.csv(SurvTab,file="C:/Users/Eric/Google Drive/PDF/MooseReview/Figures/Surv.csv")


###################
## Distribution ###
###################
unique(dat$Response)


#occupancy
nrow(dat[dat$Response=="Occupancy",])  # none.....

# relative abundance in space in time should have a predictor variable
Distribution=dat[dat$Species=="Moose" &
                   dat$Response=="Density" &
                   !is.na(dat$Predictor) &
                   dat$Predictor!="",]


length(unique(Distribution$Title))/length(unique(dat$Title))  # percentage of papers with density distribution demographics

# effects
# removed distance from roads. In the paper it basically says roadsdist and the whitezone are collinear



###################
## Selection ###
###################

Selection=dat[dat$Species=="Moose" &
                   dat$Response=="Selection",]
Selection[Selection==""]=NA

# standardized value
Selection$S=NA
Selection$S[!is.na(Selection$MetricText)]=as.character(Selection$MetricText[!is.na(Selection$MetricText)])
Selection$S[is.na(Selection$S) & Selection$ResponseType=="beta"]=
  ifelse(Selection$MetricNum[is.na(Selection$S) & Selection$ResponseType=="beta"]>0,"Select","Avoid")
Selection$S[is.na(Selection$S) & Selection$ResponseType=="ratio"]=
  ifelse(Selection$MetricNum[is.na(Selection$S) & Selection$ResponseType=="ratio"]>1,"Select","Avoid")
Selection$S[Selection$StudyArea=="Alpac"]=ifelse(Selection$MetricNum[Selection$StudyArea=="Alpac"]>0.1111,"Select","Avoid")

# remove non-disturbance predictors
Selection=Selection[!Selection$Predictor %in% c("Upland","LowLand","Water","Shrub","WetMeadow","Conifer","Decid",
                                                "Mixed","ClosedConWet","OpenConWet","Swamp","Dogwood","River","RiverDist"),]
Selection$DataCollected1[Selection$Method!="Ground" & Selection$Method!="CensusFlight"]=as.character(Selection$Method[Selection$Method!="Ground" & Selection$Method!="CensusFlight"])
Selection$DataCollected1[Selection$Method=="Ground" | Selection$Method=="CensusFlight"]=as.character(Selection$DataCollected[Selection$Method=="Ground" | Selection$Method=="CensusFlight"])
Selection$DataCollected1[Selection$DataCollected1=="Telemetry"]=paste(Selection$TelemType[Selection$DataCollected1=="Telemetry"],"Telemery",sep="_")

# review stats
length(unique(Selection$Title))/length(unique(dat$Title))  # percentage of papers 
nrow(Selection[Selection$Scale=="Population",])/nrow(Selection)  
table(Selection$ResponseType)
nrow(Selection[Selection$TelemType=="GPS" & !is.na(Selection$TelemType),])/nrow(Selection)

# table for report
Sel=Selection[,c("Scale","Predictor","Season","PredictorInteraction","S","DataCollected1","Title")]

Sel=Sel[order(Sel$Scale,Sel$Predictor),]
write.csv(Sel,file="C:/Users/Eric/Google Drive/PDF/MooseReview/Figures/Sel.csv")


##########
## Use ###
##########

Use=dat[dat$Species=="Moose" &
           dat$Response %in% c("Use"),]

# table for report
Us=Use[Use$Predictor %in% c("Cleared","Mine","CrossingStructure"),c("Scale","Predictor","Season","MetricNum","Title")]
Us=Us[order(Us$Scale, Us$Predictor),]

# review stats
length(unique(Us$Title))/length(unique(dat$Title))  # percentage of papers 

write.csv(Us,file="C:/Users/Eric/Google Drive/PDF/MooseReview/Figures/Use.csv")


##########
## Move ###
##########
# HRA seasonal overlap speed
Move=dat[dat$Species=="Moose" &
           dat$Response %in% c("HRA","Speed","SeasonalOverlap"),]

HRATab=ddply(Move[Move$Response=="HRA",],"Season",summarize,
      N=length(MetricNum),
      HRA=mean(MetricNum),
      SE=sd(MetricNum)/sqrt(N))
write.csv(HRATab,file="C:/Users/Eric/Google Drive/PDF/MooseReview/Figures/HRA.csv")


##########
## Diet ###
##########
table(dat$Response)

# HRA seasonal overlap speed
Diet=dat[dat$Species=="Moose" &
           dat$Response %in% c("Diet"),]

# NONE!! I didn't review those diet studies because there were just natural history




#####
 # export big table and map
WMU_Join$Lambda1=round(WMU_Join$Lambda,digits=2)
WMU_Join$Lambda1[WMU_Join$Lambda1==0]=NA
WMU_Join$Density=round(WMU_Join$StDens,digits=2)
WMU_Join$Bull1=round(WMU_Join$Bull,digits=2)
WMU_Join$Calf1=round(WMU_Join$Calf,digits=2)

writeOGR(WMU_Join,"C:/Users/Eric/Google Drive/PDF/MooseReview/MooseCode/RShapes","WMU_Join",driver="ESRI Shapefile")

######################
# Big table 1 ######
####################

# density
Density=dat[dat$Species=="Moose"  &
            dat$Response=="Density" &
            #dat$WMU!="" &  # this removes the density disturbance aeffects paper, which is ok because that paper cover huge areas, density - 0.27
            !is.na(dat$MetricNum) &
            dat$MetricNum!=0 
            #dat$StartYear>1990 
          ,]
Density$StudyScale="WMU"
Density$StudyScale[Density$WMU==""]="Northern Alberta"
Density$StudyScale[Density$Extent==179]="Sub WMU"
Density$DataCollected[Density$DataCollected=="ArealObs"]="ArealCount"

DensityTab=ddply(Density,c("DataCollected","StudyScale"),summarize,
                 N=length(unique(Title)))
DensityTab$Variable="Density"
names(DensityTab)[c(1,2)]=c("Method","Scale")
DensityTab=as.data.frame(DensityTab)

# lambda
LambTab=ddply(MooseLam,c("DataCollected"),summarize,
               N=length(unique(Title)))
LambTab$Scale="Sub WMU"
LambTab$Variable="Lambda"
names(LambTab)[c(1)]=c("Method")
LambTab=as.data.frame(LambTab)


# ratios
Rat=Ratios
Rat$DataCollected[Rat$DataCollected=="ArealObs"]="ArealCount"
Rat$StudyScale="WMU"
Rat$StudyScale[Rat$StudyArea=="AOSERP"]="Multiple WMU"
Rat$StudyScale[Rat$StudyArea=="Athabasca Oil Sands Region"]="Multiple WMU"
Rat$DataCollected1[Rat$DataCollected=="ArealCount" | Rat$DataCollected=="Distance"]="ArealCount"
Rat$StudyScale[Rat$Extent==179]="Sub WMU"
RatTab=ddply(Rat,c("DataCollected1","StudyScale"),summarize,
                 N=length(unique(Title)))
RatTab$Variable="Demographic Ratios"
names(RatTab)[c(1,2)]=c("Method","Scale")
RatTab=as.data.frame(RatTab)

#Selection
Sele=Selection
Sele$DataCollected1=NA
Sele$DataCollected1[Sele$Method!="Ground" & Sele$Method!="CensusFlight"]=as.character(Sele$Method[Sele$Method!="Ground" & Sele$Method!="CensusFlight"])
Sele$DataCollected1[Sele$Method=="Ground" | Sele$Method=="CensusFlight"]=as.character(Sele$DataCollected[Sele$Method=="Ground" | Sele$Method=="CensusFlight"])
Sele$DataCollected1[Sele$DataCollected1=="Telemetry"]=paste(Sele$TelemType[Sele$DataCollected1=="Telemetry"],"Telemery",sep="_")
SeleTab=ddply(Sele,c("DataCollected1","Scale"),summarize,
             N=length(unique(Title)))
SeleTab$Variable="Selection"
names(SeleTab)[1]="Method"
SeleTab=as.data.frame(SeleTab)

# USe
US=Use[Use$Predictor %in% c("Cleared","Mine","CrossingStructure"),]
USTab=ddply(US,c("Method","Scale"),summarize,
              N=length(unique(Title)))
USTab$Variable="Use"
USTab=as.data.frame(USTab)

# Home ragne
hra=Move[Move$Response=="HRA",]
hra$Scale="Multiple WMU"
hra$Method="Telemetry"
hraTab=ddply(hra,c("Scale","Method"),summarize,
              N=length(unique(Title)))
hraTab$Variable="Home Range Area"
hraTab=as.data.frame(hraTab)

# surv
SURV=Surv
SURV$Scale="Multiple WMU"
SURV$Scale[SURV$Extent==287]="Sub WMU"
SURV$Method="Telemetry"
SURVTab=ddply(SURV,c("Scale","Method"),summarize,
             N=length(unique(Title)))
SURVTab$Variable="Survival"
SURVTab=as.data.frame(SURVTab)

# dist
DIST=Distribution
DIST$Scale1[DIST$StudyArea=="Elk Island National Park"]="Sub WMU"
DIST$Scale1[DIST$StudyArea!="Elk Island National Park"]="Northern Alberta"
DIST$DataCollected1[DIST$StudyArea=="Elk Island National Park"]="Scat"
DIST$DataCollected1[DIST$StudyArea!="Elk Island National Park"]="ArealCount"
DISTTab=ddply(DIST,c("Scale1","DataCollected1"),summarize,
              N=length(unique(Title)))
DISTTab$Variable="Distribution"
DISTTab=as.data.frame(DISTTab)
names(DISTTab)[c(1,2)]=c("Method","Scale")

# put em together


BigTab=rbind.fill(DensityTab,LambTab,SeleTab,USTab,RatTab,hraTab,SURVTab,DISTTab)
BigTab=BigTab[order(BigTab$Variable, BigTab$Scale),]
write.csv(BigTab,file="C:/Users/Eric/Google Drive/PDF/MooseReview/Figures/BigTab.csv")

# summaries
unique(dat$Title) #= 19
19/33
(33-19-1-1)/34
