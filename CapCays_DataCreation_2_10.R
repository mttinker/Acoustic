# BAYESIAN DATA CREATION AND PREPROCESSING
# Cap Cays script (WTSH & BLNO)
# created 28 February 2017
# updated 14 November 2017

rm(list=ls())
gc()
options(stringsAsFactors=FALSE)

# Input Variables -------------------------------------------------------------

species='WTSH' # WTSH or BLNO

ProjectName='Bayesian_2018'
ProjectYear=2018
ProjectLocation='CapCays'

audit='NoAudit' # NoAudit or Audit

YearStart=2014
YearStop=2017

TimeStepOne = 17 # hour of first time step
startdate='-10-01' # set desired startdate of surveys

# Set up dropbox and packages ---------------------------------------------------

Dropbox<-'D:/CM,Inc/Dropbox (CMI)/CMI_Team/Analysis'
if(Sys.info()[4]=='MatthewsMac')  Dropbox<-'~/Dropbox'
today<-format(Sys.Date(),'%d%b%y')

existing_Packages<-as.list(installed.packages()[,1])
required_Packages<-c('mgcv','foreach','doParallel','oce','lubridate','stringr','ggplot2','dtplyr','data.table','proj4','ggmap','R2wd','grid','gridExtra','zoo','OpenStreetMap','rgdal')
missing_Packages<- required_Packages[!required_Packages  %in% existing_Packages]
if(length(missing_Packages)>0)install.packages(pkgs =  missing_Packages)
invisible(lapply(required_Packages, require, character.only=T,quietly = T))

# registerDoParallel(makeCluster(4))

for(file in list.files(path = 'D:/CM,Inc/R-Code_improvements/Functions/',pattern = '\\.[Rr]$',full.names = T,recursive = T)){
    source(file)
}

# Data Load -------------------------------------------------------------------

if (YearStart<=2017 & YearStop>=2017) {
  
  if (!str_detect(tolower(species),'chick')) {
    data_2017_path=file.path(Dropbox,'2016/QPWS_CapCays_2016/QPWS_CapCays_2016_2017_Season/R_output',paste0('AnalysisData_2017R1_CapCays_',species,'24Oct17_NoAudit.Rda'))
  } else {
    # data_2017_path<-file.path(Dropbox,'2016/QPWS_CapCays_2016/QPWS_CapCays_2016_2017_Season/R_output',paste0('AnalysisData_2017R1_CapCays_',species,'_02Nov17.Rda'))
  }
  
  data_2017<-readRDS(data_2017_path)
  
  data_2017$contract_year<-2017
  data_2017$file_duration_sec<-NULL
  data_2017$file<-NULL
  data_2017$audit_file<-NULL
  data_2017$probeId<-NULL
  data_2017$key<-NULL
  
  data_2017<-filter(data_2017,!str_detect(SPID,'HISM'))
  
}

if (YearStart<=2016 & YearStop>=2016) {
  
  if (audit=='NoAudit') {
    data_2016_path=file.path(Dropbox,'2016/QPWS_CapCays_2016/QPWS_CapCays_2016_2016_Season/R_output',paste0('AnalysisData_QPWS_CapCays_2016_',species,'20Apr17_NoAudit.Rda'))
  } else {
    # data_2016_path<-file.path(Dropbox,'2016/QPWS_CapCays_2016/QPWS_CapCays_2016_2016_Season/R_output',paste0('AnalysisData_QPWS_CapCays_2016_',species,'20Apr17.Rda'))
  }
  
  data_2016<-readRDS(data_2016_path)
  
  data_2016$contract_year<-2016
  data_2016$file_duration_sec<-NULL
  data_2016$file<-NULL
  data_2016$audit_file<-NULL
  data_2016$MinuteID<-NULL

  if (!str_detect(audit,'NoAudit')) {
    if (species=='WTSH') {
      DateStart = as.Date('2016-02-19')
      DateEnd = as.Date('2016-04-19')
      RiseOrSet = 'MinFromSunrise'
      PeakStart = (-150)
      PeakEnd = (-30)
    } else if (species=='WTSH_Chick') {
      DateStart = as.Date('2016-02-19')
      DateEnd = as.Date('2016-05-24')
      RiseOrSet = 'MinFromSunrise'
      PeakStart = (-150)
      PeakEnd = (-30)
    } else if (species=='BLNO') {
      DateStart = as.Date('2016-02-19')
      DateEnd = as.Date('2016-04-19')
      RiseOrSet = 'MinFromSunrise'
      PeakStart = (-60)
      PeakEnd = 120
    } else if (species=='BLNO_Chick') {
      DateStart = as.Date('2016-01-15')
      DateEnd = as.Date('2016-04-19')
      RiseOrSet = 'MinFromSunrise'
      PeakStart = (-60)
      PeakEnd = 120
    }
    data_2016<-filter(data_2016, Date>=DateStart & Date<=DateEnd)
    data_2016<-filter(data_2016,data_2016[,RiseOrSet]>=PeakStart & data_2016[,RiseOrSet]<=PeakEnd)
  }
  
}

if (YearStart<=2015 & YearStop>=2015) {
  
  if (audit=='Audit') {
    if (!str_detect(tolower(species),'chick')) {
      data_2015_path=paste0(Dropbox,'/2015/NPRSR_Cap_Cays_2015/R_output/AnalysisData_NPRSR_Cap_Cays_2015_',species,'01Feb17.Rda')
    }
  } else if (audit=='NoAudit') {
    if (!str_detect(tolower(species),'chick')) {
      data_2015_path=paste0(Dropbox,'/2015/NPRSR_Cap_Cays_2015/R_output/AnalysisData_NPRSR_Cap_Cays_2015_',species,'_NoAudit01Feb17.Rda')
    }
  }

  data_2015<-readRDS(data_2015_path)
  
  data_2015$contract_year<-2015
  data_2015$file_duration_sec<-NULL
  data_2015$file<-NULL
  data_2015$audit_file<-NULL
  data_2015$Site_Type<-NULL
  
}

if (YearStart<=2014 & YearStop>=2014) {
  
  if (audit=='Audit') {
    if (!str_detect(tolower(species),'chick')) {
      data_2014_path=paste0(Dropbox,'/2014/NPRSR_Cap_Cays_2014/R_output/AnalysisData_NPRSR_Cap_Cays_2014_',species,'01Feb17.Rda')
    } else {
      data_2014_path=paste0(Dropbox,'/2014/NPRSR_Cap_Cays_2014/R_output/AnalysisData_NPRSR_Cap_Cays_2014_',species,'22Feb17.Rda')
    } 
  } else if (audit=='NoAudit') {
    if (!str_detect(tolower(species),'chick')) {
      data_2014_path=paste0(Dropbox,'/2014/NPRSR_Cap_Cays_2014/R_output/AnalysisData_NPRSR_Cap_Cays_2014_',species,'_NoAudit01Feb17.Rda')
    } else {
      data_2014_path=paste0(Dropbox,'/2014/NPRSR_Cap_Cays_2014/R_output/AnalysisData_NPRSR_Cap_Cays_2014_',species,'_NoAudit22Feb17.Rda')
    }
  }
  
  data_2014<-readRDS(data_2014_path)
  
  data_2014$contract_year<-2014
  data_2014$file_duration_sec<-NULL
  data_2014$file<-NULL
  data_2014$audit_file<-NULL
  
}


# Data cleanup and combine years ----------------------------------------------

# combine years
if (exists('data_2017')) {
  data_ALL<-data_2017
  rm(data_2017)
}
if (exists('data_2016')) {
  if (exists('data_ALL')) { data_ALL<-bind_rows(data_ALL,data_2016) } else { data_ALL<-data_2016 }
  rm(data_2016)
} 
if (exists('data_2015')) {
  if (exists('data_ALL')) { data_ALL<-bind_rows(data_ALL,data_2015) } else { data_ALL<-data_2015 }
  rm(data_2015)
}
if (exists('data_2014')) {
  if (exists('data_ALL')) { data_ALL<-bind_rows(data_ALL,data_2014) } else { data_ALL<-data_2014 }
  rm(data_2014)
}

sapply(data_ALL, function(x) sum(is.na(x)))

data_ALL<-data_ALL[!is.na(data_ALL$flux_sensitive),]
data_ALL<-data_ALL[!is.na(data_ALL$level_absolute),]
data_ALL<-data_ALL[!is.infinite(data_ALL$level_absolute),]

data_ALL<-select(data_ALL,-Mic_Type,-Round)

# Check for duplicates
data_ALL$key<-paste(data_ALL$SPID, data_ALL$DateTime)
sum(duplicated(data_ALL$key))
data_ALL<-subset(data_ALL,!duplicated(data_ALL$key))

# data_ALL<-select(data_ALL,-c(Island))

# bring in site type/region
SiteTypeRegion_Info<-read.csv(file.path(Dropbox,ProjectYear,ProjectName,'data',ProjectLocation,paste0(ProjectLocation,'_SiteInfo_ALL.csv')))
data_ALL$Region<-NULL
data_ALL$SuperRegion<-NULL
data_ALL$Site_Type<-NULL
data_ALL$Latitude<-NULL
data_ALL$Longitude<-NULL
data_ALL$Elevation<-NULL
data_ALL$Island<-NULL
data_ALL$Easting<-NULL
data_ALL$Northing<-NULL
nrow(data_ALL)
data_ALL<-left_join(data_ALL, SiteTypeRegion_Info, by=c('SPID','contract_year'))
nrow(data_ALL)

data_ALL[data_ALL$contract_year==2014 & data_ALL$SPID=='NW7',]$SPID<-'NW7 2014'

# # Map all years survey points -------------------------------------------------
# 
# source('D:/CM,Inc/R-Code_improvements/Mapping/CMIMapFun.R')
# source('D:/CM,Inc/R-Code_improvements/Functions/getExtents.R')
# 
# MappingTable<-data_ALL %>%
#   group_by(SPID,Sensor_Name,contract_year) %>%
#   summarise(Latitude=unique(Latitude),Longitude=unique(Longitude))
# 
# # to get available extents, run getExtents('names')
# map_extent=getExtents(ProjectLocation)
# map = openmap(map_extent$upperLeft, map_extent$lowerRight,zoom=map_extent$zoom, minNumTiles=9,type='opencyclemap',mergeTiles = T)
# 
# # Repoject the base layer in Lat/Long
# map<-openproj(map,projection = CRS('+proj=longlat +ellps=WGS84')) 
# 
# # Coordinents of edges of map from for object (North Arrow, Legend, Scale Bar) placement
# left<-map$bbox$p1[1]
# top<-map$bbox$p1[2]
# bottom<-map$bbox$p2[2]
# right<-map$bbox$p2[1]
# 
# distanceFactor<-1/(distHaversine(long=c(right, right), lat=c(bottom, top)))
# # Scale placement information (should not need to be changed)
# Scale_Loc<-list(lon=left+(right-left)*.025,lat=bottom+(top-bottom)*.025,distLon=round((right-left)*10.25,1),distLat=(top-bottom)*2.25)
# 
# # Use ggplots autoplot() function to plot these data
# Base_Map<-autoplot(map)+
#   geom_point(data=MappingTable, aes(x = Longitude, y = Latitude), size=4,colour='black', shape=18)+
#   coord_fixed(xlim = c(left,right), ylim = c(bottom,top), ratio = 1)+
#   labs(x = 'Longitude', y = 'Latitude\n', size=20)+
#   geom_text(data=MappingTable,position='dodge', aes(x = Longitude, y = Latitude, label =  SPID, fontface='bold'),  size = 4, vjust = 0, hjust = -0.3)+
#   theme_cmi()
# Base_Map
# path_Base_Map<-file.path(getwd(), 'R_output', 'Figures','Maps', paste0(ProjectLocation, '_Base_Map_AllYears_.jpg'))
# ggsave(Base_Map, filename=path_Base_Map, height=8, width=10, scale=2)


# Create new variables --------------------------------------------------------

# data_ALL$JulianDate<-julian(data_ALL$Date)

if (length(which(data_ALL$contract_year>data_ALL$year)) > 0) {
    data_ALL$DayN<-ifelse(data_ALL$DayOfYear>=yday(as.Date(paste0((data_ALL$contract_year-1),startdate))),
                          data_ALL$DayOfYear - yday(as.Date(paste0((data_ALL$contract_year-1),startdate))) + 1,
                          data_ALL$DayOfYear + (365+as.numeric(leap_year(data_ALL$contract_year-1))-yday(as.Date(paste0((data_ALL$contract_year-1),startdate)))) + 1)
} else if (length(which(data_ALL$contract_year<data_ALL$year)) > 0) { # TODO: this code might not work, test
    data_ALL$DayN<-ifelse(data_ALL$DayOfYear>=yday(as.Date(paste0((data_ALL$contract_year),startdate))),
                          data_ALL$DayOfYear - yday(as.Date(paste0((data_ALL$contract_year),startdate))) + 1,
                          data_ALL$DayOfYear + (365+as.numeric(leap_year(data_ALL$contract_year))-yday(as.Date(paste0((data_ALL$contract_year),startdate)))) + 1)
} else {
    data_ALL$DayN<-data_ALL$DayOfYear-min(data_ALL$DayOfYear)+1
}
data_ALL$week<-week(data_ALL$DateTime)
data_ALL$WeekN<-ceiling(data_ALL$DayN/7)

# Make time step
# Starts at midnight
data_ALL$TimeStep<-getTimeStep(data_ALL$DateTime, TimeStepOne)
data_ALL$TimeStepStart<-getTimeStepStart(data_ALL$DateTime, TimeStepOne)
data_ALL$TimeStepN<-data_ALL$TimeStep-min(data_ALL$TimeStep)+1

data_ALL$SunsetTimeStep<-getTimeStep(data_ALL$sunset_time, TimeStepOne)
data_ALL$SunsetTimeStepN<-data_ALL$SunsetTimeStep-min(data_ALL$TimeStep)+1

data_ALL$SunriseTimeStep<-getTimeStep(data_ALL$sunrise_time, TimeStepOne)
data_ALL$SunriseTimeStepN<-data_ALL$SunriseTimeStep-min(data_ALL$TimeStep)+1

# Change dates to POSIXct
a<-sapply(data_ALL, is.POSIXlt)
data_ALL[a]<-lapply(data_ALL[a], as.POSIXct)

# ROLL UP Bayesian Data -------------------------------------------------------

BayesianData<-data_ALL %>% 
    group_by(SPID, Island, Habitat, StrataName, Date, TimeStep, TimeStepN, TimeStepStart) %>% 
    summarise(minutes=length(unique(minute)),hits=sum(Total_hits),
            contract_year=unique(contract_year),
            year=mean(year),
            month=mean(month),week=unique(week),WeekN=unique(WeekN),
            day=unique(day),DayN=unique(DayN),DayOfYear=mean(DayOfYear),
            sunset_time=unique(sunset_time),SunsetTimeStep=mean(SunsetTimeStep),SunsetTimeStepN=mean(SunsetTimeStepN),
            sunrise_time=unique(sunrise_time),SunriseTimeStep=mean(SunriseTimeStep),SunriseTimeStepN=mean(SunriseTimeStepN),
            Illu=mean(Illu),
            flux_sensitive=mean(flux_sensitive),level_absolute=mean(level_absolute),click=mean(click,na.rm=T),burst=mean(burst,na.rm=T),
            Latitude=mean(Latitude),Longitude=mean(Longitude),
            Northing=mean(Northing),Easting=mean(Easting),
            # SiteType=unique(Site_Type),
            # Rover=unique(Rover),
            alarms=sum(Total_alarms)
            # Mic_Type=unique(Mic_Type)
  )
TimeStepOne=paste0(TimeStepOne,':00:00')

# Prepare count data -------------------------------------------
Counts_path=file.path(Dropbox,ProjectYear,ProjectName,'data',ProjectLocation,paste0(ProjectLocation,'_Counts_ALL.csv'))

Counts<-read.csv(Counts_path)
Counts<-select(Counts,SPID,Sensor_Name,contract_year,Island,Count_Date,Species,Density_Radius,Density)

# for now, take out counts with no corresponding acoustic site
Counts<-filter(Counts,!is.na(Sensor_Name))

if (str_detect(tolower(species),'chick')) {
  Counts<-filter(Counts,contract_year %in% c(2014,2016))
} else {
  Counts<-filter(Counts,contract_year %in% c(2014:2016))
}
Counts<-filter(Counts,contract_year>=YearStart & contract_year<=YearStop)

Counts<-left_join(Counts,select(SiteTypeRegion_Info,SPID,contract_year,Island,Habitat,StrataName),by=c('SPID','contract_year','Island'))

Counts<-filter(Counts,Species==gsub('_Chick','',species))
Counts[Counts$contract_year==2014 & Counts$SPID=='NW7',]$SPID<-'NW7 2014'

write.csv(Counts,file.path(Dropbox,ProjectYear,ProjectName,'data',ProjectLocation,paste0(ProjectLocation,'_',str_replace(species,'_Chick',''),'_Counts_',YearStart,'-',YearStop,'.csv')),row.names=F)

# Final check & write ---------------------------------------------------------

for(q in names(BayesianData)){
    if(sum(is.na(BayesianData[,q]))>0){
        print(q)
    }
}
head(BayesianData)
save(data_ALL,BayesianData,species,TimeStepOne,today,ProjectName,ProjectYear,ProjectLocation,file = file.path(Dropbox,ProjectYear,ProjectName,'data',ProjectLocation,paste0(ProjectLocation,'_',species,'_Data_2_10_',YearStart,'-',YearStop,'_',audit,'_ALL.RData')))
save(BayesianData,species,TimeStepOne,today,file = file.path(Dropbox,ProjectYear,ProjectName,'data',ProjectLocation,paste0(ProjectLocation,'_',species,'_Data_2_10_',YearStart,'-',YearStop,'_',audit,'.RData')))
