# BAYESIAN DATA CREATION AND PREPROCESSING
# BOPE

rm(list=ls())
gc()
options(stringsAsFactors=FALSE)

# Input Variables -------------------------------------------------------------

species='ANMU' # ANMU

ProjectName='Bayesian_2018'
ProjectYear=2018
ProjectLocation='Haida_Gwaii'

YearStart=2017
YearStop=2017

TimeStepOne = 17 # hour of first time step
startdate='-05-01' # set desired startdate of surveys
TimeZone='Canada/Pacific'

# Set up dropbox and packages ----------------------------------------------------------------

Dropbox<-'D:/CM,Inc/Dropbox (CMI)/CMI_Team/Analysis'
if(Sys.info()[4]=='MatthewsMac')  Dropbox<-'~/Dropbox' 
today<-format(Sys.Date(),'%d%b%y')

existing_Packages<-as.list(installed.packages()[,1])
required_Packages<-c('tidyverse','stringr','data.table','lubridate')
missing_Packages<- required_Packages[!required_Packages  %in% existing_Packages]
if(length(missing_Packages)>0)install.packages(pkgs =  missing_Packages)
invisible(lapply(required_Packages, require, character.only=T,quietly = T))

for(file in list.files(path = 'D:/CM,Inc/R-Code_improvements/Functions',pattern = '\\.[Rr]$',full.names = T,recursive = T)){
  source(file)
}

# Data Load -------------------------------------------------------------------

# Counts
Counts<-read.csv("D:/CM,Inc/Dropbox (CMI)/CMI_Team/Analysis/2017/EnvCan_BC_CAAU_ANMU_2017/Field_Data/DensityData_KERRYDONTLOOK/EnvCan_CMI_Density_20180103_WilsonL.csv")

# Acoustics
data_ALL<-readRDS("D:/CM,Inc/Dropbox (CMI)/CMI_Team/Analysis/2017/EnvCan_BC_CAAU_ANMU_2017/R_output/AnalysisData_EnvCan_BC_CAAU_ANMU_2017_ANMU08Mar18.Rda")

# Data cleanup and combine years ----------------------------------------------

# Check for duplicates
data_ALL$key<-paste(data_ALL$SPID, data_ALL$DateTime)
sum(duplicated(data_ALL$key))

head(data_ALL)

data_ALL$StrataName=data_ALL$Island
data_ALL$contract_year=data_ALL$year

# Create new variables --------------------------------------------------------

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
data_ALL$TimeStep<-getTimeStep(data_ALL$DateTime, starthour=TimeStepOne)
data_ALL$TimeStepStart<-getTimeStepStart(data_ALL$DateTime, TimeStepOne)
data_ALL$TimeStepN<-data_ALL$TimeStep-min(data_ALL$TimeStep)+1

data_ALL$SunsetTimeStep<-getTimeStep(data_ALL$sunset_time, TimeStepOne)
data_ALL$SunsetTimeStepN<-data_ALL$SunsetTimeStep-min(data_ALL$TimeStep)+1

data_ALL$SunriseTimeStep<-getTimeStep(data_ALL$sunrise_time, TimeStepOne)
data_ALL$SunriseTimeStepN<-data_ALL$SunriseTimeStep-min(data_ALL$TimeStep)+1

# ROLL UP Bayesian Data -------------------------------------------------------

BayesianData<-data_ALL %>% 
  group_by(SPID, StrataName, Island, Date, TimeStep, TimeStepN, TimeStepStart) %>% 
  summarise(minutes=length(unique(minute)),hits=as.integer(sum(Total_hits)),
            # contract_year=unique(contract_year),
            year=mean(year),
            month=mean(month),week=unique(week),WeekN=unique(WeekN),
            day=unique(day),DayN=unique(DayN),DayOfYear=mean(DayOfYear),
            sunset_time=unique(sunset_time),SunsetTimeStep=mean(SunsetTimeStep),SunsetTimeStepN=mean(SunsetTimeStepN),
            sunrise_time=unique(sunrise_time),SunriseTimeStep=mean(SunriseTimeStep),SunriseTimeStepN=mean(SunriseTimeStepN),
            Illu=mean(Illu),
            flux_sensitive=mean(flux_sensitive),level_absolute=mean(level_absolute),click=mean(click,na.rm=T),burst=mean(burst,na.rm=T),
            Latitude=mean(Latitude),Longitude=mean(Longitude)
            # Northing=mean(Northing),Easting=mean(Easting),
            # SiteType=unique(Site_Type),Rover=unique(Rover),
            # alarms=sum(Total_alarms)
            # Mic_Type=unique(Mic_Type)
  )

TimeStepOne=paste0(TimeStepOne,':00:00')
unique(BayesianData$year)

# Final check & write ---------------------------------------------------------

for(q in names(BayesianData)){
  if(sum(is.na(BayesianData[,q]))>0){
    print(q)
  }
}
head(BayesianData)

save(data_ALL,BayesianData,species,TimeStepOne,today,ProjectName,ProjectYear,ProjectLocation,file = file.path(Dropbox,ProjectYear,ProjectName,'data',ProjectLocation,paste0(ProjectLocation,'_',species,'_Data_2_10_',YearStart,'-',YearStop,'_ALL.RData')))
save(BayesianData,species,TimeStepOne,today,file = file.path(Dropbox,ProjectYear,ProjectName,'data',ProjectLocation,paste0(ProjectLocation,'_',species,'_Data_2_10_',YearStart,'-',YearStop,'.RData')))

# Prepare count data -------------------------------------------

Counts$year=2017
Counts$ # continue doing counts

# select and rename columns
Counts<-dplyr::select(Counts,SPID,year,mid_date,Transect_10M,Radius,Density)

Counts<-filter(Counts,contract_year>=YearStart & contract_year<=YearStop)
Counts<-filter(Counts,Species==species)

# add season information
Counts<-left_join(Counts,dplyr::select(CountsInfo,year,Round,Season),by=c('year','Round'))

setnames(Counts,'year','contract_year')
setnames(Counts,'Radius','Density_Radius')

# add location information
Counts<-left_join(Counts,dplyr::select(SiteTypeRegion_Info,SPID,contract_year,Island,Habitat,StrataName),by=c('SPID','contract_year'))

Counts$Count_Date=Counts$mid_date
Counts$Count_Date[Counts$mid_date=='3/8/2017']='2/28/2017'

for (CountType in c('T','10M')) {
  CountsFinal<-dplyr::select(Counts,-c(mid_date))
  CountsFinal<-filter(CountsFinal,Transect_10M==CountType) # only take transect densities ('T') or only take 5/10M circles (10M)
  write.csv(CountsFinal,file=paste0('D:/CM,Inc/Dropbox (CMI)/CMI_Team/Analysis/2018/Bayesian_2018/data/',ProjectLocation,'/',ProjectLocation,'_BOPE_Counts_',CountType,'_',YearStart,'-',YearStop,'.csv'),row.names=F)
}







