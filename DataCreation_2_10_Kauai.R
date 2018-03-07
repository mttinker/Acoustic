# BAYESIAN DATA CREATION AND PREPROCESSING
# Kauai HAPE/NESH
# updated 07 Nov 2017

rm(list=ls())
gc()
options(stringsAsFactors=FALSE)

# Input Variables -------------------------------------------------------------

species='HAPE' # HAPE or NESH

ProjectName='Bayesian_2018'
ProjectYear=2018
ProjectLocation='Kauai'

YearStart=2012
YearStop=2017

TimeStepOne = 17 # hour of first time step

# Set up dropbox and packages -------------------------------------------------------------

Dropbox<-'D:/CM,Inc/Dropbox (CMI)/CMI_Team/Analysis'
if(Sys.info()[4]=='MatthewsMac')  Dropbox<-'~/Dropbox' 
today<-format(Sys.Date(),'%d%b%y')

existing_Packages<-as.list(installed.packages()[,1])
required_Packages<-c('tidyverse','stringr','data.table','lubridate')
missing_Packages<- required_Packages[!required_Packages  %in% existing_Packages]
if(length(missing_Packages)>0)install.packages(pkgs =  missing_Packages)
invisible(lapply(required_Packages, require, character.only=T,quietly = T))

for(file in list.files(path = 'D:/CM,Inc/R-Code_improvements/Functions/',pattern = '\\.[Rr]$',full.names = T,recursive = T)){
    source(file)
}

# Data Load -------------------------------------------------------------------

if (YearStart<=2017 & YearStop>=2017) {
  
  data_2017_HONO_path<-paste0(Dropbox,'/2017/KIUC_KESRP_HONO_2017/R_output/AnalysisData_KIUC_KESRP_HONO_2017_',species,'20Nov17.csv')
  data_2017_ULP_path<-paste0(Dropbox,'/2017/KIUC_KESRP_ULP_2017/R_output/AnalysisData_KIUC_KESRP_ULP_2017_',species,'14Nov17.csv')
  data_2017_Manoa_path<-paste0(Dropbox,'/2017/KIUC_KESRP_UManoa_2017/R_output/AnalysisData_KIUC_KESRP_UManoa_2017_',species,'15Nov17.csv')
  data_2017_Rovers_path<-paste0(Dropbox,'/2017/DOFAW_KESRP_Rovers_2017/R_output/AnalysisData_DOFAW_KESRP_Rovers_2017_',species,'28Nov17.csv') # KSHCP + Rovers + Lehua
  data_2017_HKPI_path<-paste0(Dropbox,'/2017/ABC_KESRP_hkpi_hono_2017/R_output/AnalysisData_ABC_KESRP_hkpi_hono_2017_',species,'06Dec17.csv')
  data_2017_Lanai_path<-paste0('D:/CM,Inc/Dropbox (CMI)/CMI_Team/Analysis/2017/USFWS_Lanai_2017/R_output/AnalysisData_USFWS_Lanai_2017_',species,'08Dec17.csv')

  # read in 2017 datas
  data_2017_HONO<-read.csv(data_2017_HONO_path)
  data_2017_ULP<-read.csv(data_2017_ULP_path)
  data_2017_Manoa<-read.csv(data_2017_Manoa_path)
  data_2017_Rovers<-read.csv(data_2017_Rovers_path) # KSHCP + Rovers + Lehua
  data_2017_HKPI<-read.csv(data_2017_HKPI_path)
  data_2017_Lanai<-read.csv(data_2017_Lanai_path)  
  
  # record sources of 2017 data
  data_2017_HONO$Source<-data_2017_HONO_path
  data_2017_ULP$Source<-data_2017_ULP_path
  data_2017_Manoa$Source<-data_2017_Manoa_path
  data_2017_Rovers$Source<-data_2017_Rovers_path
  data_2017_HKPI$Source<-data_2017_HKPI_path
  data_2017_Lanai$Source<-data_2017_Lanai_path
  
  data_2017_HONO$Round<-NA
  data_2017_HONO<-filter(data_2017_HONO,!str_detect(SPID,'-SM4'))
  
  data_2017<-bind_rows(data_2017_HONO,data_2017_ULP,data_2017_Manoa,data_2017_Rovers,data_2017_HKPI,data_2017_Lanai)
  rm(data_2017_HONO,data_2017_ULP,data_2017_Manoa,data_2017_Rovers,data_2017_HKPI,data_2017_Lanai)
  
}

if (YearStart<=2016 & YearStop>=2016) {
  
  data_2016_ULP_path<-paste0(Dropbox,'/2016/KIUC_KESRP_ULP_2016/R_output/AnalysisData_KIUC_KESRP_ULP_2016_',species,'19Dec16.csv')
  data_2016_HONO_path<-paste0(Dropbox,'/2016/KIUC_KESRP_HONO_2016/R_output/AnalysisData_KIUC_KESRP_HONO_2016_',species,'10Jan17.csv')
  data_2016_Manoa_path<-paste0(Dropbox,'/2016/KIUC_KESRP_UMANOA_2016/R_output/AnalysisData_KIUC_KESRP_UMANOA_2016_',species,'19Jan17.csv')
  data_2016_Napali_path<-paste0(Dropbox,'/2016/KESRP_Napali_2016/R_output/AnalysisData_KESRP_Napali_2016_',species,'04Jan17.csv')
  data_2016_HKPI_path<-paste0(Dropbox,'/2016/KESRP_ABC_hkpi_hnkoa_2016/R_output/AnalysisData_KESRP_ABC_hkpi_hnkoa_2016_',species,'04Dec17.csv')
  data_2016_ROV_path<-paste0(Dropbox,'/2016/KESRP_Rovers_ULP_2016/R_output/AnalysisData_KESRP_Rovers_ULP_2016_',species,'02Jan17.csv')
  
  data_2016_ULP<-read.csv(data_2016_ULP_path)
  data_2016_HONO<-read.csv(data_2016_HONO_path)
  data_2016_Manoa<-read.csv(data_2016_Manoa_path)
  data_2016_Napali<-read.csv(data_2016_Napali_path)
  data_2016_HKPI<-read.csv(data_2016_HKPI_path)
  data_2016_ROV<-read.csv(data_2016_ROV_path)
  
  data_2016_ULP$Source<-data_2016_ULP_path
  data_2016_HONO$Source<-data_2016_HONO_path
  data_2016_Manoa$Source<-data_2016_Manoa_path
  data_2016_Napali$Source<-data_2016_Napali_path
  data_2016_HKPI$Source<-data_2016_HKPI_path
  data_2016_ROV$Source<-data_2016_ROV_path
  
  data_2016_ULP$SuperRegion<-data_2016_ULP$Region
  data_2016_HONO$key<-NULL
  
  if (species=='HAPE') {
    
    # data_2016_Lanai_path<-"D:/CM,Inc/Dropbox (CMI)/CMI_Team/Analysis/2016/USFWS_Lanai_2016b/R_output/AnalysisData_USFWS_Lanai_2016b_HAPE22Mar17.Rda"
    # data_2016_Lanai<-readRDS(data_2016_Lanai_path)
    # data_2016_Lanai$Source<-data_2016_Lanai_path
    data_2016_Lanai_path<-"D:/CM,Inc/Dropbox (CMI)/CMI_Team/Analysis/2016/USFWS_Lanai_2016b/R_output/AnalysisData_USFWS_Lanai_2016b_HAPE22Mar17.csv"
    data_2016_Lanai<-read.csv(data_2016_Lanai_path)
    data_2016_Lanai$Source<-data_2016_Lanai_path
    
    data_2016<-bind_rows(data_2016_ROV, data_2016_ULP, data_2016_HONO, data_2016_Manoa, data_2016_Napali, data_2016_HKPI, data_2016_Lanai)
    rm(data_2016_ROV,data_2016_HKPI,data_2016_ULP,data_2016_HONO,data_2016_Napali,data_2016_Manoa, data_2016_Lanai)
    
  } else if (species=='NESH') {
    
    data_2016<-bind_rows(data_2016_ROV, data_2016_ULP, data_2016_HONO, data_2016_Manoa, data_2016_Napali, data_2016_HKPI)
    rm(data_2016_ROV,data_2016_HKPI,data_2016_ULP,data_2016_HONO,data_2016_Napali,data_2016_Manoa)
    
  }
  
}

if (YearStart<=2015 & YearStop>=2015) {
  
  data_2015_ULPHONO_path<-paste0(Dropbox,'/2015/KESRP_ULPHONO_2015/R_output/AnalysisData_KESRP_ULPHONO_2015_',species,'_ALL_30Nov17.csv')
  data_2015_Manoa_path<-paste0(Dropbox,'/2015/KESRP_UMANOA_2015/R_output/AnalysisData_KESRP_UMANOA_2015_',species,'23Nov15.csv')
  data_2015_FIWI_path<-paste0(Dropbox,'/2015/FIWI_KESRP_2015/R_output/AnalysisData_FIWI_KESRP_2015_',species,'_17Dec15.Rda')
  rundateLehua2015b = ifelse(species=='HAPE','21Jan16','22Jan16')
  data_2015_Lehua_path<-paste0(Dropbox,'/2015/IC_Lehua_2015b/R_output/AnalysisData_IC_Lehua_2015b_',species,'_ALL_',rundateLehua2015b,'.Rda')

  data_2015_ULPHONO<-read.csv(data_2015_ULPHONO_path)
  data_2015_Manoa<-read.csv(data_2015_Manoa_path)
  load(data_2015_FIWI_path)
  data_2015_FIWI<-AnalysisDataFull
  load(data_2015_Lehua_path)
  data_2015_Lehua<-AnalysisDataFull
  rm(AnalysisDataFull)
  
  data_2015_ULPHONO$Source<-data_2015_ULPHONO_path
  data_2015_Manoa$Source<-data_2015_Manoa_path
  data_2015_FIWI$Source<-data_2015_FIWI_path
  data_2015_Lehua$Source<-data_2015_Lehua_path
  
  if (species=='HAPE') {
    data_2015_Lanai_path<-"D:/CM,Inc/Dropbox (CMI)/CMI_Team/Analysis/2016/USFWS_Lanai_2015/R_output/AnalysisData_USFWS_Lanai_2015_HAPE_ALL_25Apr16.csv"
    data_2015_Lanai<-read.csv(data_2015_Lanai_path)
    data_2015_Lanai$Source<-data_2015_Lanai_path
  }
  
}

if (YearStart<=2014 & YearStop>=2014) {
  
  data_2014_ULPHONO_path<-paste0(Dropbox,'/2014/KESRP_ULPHONO_2014/R_output/AnalysisData_KESRP_ULPHONO_2014_',species,'05Jan15.csv')
  data_2014_FIWI_path<-paste0(Dropbox,'/2014/FIWI_2014/R_output/AnalysisData_FIWI_2014_Kauai_Lehua_',species,'_01Dec14.csv')
  data_2014_Lehua_path<-paste0(Dropbox,'/2015/IC_Lehua_2015/R_output/old/AnalysisData_IC_Lehua_2015_Lehua_',species,'04Feb15.csv')
  
  data_2014_ULPHONO<-read.csv(data_2014_ULPHONO_path)
  data_2014_FIWI<-read.csv(data_2014_FIWI_path)
  data_2014_Lehua<-read.csv(data_2014_Lehua_path)
  
  data_2014_ULPHONO$Source<-data_2014_ULPHONO_path
  data_2014_FIWI$Source<-data_2014_FIWI_path
  data_2014_Lehua$Source<-data_2014_Lehua_path
  
}

if (YearStart<=2013 & YearStop>=2013) {
  
  data_2013_ULPHONO_path<-paste0(Dropbox,'/2014/KESRP_ULPHONO_2013_reaudit/R_output/AnalysisData_KESRP_ULPHONO_2013_reaudit_',species,'19Jan17.csv')
  data_2013_Lehua_path<-paste0(Dropbox,'/2014/IC_LEHUA_2013/R_output/AnalysisData_IC_Lehua_2013_',species,'18Jun14.csv')
  
  data_2013_ULPHONO<-read.csv(data_2013_ULPHONO_path)
  data_2013_Lehua<-read.csv(data_2013_Lehua_path)
  
  data_2013_ULPHONO$Source<-data_2013_ULPHONO_path
  data_2013_Lehua$Source<-data_2013_Lehua_path
  
}

if (YearStart<=2012 & YearStop>=2012) {
  
  data_2012_path<-paste0(Dropbox,'/2014/KESRP_ULPHONO_2012_reaudit/R_output/AnalysisData_KESRP_ULPHONO_2012_reaudit_',species,'05Jan15.csv')
  
  data_2012<-read.csv(data_2012_path)
  
  data_2012$Source<-data_2012_path
  
}


# Data cleanup and combine years ----------------------------------------------

if (YearStart<=2017 & YearStop>=2017) {
  
  data_2017$SPID<-str_replace(data_2017$SPID,'HNKO2','HKOA2')

  data_2017$key<-NULL
  data_2017$Analyzed<-NULL
  
  data_2017$NESH_Density200<-NA
  data_2017$HAPE_Density200<-NA
  
  data_2017$Round<-as.character(data_2017$Round)
  
  data_2017<-fixDatesTimes(data_2017,TimeZone='HST')
  
  sapply(data_2017, function(x) sum(is.na(x)))
  
}

if (YearStart<=2016 & YearStop>=2016) {
  
  # fix SPIDs
  data_2016$SPID<-str_replace(data_2016$SPID, 'PIHEA', 'PIHE')
  data_2016$SPID[data_2016$SPID=='LANA1']='HII03'
  data_2016$SPID[data_2016$SPID=='LANA2']='NHAU01'
  data_2016$SPID[data_2016$SPID=='LANA3']='EHAU02'
  data_2016$SPID[data_2016$SPID=='LANA9-1']='KUNO01'
  data_2016$SPID[data_2016$SPID=='LANA5']='NHAU02'
  data_2016$SPID[data_2016$SPID=='LANA4']='HII01'
  data_2016$SPID[data_2016$SPID=='LANA7-1']='EHAU01'
  
  # remove comparison sensors placed at same locations as other sensors
  data_2016<-data_2016 %>% filter(!str_detect(SPID, 'CONS'))
  
  # 2016 DATA
  data_2016$NESH_Density200<-NA
  data_2016$HAPE_Density200<-NA
  
  # make all dates/times POSIXct and correct time zone
  data_2016<-fixDatesTimes(data_2016,TimeZone='HST')
  
  sapply(data_2016, function(x) sum(is.na(x)))
  
  data_2016<-subset(data_2016,!is.na(flux_sensitive))
  
  data_2016$Round<-as.character(data_2016$Round)
  
  data_2016$audit_file<-NA
  data_2016$probeId<-NA
  
}

if (YearStart<=2015 & YearStop>=2015) {
  
  data_2015_Manoa$Mic_Type<-NA
  
  a<-sapply(data_2015_FIWI, is.factor)
  data_2015_FIWI[a]<-lapply(data_2015_FIWI[a], as.character)
  a<-sapply(data_2015_Lehua, is.factor)
  data_2015_Lehua[a]<-lapply(data_2015_Lehua[a], as.character)
  a<-sapply(data_2015_Lehua, is.POSIXlt)
  data_2015_Lehua[a]<-lapply(data_2015_Lehua[a], as.character)
  data_2015_Lehua$Date<-as.character(data_2015_Lehua$Date)
  
  # add missing lat/longs (2015)
  MappingTable2015Lehua<-read.csv(paste0(Dropbox,'/2015/IC_Lehua_2015b/R_output/Tables/IC_Lehua_2015b_MappingTable_24Feb16.csv'))
  MappingTable2015Lehua<-select(MappingTable2015Lehua,SPID,Latitude,Longitude)
  data_2015_Lehua$Latitude<-NULL
  data_2015_Lehua$Longitude<-NULL
  data_2015_Lehua$SPID<-as.character(data_2015_Lehua$SPID)
  MappingTable2015Lehua$SPID<-str_replace(MappingTable2015Lehua$SPID,'LEHUA','LEHU')
  MappingTable2015Lehua$SPID<-str_replace(MappingTable2015Lehua$SPID, 'U0', 'U')
  data_2015_Lehua<-left_join(data_2015_Lehua,MappingTable2015Lehua,by='SPID')
  rm(MappingTable2015Lehua)
  
  data_2015_Lanai$SPID<-as.character(data_2015_Lanai$SPID)
  data_2015_Lanai$SPID[data_2015_Lanai$SPID=='Hii']='HII03'
  data_2015_Lanai$SPID[data_2015_Lanai$SPID=='Nhauola']='NHAU01'
  data_2015_Lanai$SPID[data_2015_Lanai$SPID=='Ehauola']='EHAU02'
  
  data_2015_Lanai$Round<-NA
  data_2015_Lanai$Mic_Type<-NA
  data_2015_Lanai$Site_Type<-NA
  data_2015_Lanai$Site_Description<-NA
  data_2015_Lanai$SuperRegion<-NA
  data_2015_Lanai$Region<-NA
  data_2015_Lanai$Island<-NA
  data_2015_Lanai$Easting<-NA
  data_2015_Lanai$Northing<-NA
  
  if (species=='HAPE') {
    data_2015<-bind_rows(data_2015_ULPHONO, data_2015_FIWI, data_2015_Lehua, data_2015_Manoa, data_2015_Lanai)
    rm(data_2015_FIWI,data_2015_Lehua,data_2015_Manoa,data_2015_ULPHONO, data_2015_Lanai)
  } else if (species=='NESH') {
    data_2015<-bind_rows(data_2015_ULPHONO, data_2015_FIWI, data_2015_Lehua, data_2015_Manoa)
    rm(data_2015_FIWI,data_2015_Lehua,data_2015_Manoa,data_2015_ULPHONO)
  }

  data_2015$SPID<-str_replace(data_2015$SPID, 'MANO', 'MANOA')
  data_2015<-data_2015[!is.na(data_2015$flux_sensitive),]
  
  # fix lehua sensor names
  data_2015$SPID<-str_replace(data_2015$SPID, 'LEHU','LEHU0')
  data_2015$SPID<-str_replace(data_2015$SPID, 'A0', '')
  data_2015$SPID<-str_replace(data_2015$SPID, '010', '10')
  
  # 2015 DATA
  # names(data_2016)[which(!names(data_2016) %in% names(data_2015))]

  data_2015<-fixDatesTimes(data_2015,TimeZone='HST')
  data_2015$MoonAltitude<-NA
  data_2015$NightOfYear<-ifelse(hour(data_2015$DateTime) %in% 12:23, yday(data_2015$Date), (yday(data_2015$Date)-1))
  data_2015$NESH_Density200<-NA
  data_2015$HAPE_Density200<-NA
  
  sapply(data_2015, function(x) sum(is.na(x)))
  
  data_2015$Round<-as.character(data_2015$Round)
  
  data_2015$audit_file<-NA
  data_2015$probeId<-NA
  
}

if (YearStart<=2014 & YearStop>=2014) {
  
  data_2014_FIWI$X.1<-NULL
  data_2014_FIWI$NESH_Density200<-NA
  data_2014_FIWI$HAPE_Density200<-NA
  data_2014_Lehua$NESH_Density200<-NA
  data_2014_Lehua$HAPE_Density200<-NA
  setnames(data_2014_ULPHONO, 'Survey_Point', 'SensorID')
  data_2014_FIWI$Region<-data_2014_FIWI$Site_Description
  data_2014_Lehua$Region<-data_2014_Lehua$Site_Description
  data_2014_FIWI$Site_Type<-NA
  data_2014_Lehua$Site_Type<-NA
  POHK3<-subset(data_2014_ULPHONO, SPID=='POHAKEA3')
  data_2014_ULPHONO<-subset(data_2014_ULPHONO, SPID!='POHAKEA3')
  POHK3$Latitude<-22.18613
  POHK3$Longitude<-(-159.603)
  data_2014_ULPHONO<-bind_rows(data_2014_ULPHONO,POHK3)
  data_2014<-bind_rows(data_2014_ULPHONO, data_2014_FIWI, data_2014_Lehua)
  data_2014<-select(data_2014,-X)
  rm(POHK3, data_2014_ULPHONO, data_2014_FIWI, data_2014_Lehua)
  
  setnames(data_2014, 'illu', 'Illu')
  
  data_2014$SPID<-str_replace(data_2014$SPID, 'PIHEA', 'PIHE')
  data_2014$SPID<-str_replace(data_2014$SPID, 'PIHE4R', 'PIHE4')
  data_2014$SPID<-str_replace(data_2014$SPID, 'PIHE3R', 'PIHE3')
  data_2014$SPID<-str_replace(data_2014$SPID, 'POHAKEA', 'POHK')
  
  # fix lehua sensor names
  data_2014$SPID<-str_replace(data_2014$SPID, 'LEHUA7', 'LEHU10')
  data_2014$SPID<-str_replace(data_2014$SPID, 'LEHUA10', 'LEHU08')
  data_2014$SPID<-str_replace(data_2014$SPID, 'LEHUA1', 'LEHU03')
  data_2014$SPID<-str_replace(data_2014$SPID, 'LEHUA5', 'LEHU05')
  data_2014$SPID<-str_replace(data_2014$SPID, 'LEHUA6', 'LEHU06')
  data_2014$SPID<-str_replace(data_2014$SPID, 'LEHUA4', 'LEHU04')
  data_2014$SPID<-str_replace(data_2014$SPID, 'LEHUA2', 'LEHU11')
  data_2014$SPID<-str_replace(data_2014$SPID, 'LEHUA3', 'LEHU12')
  data_2014$SPID<-str_replace(data_2014$SPID, 'LEHUA8', 'LEHU13')
  data_2014$SPID<-str_replace(data_2014$SPID, 'LEHUA9', 'LEHU14')
  
  # 2014 DATA
  names(data_2016)[which(!names(data_2016) %in% names(data_2014))]
  names(data_2014)[which(!names(data_2014) %in% names(data_2016))]
  setnames(data_2014, 'MinFrommoonset', 'MinFromMoonset')
  setnames(data_2014, 'MinFrommoonrise', 'MinFromMoonrise')
  data_2014$X<-NULL
  setnames(data_2014, 'SensorID', 'Sensor_Name')
  data_2014$species<-NULL
  setnames(data_2014, 'MinFromDawn', 'MinFromNautDawn')
  setnames(data_2014, 'MinFromDusk', 'MinFromNautDusk')
  data_2014$Elevation<-NA
  data_2014$Easting<-NA
  data_2014$Northing<-NA
  data_2014$Mic_Type<-NA
  data_2014$Round<-NA
  data_2014$MoonAltitude<-NA
  data_2014$civil_dawn<-NA
  data_2014$civil_dusk<-NA
  data_2014$MinFromCivilDawn<-NA
  data_2014$MinFromCivilDusk<-NA
  data_2014$astronomical_dawn<-NA
  data_2014$astronomical_dusk<-NA
  data_2014$MinFromAstroDawn<-NA
  data_2014$MinFromAstroDusk<-NA
  data_2014$SuperRegion<-NA
  
  data_2014<-fixDatesTimes(data_2014,TimeZone='HST')
  
  data_2014$NightOfYear<-ifelse(hour(data_2014$DateTime) %in% 12:23, yday(data_2014$Date), (yday(data_2014$Date)-1))
  data_2014$NESH_Density200<-NA
  data_2014$HAPE_Density200<-NA
  data_2014$Moonup<-NA
  
  sapply(data_2014, function(x) sum(is.na(x)))
  
  data_2014$Round<-as.character(data_2014$Round)
  
  data_2014$audit_file<-NA
  data_2014$probeId<-NA
  
}

if (YearStart<=2013 & YearStop>=2013) {
  
  data_2013_Lehua$NESH_Density200<-NA
  data_2013_Lehua$HAPE_Density200<-NA
  # setnames(data_2013_ULPHONO, 'Survey_Point', 'Sensor_Name')
  setnames(data_2013_Lehua, 'Sensor_NameID', 'Sensor_Name')
  data_2013_Lehua$Site_Description<-NA
  data_2013_ULPHONO$Northing<-NA
  data_2013_ULPHONO$Easting<-NA
  
  # add missing lat/longs (2013)
  MappingTable2013Lehua<-read.csv(paste0(Dropbox,'/2014/IC_LEHUA_2013/Field_Data/IC_Lehua_2013_Deployment_Info.csv'))
  MappingTable2013Lehua<-select(MappingTable2013Lehua,SPID,Latitude,Longitude)
  # data_2013_Lehua$SPID<-as.character(data_2013_Lehua$SPID)
  data_2013_Lehua<-left_join(data_2013_Lehua,MappingTable2013Lehua,by='SPID')
  rm(MappingTable2013Lehua)
  
  data_2013<-bind_rows(data_2013_ULPHONO,data_2013_Lehua)
  data_2013$SPID<-str_replace(data_2013$SPID, '-', '')
  rm(data_2013_ULPHONO,data_2013_Lehua)
  
  setnames(data_2013, 'illu', 'Illu')
  
  data_2013$SPID<-str_replace(data_2013$SPID, 'POHAKEA', 'POHK') 
  data_2013$SPID<-str_replace(data_2013$SPID, 'PIHEA', 'PIHE')
  
  data_2013$SPID<-str_replace(data_2013$SPID, 'Lehua2', 'LEHU08')
  data_2013$SPID<-str_replace(data_2013$SPID, 'Lehua1', 'LEHU01')
  data_2013$SPID<-str_replace(data_2013$SPID, 'Lehua3', 'LEHU02')
  data_2013$SPID<-str_replace(data_2013$SPID, 'Lehua4', 'LEHU15')
  data_2013$SPID<-str_replace(data_2013$SPID, 'Lehua5', 'LEHU16')
  data_2013$SPID<-str_replace(data_2013$SPID, 'Lehua6', 'LEHU17')
  # 2013 DATA
  names(data_2016)[which(!names(data_2016) %in% names(data_2013))]
  names(data_2013)[which(!names(data_2013) %in% names(data_2016))]
  data_2013$X<-NULL
  setnames(data_2013, 'MinFrommoonset', 'MinFromMoonset')
  setnames(data_2013, 'MinFrommoonrise', 'MinFromMoonrise')
  setnames(data_2013, 'MinFromDawn', 'MinFromNautDawn')
  setnames(data_2013, 'MinFromDusk', 'MinFromNautDusk')
  data_2013$species<-NULL
  data_2013$Elevation<-NA
  data_2013$Mic_Type<-NA
  data_2013$Round<-NA
  data_2013$MoonAltitude<-NA
  data_2013$civil_dawn<-NA
  data_2013$civil_dusk<-NA
  data_2013$MinFromCivilDawn<-NA
  data_2013$MinFromCivilDusk<-NA
  data_2013$astronomical_dawn<-NA
  data_2013$astronomical_dusk<-NA
  data_2013$MinFromAstroDawn<-NA
  data_2013$MinFromAstroDusk<-NA
  data_2013$SuperRegion<-NA
  
  data_2013<-fixDatesTimes(data_2013,TimeZone='HST')
  
  data_2013$NightOfYear<-ifelse(hour(data_2013$DateTime) %in% 12:23, yday(data_2013$Date), (yday(data_2013$Date)-1))
  data_2013$Moonup<-NA
  
  sapply(data_2013, function(x) sum(is.na(x)))
  
  data_2013$Round<-as.character(data_2013$Round)
  
  data_2013$audit_file<-NA
  data_2013$probeId<-NA
  
}

if (YearStart<=2012 & YearStop>=2012) {
  
  setnames(data_2012, 'illu', 'Illu')
  
  data_2012<-data_2012[!is.na(data_2012$flux_sensitive),]
  data_2012$SPID<-str_replace(data_2012$SPID, 'R10', 'R6')
  data_2012$SPID<-str_replace(data_2012$SPID, 'R13', 'R9')
  
  # 2012 DATA
  names(data_2016)[which(!names(data_2016) %in% names(data_2012))]
  names(data_2012)[which(!names(data_2012) %in% names(data_2016))]
  data_2012$X<-NULL
  setnames(data_2012, 'Survey_Point', 'Sensor_Name')
  data_2012$species<-NULL
  setnames(data_2012, 'MinFrommoonset', 'MinFromMoonset')
  setnames(data_2012, 'MinFrommoonrise', 'MinFromMoonrise')
  setnames(data_2012, 'MinFromDawn', 'MinFromNautDawn')
  setnames(data_2012, 'MinFromDusk', 'MinFromNautDusk')
  data_2012$Easting<-NA
  data_2012$Northing<-NA
  data_2012$Elevation<-NA
  data_2012$Mic_Type<-NA
  data_2012$Round<-NA
  data_2012$MoonAltitude<-NA
  data_2012$civil_dawn<-NA
  data_2012$civil_dusk<-NA
  data_2012$MinFromCivilDawn<-NA
  data_2012$MinFromCivilDusk<-NA
  data_2012$astronomical_dawn<-NA
  data_2012$astronomical_dusk<-NA
  data_2012$MinFromAstroDawn<-NA
  data_2012$MinFromAstroDusk<-NA
  data_2012$SuperRegion<-NA
  
  data_2012<-fixDatesTimes(data_2012,TimeZone='HST')
  
  data_2012$NightOfYear<-ifelse(hour(data_2012$DateTime) %in% 12:23, yday(data_2012$Date), (yday(data_2012$Date)-1))
  data_2012$Moonup<-NA
  
  sapply(data_2012, function(x) sum(is.na(x)))
  
  data_2012$Round<-as.character(data_2012$Round)
  
  data_2012$audit_file<-NA
  data_2012$probeId<-NA
  
}

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
if (exists('data_2013')) {
  if (exists('data_ALL')) { data_ALL<-bind_rows(data_ALL,data_2013) } else { data_ALL<-data_2013 }
  rm(data_2013)
}
if (exists('data_2012')) {
  if (exists('data_ALL')) { data_ALL<-bind_rows(data_ALL,data_2012) } else { data_ALL<-data_2012 }
  rm(data_2012)
}

sapply(data_ALL, function(x) sum(is.na(x)))

data_ALL<-data_ALL[!is.na(data_ALL$flux_sensitive),]
data_ALL<-data_ALL[!is.na(data_ALL$level_absolute),]
data_ALL<-data_ALL[!is.infinite(data_ALL$level_absolute),]
data_ALL$Mic_Type<-NULL

# Check for duplicates
data_ALL$key<-paste(data_ALL$SPID, data_ALL$DateTime, data_ALL$file)
sum(duplicated(data_ALL$key))   # there are duplicates, but all variables are the 
                                # same so just remove one (code for this in 
                                # Data_Creation_Troubleshoot_17Jan17.R)
data_ALL<-subset(data_ALL,!duplicated(data_ALL$key))

names(data_ALL)
data_ALL<-select(data_ALL,-c(Island))

# bring in site type/region
SiteTypeRegion_Info<-read.csv(file.path(Dropbox,ProjectYear,ProjectName,'data',ProjectLocation,paste0(ProjectLocation,'_SiteInfo_ALL.csv')))
data_ALL$Region<-NULL
data_ALL$SuperRegion<-NULL
data_ALL$Site_Type<-NULL
data_ALL$Latitude<-NULL
data_ALL$Longitude<-NULL
data_ALL$Elevation<-NULL
data_ALL<-left_join(data_ALL, SiteTypeRegion_Info, by=c('SPID','year'))

# FIX moon & sunrise/sunset info ----------------------------------------------

# TODO: split data by island and use different almanacs
unique(data_ALL$Island)

# # OR BRING IN ALREADY MADE ALMANAC
Almanac<-readRDS(file.path(Dropbox,'Almanac_2012-2017_Kauai.Rda'))
# head(Almanac)

data_ALL<-select(data_ALL, -c(sunrise_time,sunset_time,moonrise_time,moonset_time,
                                              civil_dusk,civil_dawn,naut_dusk,naut_dawn,
                                              astronomical_dusk,astronomical_dawn,Illu))

data_ALL$NightOfYear<-ifelse(hour(data_ALL$DateTime)%in%12:23,yday(data_ALL$Date),(yday(data_ALL$Date)-1))
data_ALL$SRiseNight<-ifelse(hour(data_ALL$DateTime)%in%0:11,yday(data_ALL$Date),(yday(data_ALL$Date)+1))
data_ALL$NightDate<-as.Date(data_ALL$NightOfYear, origin=paste(data_ALL$year,'-01-01',sep=''))- days(1)
data_ALL$SRiseNightDate<-as.Date(data_ALL$SRiseNight, origin=paste(data_ALL$year,'-01-01',sep=''))- days(1)

# Remove a few columns before you merge so that you do not have duplicate names
Almanac<-Almanac[,!(names(Almanac) %in%  c('Latitude','Longitude','Location'))]

Almanac$Date<-as.Date(Almanac$Date)
AlmanacRise<-select(Almanac,Date,sunrise_time,moonrise_time,civil_dawn,naut_dawn,astronomical_dawn,Illu)
AlmanacSet<-select(Almanac,-c(sunrise_time,moonrise_time,civil_dawn,naut_dawn,astronomical_dawn,Illu))
# Merge almanac with Combined data
data_ALL<-left_join(data_ALL,AlmanacRise,by=c('SRiseNightDate' = 'Date'))
data_ALL<-left_join(data_ALL,AlmanacSet,by=c('NightDate' = 'Date'))

# almanac workaround
# data_ALL<-left_join(data_ALL,Almanac,by='NightOfYear')

# Calculate diff times for all sun/moon
data_ALL$MinFromSunset<-as.numeric(difftime((data_ALL$DateTime),(data_ALL$sunset_time), units='mins'))
data_ALL$MinFromSunrise<-as.numeric(difftime((data_ALL$DateTime),(data_ALL$sunrise_time), units='mins'))
data_ALL$MinFromMoonset<-as.numeric(difftime((data_ALL$DateTime),(data_ALL$moonset_time), units='mins'))
data_ALL$MinFromMoonrise<-as.numeric(difftime((data_ALL$DateTime),(data_ALL$moonrise_time), units='mins'))
data_ALL$MinFromCivilDawn<-as.numeric(difftime((data_ALL$DateTime),(data_ALL$civil_dawn), units='mins'))
data_ALL$MinFromCivilDusk<-as.numeric(difftime((data_ALL$DateTime),(data_ALL$civil_dusk), units='mins'))
data_ALL$MinFromNautDawn<-as.numeric(difftime((data_ALL$DateTime),(data_ALL$naut_dawn), units='mins'))
data_ALL$MinFromNautDusk<-as.numeric(difftime((data_ALL$DateTime),(data_ALL$naut_dusk), units='mins'))
data_ALL$MinFromAstroDawn<-as.numeric(difftime((data_ALL$DateTime),(data_ALL$astronomical_dawn), units='mins'))
data_ALL$MinFromAstroDusk<-as.numeric(difftime((data_ALL$DateTime),(data_ALL$astronomical_dusk), units='mins'))

# generate moon data and use moon altitude to determine if the moon was above the horizon
moonInfo<-moonAngle(with_tz(data_ALL$DateTime,tzone = 'UTC'),
                    longitude =data_ALL$Longitude,
                    latitude = data_ALL$Latitude)

moonInfo<-bind_rows(moonInfo)
data_ALL$Moonup<-ifelse(moonInfo$altitude>0,1,0) 
data_ALL$MoonAltitude<-moonInfo$altitude
data_ALL$Illu<-moonInfo$illuminatedFraction
rm(moonInfo)

a<-sapply(data_ALL, function(x) sum(is.na(x)))
a
data_ALL<-select(data_ALL, -c(SRiseNight,NightDate,SRiseNightDate))
data_ALL<-select(data_ALL,-X)

rm(Almanac, AlmanacRise, AlmanacSet)

# Create new variables --------------------------------------------------------

# data_ALL$JulianDate<-julian(data_ALL$Date)

data_ALL$DayN<-data_ALL$DayOfYear-min(data_ALL$DayOfYear)+1
data_ALL$week<-week(data_ALL$DateTime)
data_ALL$WeekN<-ceiling(data_ALL$DayN/7)

# Make time step
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
    group_by(SPID, StrataName, Island, Date, TimeStep, TimeStepN, TimeStepStart) %>% 
    summarise(minutes=length(unique(minute)),hits=sum(Total_hits),
              # contract_year=unique(contract_year),
              year=mean(year),
              month=mean(month),week=unique(week),WeekN=unique(WeekN),
              day=unique(day),DayN=unique(DayN),DayOfYear=mean(DayOfYear),
              sunset_time=unique(sunset_time),SunsetTimeStep=mean(SunsetTimeStep),SunsetTimeStepN=mean(SunsetTimeStepN),
              sunrise_time=unique(sunrise_time),SunriseTimeStep=mean(SunriseTimeStep),SunriseTimeStepN=mean(SunriseTimeStepN),
              Illu=mean(Illu),
              flux_sensitive=mean(flux_sensitive),level_absolute=mean(level_absolute),click=mean(click,na.rm=T),burst=mean(burst,na.rm=T),
              Latitude=mean(Latitude),Longitude=mean(Longitude),
              Northing=mean(Northing),Easting=mean(Easting),
              SiteType=unique(Site_Type),Rover=unique(Rover),
              alarms=sum(Total_alarms)
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

