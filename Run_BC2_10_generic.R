# RunBC_EDIT_THIS_PART = User File to Run "BayesCalls" Program to analyze Acoustic call data 
#    using a Bayesian State Space model (BayesCalls)
#    (***filename needs to begin with "RunBC_")
# Purpose of this file is for user to specify parameters needed to run analysis
# NOTE: Results file will get same name as this script, with "RunBC" replaced by "Results"

rm(list = ls())
gc()

existing_Packages<-as.list(installed.packages()[,1])
required_Packages<-c("stringr",'beepr')
missing_Packages<- required_Packages[!required_Packages%in% existing_Packages]
if(length(missing_Packages)>0)install.packages(pkgs =  missing_Packages)
invisible(lapply(required_Packages, require, character.only=T,quietly = T))

start.time <- Sys.time()  # start a timer

# USER SPECIFIED PARAMETERS -------------------------------------------------
#
Species =  'HAPE'  # Species name for data analysis (HAPE,NESH,WTSH,BLNO,BOPE:Aerial,Growl_old,Growl_new)
Yearfocal =  2017  # Focal year for Bayesian analysis
subsamp =  3  # Level of Sub-sampling of entire data set: use every nth record
data_opt =  1  # Data Option: 1 = Calls Only, 2 = Calls plus Nest Counts
QC_opt = 1   # QC option: 0 = filter/do not adjust for QC, 1 = adjust call rate w. fitted QC fxn
prior_opt = 1   # Priors: 1 = uninformed, 2 = informed (must supply results file)

AnalysisFolder = '/home/cmi/hape'  # Folder path within RootDir where analysis code is stored
RunFile = 'BayesCalls2_10'       # Version of BayesCalls to run
DataFolder = '/home/cmi/hape'  # Folder path within RootDir where raw data files stored
ResultsFolder = '/home/cmi/hape'  # Folder path within RootDir where results files stored

Nchains = 8
Nburnin =  2750  # Number of burn-in reps Total reps = (Nsim-Nburnin) * (num Cores)
Nadapt =  100  # Number of adapting reps, default 100
Totalreps = 10000 # Total desired reps (ie # simulations making up posterior)

ProjectName =  'Bayesian_2018'  # Name of project (for user reference)
ProjectYear =  2018  # Year of analysis project

if (Species=='WTSH'|Species=='BLNO') {
  ProjectLocation = 'CapCays'
  Datafile =  paste0(ProjectLocation,'_',Species,'_Data_2_10_2014-2017_NoAudit.RData')  #Name of data file for analysis 
  calendar_opt =  2  # year range: 1 = All data within calendar year, 2 = data spans New Year
  
  if (data_opt==2) {
    Countsdatfile = c(paste0(ProjectLocation,'_',species,'_Counts_',CountType,'_2014-2017.csv'))
  }
  
  if (Yearfocal==2017) {
    # Analysis period
    DayOfYear_strt =  275  # Oct 1 2016
    DayOfYear_end =  152  # Jun 1 2017
  } else if (Yearfocal==2016) {
    # Analysis period
    DayOfYear_strt =  274  # Oct 1 2015
    DayOfYear_end =  153  # Jun 1 2016
  } else {
    # Analysis period
    DayOfYear_strt =  274  # Oct 1
    DayOfYear_end =  152  # Jun 1
  }
  
  # set peak dates (DOY not day of survey)
  if (Species=='WTSH') {
    
    # acoustic peak period
    # peakdates_strt = 50
    # if (Yearfocal==2016) { peakdates_end = 109 } else { peakdates_end = 109 }
    # calendar_pk_opt = 1 # 1 = all in one year, 2 = peak spans new year
    
    # peak period = egg laying
    if (Yearfocal==2017) { peakdates_strt = 345 } else { peakdates_strt = 344 } # Dec 10
    if (Yearfocal==2016) { peakdates_end = 121 } else { peakdates_end = 120 } # April 30
    calendar_pk_opt = 2 # 1 = all in one year, 2 = peak spans new year
    
  } else if (Species=='BLNO') {
    # use wide "index" period to capture any peak
    if (Yearfocal==2017) { peakdates_strt = 350 } else { peakdates_strt = 349 } # Dec 15
    peakdates_end = 31 # Jan 31
    calendar_pk_opt = 2 # 1 = all in one year, 2 = peak spans new year
  }
  
  if (Species=='WTSH') {
    peaktimes_strt =  -120  # Peak time boundary 1, minutes relative to a reference event (sunrise or sunset)
    peaktimes_stop =  -60  # Peak time boundary 2, always > than boundary1 (minutes relative to event) 
    peaktimes_ref =  2  # Reference event: 1 = after sunset, 2 = before sunrise, 3 = sunrise AND sunset 
  } else if (Species=='BLNO') {
    # use two hours to capture any peak
    peaktimes_strt =  -60  # Peak time boundary 1, minutes relative to a reference event (sunrise or sunset)
    peaktimes_stop =  60  # Peak time boundary 2, always > than boundary1 (minutes relative to event) 
    peaktimes_ref =  2  # Reference event: 1 = after sunset, 2 = before sunrise, 3 = sunrise AND sunset 
  }
  
  if (prior_opt==2) {
    PriorResultsFile =  c()  # OPTIONAL: if prior_opt = 2, Rdata file containing parameter priors
  }
  
} else if (Species=='HAPE'|Species=='NESH') {
  ProjectLocation = 'Kauai'
  Datafile =  paste0(ProjectLocation,'_',Species,'_Data_2_10_2012-2017.RData')  #Name of data file for analysis 
  calendar_opt =  1  # year range: 1 = All data within calendar year, 2 = data spans New Year
  calendar_pk_opt = 1 # 1 = all in one year, 2 = peak spans new year
  
  if (prior_opt==2) {
    PriorResultsFile =  c()  # OPTIONAL: if prior_opt = 2, Rdata file containing parameter priors
  }
  
  if (Yearfocal==2016 | Yearfocal==2012) {
    # Analysis period
    DayOfYear_strt =  92  # Apr 1
    DayOfYear_end =  274  # Sept 30
  } else {
    # Analysis period
    DayOfYear_strt =  91  # Apr 1
    DayOfYear_end =  273  # Sept 30
  }
  
  if (Yearfocal==2017) {
    peakdates_strt = 116 # Apr 26
    peakdates_end = 233 # Aug 21
  } else if (Yearfocal==2016) {
    # Peak period = "comparison period" from each year
    peakdates_strt = 127 # May 6
    peakdates_end = 245 # Sept 1
  } else if (Yearfocal==2015) {
    peakdates_strt = 124 # May 4
    peakdates_end = 241 # Aug 29
  } else if (Yearfocal==2014) {
    peakdates_strt = 120 # Apr 30
    peakdates_end = 237 # Aug 25
  } else if (Yearfocal==2013) {
    peakdates_strt = 115 # Apr 25
    peakdates_end = 233 # Aug 21
  } else if (Yearfocal==2012) {
    peakdates_strt = 127 # May 6
    peakdates_end = 244 # Aug 31
  }
  
  if (Species=='HAPE') {
    peaktimes_strt =  40  # Peak time boundary 1, minutes relative to a reference event (sunrise or sunset)
    peaktimes_stop =  100  # Peak time boundary 2, always > than boundary1 (minutes relative to event) 
    peaktimes_ref =  1  # Reference event: 1 = after sunset, 2 = before sunrise, 3 = sunrise AND sunset 
  } else if (Species=='NESH') {
    peaktimes_strt =  -90  # Peak time boundary 1, minutes relative to a reference event (sunrise or sunset)
    peaktimes_stop =  -30  # Peak time boundary 2, always > than boundary1 (minutes relative to event) 
    peaktimes_ref =  2  # Reference event: 1 = after sunset, 2 = before sunrise, 3 = sunrise AND sunset 
  }
  
} else if (Species=='BOPE'|Species=='Aerial'|str_detect(Species,'Growl')) {
  
  ProjectLocation = 'Midway'
  Datafile =  paste0(ProjectLocation,'_',Species,'_Data_2_10_2016-2017.RData')  #Name of data file for analysis 
  calendar_opt =  2  # year range: 1 = All data within calendar year, 2 = data spans New Year
  
  if (data_opt==2) {
    CountType = 'T' # T=transect, 10M=5m and 10m circles
    Countsdatfile = c(paste0(ProjectLocation,'_BOPE_Counts_',CountType,'_2016-2017.csv'))
  }
  
  if (Yearfocal==2017) {
    # Analysis period
    DayOfYear_strt =  335  # Nov 30 2016
    DayOfYear_end =  191  # Jul 10 2017
  } else if (Yearfocal==2016) {
    DayOfYear_strt =  334  # Nov 30 2015
    DayOfYear_end =  192  # Jul 10 2016
  }
  
  # set peak dates (DOY not day of survey)
  peakdates_strt = 32 # Feb 1
  if (Yearfocal==2016) { peakdates_end = 91 } else { peakdates_end = 90 } # Mar 31
  calendar_pk_opt = 1 # 1 = all in one year, 2 = peak spans new year
  peaktimes_strt =  50  # Peak time boundary 1, minutes relative to a reference event (sunrise or sunset)
  peaktimes_stop =  110  # Peak time boundary 2, always > than boundary1 (minutes relative to event) 
  peaktimes_ref =  1  # Reference event: 1 = after sunset, 2 = before sunrise, 3 = sunrise AND sunset 
  
  if (prior_opt==2) {
    PriorResultsFile =  c()  # OPTIONAL: if prior_opt = 2, Rdata file containing parameter priors
  }
  
}

TS_strt =  1  # Starting time step (beginning of range to load data)
TS_stop =  57  # Ending timestep (end of range to load data)
TimeStep1 =  '5:00PM'  # Clock time (24hr) of timestep1
TimeStepIntvl =  15  # Number of minutes in each timestep

# END USER SPECIFIED PARAMETERS ---------------------------------------------
#
# (Should not need to edit anything below here)
# 
Nsim =  Totalreps/Nchains + Nburnin  # Total # MCMS sims: Actual saved reps = (Nsim-Nburnin) * (num Cores)
# Process Filenames and directory names -------------------------------------
#
RunFile = paste0(AnalysisFolder,"/",RunFile,".R")
loadfile1 = paste0(DataFolder,"/",Datafile)
if (data_opt>1){
  loadfile2 =  paste0(DataFolder,"/",Countsdatfile)
}
BayesModelVersion = str_replace(str_replace(basename(RunFile),'BayesCalls',''),'.R','')
SaveResults = paste0(ResultsFolder,'/', ProjectLocation, '_', Species, '_Results_',BayesModelVersion,'_',Yearfocal,".Rdata")

#
# Run Bayesian Analysis -----------------------------------------------------
# Source BayesCalls program (specify version as X_XX, e.g. BayesCalls2_01) 
#
source(RunFile)

Totalend.time <- Sys.time()
Totaltime.taken <- start.time - Totalend.time
Totaltime.taken

beep('mario')

