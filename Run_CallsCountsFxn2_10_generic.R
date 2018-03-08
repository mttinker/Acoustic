# Run_CallsCounts
# Set-up for estimating Calls vs Counts conversion function
# (allows estimating density from call rate)
# 

library(stringr)

rm(list=ls())
gc()

# USER SPECIFIED PARAMETERS -------------------------------------------------
AnalysisFolder = 'D:/CM,Inc/Acoustic'
ResultsFolder = 'D:/CM,Inc/Dropbox (CMI)/CMI_Team/Analysis/2018/Bayesian_2018/results'
DataFolder = 'D:/CM,Inc/Dropbox (CMI)/CMI_Team/Analysis/2018/Bayesian_2018/data'
RunFile = 'CallsCountsFxn2_10'

diagnostic_plots=F

Species =  'WTSH'  # Species name for data analysis

# Midway, Kauai, CapCays
if (Species=='BOPE'|Species=='Aerial'|str_detect(Species,'Growl')) {
  ProjectLocation='Midway'
  StartYear = 2016
  StopYear = 2017
} else if (Species=='WTSH'|Species=='BLNO') {
  ProjectLocation='CapCays'
  StartYear = 2014
  StopYear = 2016
} else if (Species=='HAPE|NESH') {
  ProjectLocation='Kauai'
  StartYear = 2012
  StopYear = 2017
}

CountType = ''; IndexPeriod = ''
if (ProjectLocation=='Midway') {
  CountType = '_T' # Only fill in for Midway, 10M or T
} else if (ProjectLocation=='CapCays' & Species=='WTSH') {
  IndexPeriod = '_DecJan' # '' (blank) or '_DecJan'
  CountType = '_Occupied' # '_TotalBurrows' or '_Occupied'
}

Nchains = 8
Nburnin =  8000  # Number of burn-in reps Total reps = (Nsim-Nburnin) * (num Cores)
Nadapt =  100  # Number of adapting reps, default 100
Totalreps = 20000 # Total desired reps (ie # simulations making up posterior)

# END USER SPECIFIED PARAMETERS ---------------------------------------------
#
# (Should not need to edit anything below here)
# 
# Process Filenames and directory names -------------------------------------
#
Nsim =  Totalreps/Nchains + Nburnin  # Total # MCMS sims: Actual saved reps = (Nsim-Nburnin) * (num Cores)

simsamp = 100;

Resultsfiles = c()
for (i in StartYear:StopYear) {
    Resultsfiles = c(Resultsfiles,paste0(ProjectLocation, '_', Species, '_Results_2_10_', i, IndexPeriod))
}

if (ProjectLocation=='CapCays') {
  Countsdatfile = c(paste0(ProjectLocation,'_',Species,'_Counts',CountType,'_2014-2017.csv')) # Name of matching data file with nest count data (OTIONAL, enter blank if no nest counts)
  Areasdatfile = c(paste0('CapCays_StrataArea_ALL.csv')) # Name of matching data file with nest count data (OTIONAL, enter blank if no nest counts)
  Seasondefine = c(2,2,1,1,1,3,3,3,3,3,3,2)
} else if (ProjectLocation=='Midway') {
  Countsdatfile = c(paste0(ProjectLocation,'_BOPE_Counts',CountType,'_2016-2017.csv')) # Name of matching data file with nest count data (OTIONAL, enter blank if no nest counts)
  # Areasdatfile = c(paste0('CapCays_StrataArea_ALL.csv')) # Name of matching data file with nest count data (OTIONAL, enter blank if no nest counts)
  Seasondefine = c(1,2,3,3,3,3,3,3,3,3,3,3)
}

savename = paste0(ProjectLocation, '_', Species, '_CallsCountsFxn_2_10', CountType, IndexPeriod, ".Rdata")
RunFile = paste0(AnalysisFolder,"/",RunFile,".r")
loadfiles = paste0(ResultsFolder,"/",ProjectLocation,'/',Resultsfiles,".Rdata")
loaddat = paste0(DataFolder,"/",ProjectLocation,'/',Countsdatfile)
# loadAdat = paste0(DataFolder,"/",Areasdatfile)
SaveResults = paste0(ResultsFolder,"/",ProjectLocation,'/',savename)

source(RunFile)

#