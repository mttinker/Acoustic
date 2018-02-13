# Run_CallsCounts
# Set-up for estimating Calls vs Counts conversion function
# (allows estimating density from call rate)
# 

rm(list=ls())
gc()

# USER SPECIFIED PARAMETERS -------------------------------------------------
AnalysisFolder = 'D:/CM,Inc/Acoustic'
ResultsFolder = 'D:/CM,Inc/Dropbox (CMI)/CMI_Team/Analysis/2018/Bayesian_2018/results'
DataFolder = 'D:/CM,Inc/Dropbox (CMI)/CMI_Team/Analysis/2018/Bayesian_2018/data'
RunFile = 'CallsCountsFxn2_10'

diagnostic_plots=T

Species =  'BOPE'  # Species name for data analysis
ProjectLocation = 'Midway' # Midway, Kauai, CapCays
if (ProjectLocation=='Midway') {
  StrataType = 'Habitat' # Habitat or Flyover
  CountType = '10M' # Only fill in for Midway, 10M or T
}
StartYear = 2016
StopYear = 2017

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
  if (ProjectLocation=='Midway') {
    Resultsfiles = c(Resultsfiles,paste0('Results_',Species,'_',i,'_2_10_',StrataType))
  } else {
    Resultsfiles = c(Resultsfiles,paste0('Results_',Species,'_',i,'_2_10'))
  }
}

if (ProjectLocation=='CapCays') {
  Countsdatfile = c(paste0(ProjectLocation,'_Counts_',Species,'_2014-2017.csv')) # Name of matching data file with nest count data (OTIONAL, enter blank if no nest counts)
  Areasdatfile = c(paste0('CapCays_StrataArea_ALL.csv')) # Name of matching data file with nest count data (OTIONAL, enter blank if no nest counts)
  Seasondefine = c(2,2,1,1,1,3,3,3,3,3,3,2)
} else if (ProjectLocation=='Midway') {
  Countsdatfile = c(paste0(ProjectLocation,'_Counts_',CountType,'_',Species,'_2016-2017.csv')) # Name of matching data file with nest count data (OTIONAL, enter blank if no nest counts)
  # Areasdatfile = c(paste0('CapCays_StrataArea_ALL.csv')) # Name of matching data file with nest count data (OTIONAL, enter blank if no nest counts)
  Seasondefine = c(1,2,3,4,4,4,4,4,4,4,4,4)
}


savename = paste0(Species, "_", RunFile, ".Rdata")
RunFile = paste0(AnalysisFolder,"/",RunFile,".r")
loadfiles = paste0(ResultsFolder,"/",Resultsfiles,".Rdata")
loaddat = paste0(DataFolder,"/",Countsdatfile)
# loadAdat = paste0(DataFolder,"/",Areasdatfile)
SaveResults = paste0(ResultsFolder,"/",savename)

source(RunFile)

#