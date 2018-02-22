# Run_CallsCounts
# Set-up for estimating Calls vs Counts conversion function
# (allows estimating density from call rate)
# 
# NOTE: Assumed that this script is in same directory as Results files
rm(list = ls())
# USER SPECIFIED PARAMETERS -------------------------------------------------
# Set root directory path... enter absolute path or relative,
RootDir =  "above1"  # Examples "current" or "above1" or "C:/Users/XXXX/BayesianMethods"
AnalysisFolder = 'Acoustic2'  # Folder path within RootDir where analysis code is stored
ResultsFolder = 'CapCays/results'  # Folder path within RootDir where results files stored
DataFolder = 'CapCays/data'  # Folder path within RootDir where raw data files stored
RunFile = 'CallsCountsFxn2_10'       # Plotting Script
Species =  'WTSH'  # Species name for data analysis
# Specify results files to be included (one for each year):
Resultsfiles = c(paste0("Results_", Species,"_2014_2_10"),
                 paste0("Results_", Species,"_2015_2_10"),
                 paste0("Results_", Species,"_2016_2_10"))
# Specify name of Data file with Areas of each strata
Countsdatfile = c(paste0('Counts_CapCays_',Species,'_2014-2017.csv')) # Name of matching data file with nest count data (OTIONAL, enter blank if no nest counts)
Areasdatfile = c(paste0('QPWS_CapCays_Strata_Area.csv')) # Name of matching data file with nest count data (OTIONAL, enter blank if no nest counts)
Seasondefine = c(2,2,1,1,1,3,3,3,3,3,3,2)
Nchains = 20
Nburnin =  7000  # Number of burn-in reps 
Nadapt =  100  # Number of adapting reps, default 100
Totalreps = 10000 # Total desired reps (ie # simulations making up posterior)

# END USER SPECIFIED PARAMETERS ---------------------------------------------
#
# (Should not need to edit anything below here)
# 
# Process Filenames and directory names -------------------------------------
#
Nsim =  Totalreps/Nchains + Nburnin  # Total # MCMS sims: Actual saved reps = (Nsim-Nburnin) * (num Cores)

simsamp = 100;

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
thisdir = getwd()
if (RootDir=='current'){
  RootDir = getwd()
} else if(RootDir=='above1'){
  tmp1 = getwd()
  setwd('..')
  RootDir = getwd()
  setwd(tmp1)
  rm('tmp1')
} else if(RootDir=='above2'){
  tmp1 = getwd()
  setwd('..')
  setwd('..')
  RootDir = getwd()
  setwd(tmp1)
  rm('tmp1')
} else {
  RootDir = RootDir
}

AnalysisFolder = paste0(RootDir,"/",AnalysisFolder)
savename = paste0(Species, "_", RunFile, ".Rdata")
RunFile = paste0(AnalysisFolder,"/",RunFile,".r")
ResultsFolder = paste0(RootDir,"/",ResultsFolder)
loadfiles = paste0(ResultsFolder,"/",Resultsfiles,".rdata")
DataFolder = paste0(RootDir,"/",DataFolder)
loaddat = paste0(DataFolder,"/",Countsdatfile)
loadAdat = paste0(DataFolder,"/",Areasdatfile)
SaveResults = paste0(ResultsFolder,"/",savename)
source(RunFile)

#