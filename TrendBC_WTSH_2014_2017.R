# Set-up for doing trend analysis from BayesCalls analysis
# Purpose of this file is for user to specify parameters for doing trend analysis
#
rm(list = ls())
# USER SPECIFIED PARAMETERS -------------------------------------------------
# Set root directory path... enter absolute path or relative,
RootDir =  "above1"  # Examples "current" or "above1" or "C:/Users/XXXX/BayesianMethods"
AnalysisFolder = 'Acoustic2'  # Folder path within RootDir where analysis code is stored
ResultsFolder = 'CapCays/results'  # Folder path within RootDir where results files stored
RunFile = 'BayesCallsTrends2_10'       # Script to run
Species =  'WTSH'  # Species name for data analysis
# Specify results files to be included (one for each year):
Resultsfiles = c(paste0("Results_", Species,"_2014_2_10"),
                 paste0("Results_", Species,"_2015_2_10"),
                 paste0("Results_", Species,"_2016_2_10"),
                 paste0("Results_", Species,"_2017_2_10"))
# Specify focal strata for estimating trends: 
#   NOTE: all named strata must be present in each years data!!
#   ALSO: If Trendtype = 3 then specify Strata for whole Island, or whole Region
Strata_trends = c('North West Pisonia','North West Fringe','North West Rock')
# Can either estimate density for all sites each year (FocalSites = 0)
#  OR for a select sequence of sites present each year (FocalSites = 1)
FocalSites = 0
# If FocalSites = 1, specify sites within focal strata for estimating trends: 
#  NOTE: these must be sites in above-named strata and present in each years data!!
Sites_trends = c('')
# Folder path within RootDir where raw data files stored
DataFolder = 'CapCays/data'  
# Specify name of Data files with Conversion function and Areas of each strata
Convertfile = 'WTSH_CallsCountsFxn2_10' # Results file with conversion function
# Name of matching data file with nest count data (OTIONAL, enter blank if no nest counts)
Areasdatfile = 'QPWS_CapCays_Strata_Area.csv' 
# Trendtype option: If type = 1, calculate trend for call rate, if 2 calculate for density,
#  if 3 calculate for "TOTAL" abundance, either whole-Island or whole-region
#   (assumes ALL strata sampled each year)
Trendtype = 3 
#
# Params for JAGS
Nchains = 20
Nburnin =  5000  # Number of burn-in reps Total reps = (Nsim-Nburnin) * (num Cores)
Nadapt =  100  # Number of adapting reps, default 100
Totalreps = 5000 # Total desired reps (ie # simulations making up posterior)

# END USER SPECIFIED PARAMETERS ---------------------------------------------
#
# (Should not need to edit anything below here)
# 
# Process Filenames and directory names -------------------------------------
#
Nsim =  Totalreps/Nchains + Nburnin  # Total # MCMS sims: Actual saved reps = (Nsim-Nburnin) * (num Cores)

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
RunFile = paste0(AnalysisFolder,"/",RunFile,".r")
ResultsFolder = paste0(RootDir,"/",ResultsFolder)
loadfiles = paste0(ResultsFolder,"/",Resultsfiles,".rdata")
loadfile2 = paste0(ResultsFolder,"/",Convertfile,".rdata")
DataFolder = paste0(RootDir,"/",DataFolder)
Areasdatfile = paste0(DataFolder,"/",Areasdatfile)

rm(thisdir)
#
source(RunFile)
