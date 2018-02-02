# Script to set up and run Power Analysis to determine ability to detect trends 
#  in seabird colonies using data from Acoustic monitors and nest counts, 
#  based on results from existing analyses
rm(list=ls())
# USER SPECIFIED PARAMETERS -------------------------------------------------
#
RootDir =  "above1"  # Examples "current" or "above1" or "C:/Users/XXXX/BayesianMethods"
AnalysisFolder = 'Acoustic2'  # Folder path within RootDir where plot code is stored
RunFile = 'BayesCallsPower2_10'       # Plotting Script
Species =  'WTSH'  # Species name for data analysis
# Specify results file to use for variance parameter estimates:
Resultsfile = c("Results_WTSH_2017_2_10")
ResultsFolder = 'CapCays/results'  # Folder path within RootDir where results files stored
Strata_trends = c('North West Pisonia')
DataFolder = 'CapCays/data'  
# Specify name of Data files with Conversion function and Areas of each strata
Convertfile = 'WTSH_CallsCountsFxn2_10' # Results file with conversion function
# Name of matching data file with nest count data (OTIONAL, enter blank if no nest counts)
Areasdatfile = 'QPWS_CapCays_Strata_Area.csv' 
Countsdatfile = c(paste0('Counts_CapCays_',Species,'_2014-2017.csv')) # Name of matching data file with nest count data (OTIONAL, enter blank if no nest counts)
#
# Set parameters for Power Analysis
simreps = 100       # Number reps for Power analysis (recomend at least 100)
NyrsP = 15          # Number of Years of Monitoring
TRUE_r = -0.03     # Desired detectable trend (ie true trend)
Sigma_r = 0.1       # Process error: standard deviation in trend over years
NSiteA = 40         # Number of Sites Monitored per year (Acoustic)
Countfreq = 2       # Frequency of nest counts (counts every X years)(0=no counts)
NSiteC = 40         # Number of Sites with nest counts on count years
NcountsPSite = 2    # Avg Number Nest count replicates per site 
RecPsite = 1000     # Number of 15min acoustic records per site
P_signif = 0.95     # Desired level of certainty for CI and P values
#
# END USER SPECIFIED PARAMETERS ---------------------------------------------
#
# (Should not need to edit anything below here)
# 
# Process Filenames and directory names -------------------------------------
#
library(rstudioapi)
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
loadfile = paste0(ResultsFolder,"/",Resultsfile,".rdata")
loadfile2 = paste0(ResultsFolder,"/",Convertfile,".rdata")
DataFolder = paste0(RootDir,"/",DataFolder)
Areasdatfile = paste0(DataFolder,"/",Areasdatfile)
loadCdat = paste0(DataFolder,"/",Countsdatfile)
rm(thisdir)
#
source(RunFile)