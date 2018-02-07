# Script to set up and run Power Analysis to determine ability to detect trends 
#  in seabird colonies using data from Acoustic monitors and nest counts, 
#  based on results from existing analyses

rm(list=ls())

# USER SPECIFIED PARAMETERS -------------------------------------------------

AnalysisFolder = 'D:/CM,Inc/Acoustic'  # Folder path within RootDir where plot code is stored
ResultsFolder = 'D:/CM,Inc/Dropbox (CMI)/CMI_Team/Analysis/2018/Bayesian_2018/results'  # Folder path within RootDir where results files stored
DataFolder = 'D:/CM,Inc/Dropbox (CMI)/CMI_Team/Analysis/2018/Bayesian_2018/data'  
RunFile = 'BayesCallsPower2_10'       # Plotting Script

Species =  'WTSH'  # Species name for data analysis
Yearfocal = 2016 # results file used for variance parameter estimates

Strata_trends = c('North West Pisonia')

# Set parameters for Power Analysis
NyrsP = 13          # Number of Years of Monitoring
TRUE_r = -0.06     # Desired detectable trend (ie true trend)
NSiteA = 15         # Number of Sites Monitored per year (Acoustic)
Countfreq = 1       # Frequency of nest counts (counts every X years)(0=no counts)
NSiteC = 15         # Number of Sites with nest counts on count years
NcountsPSite = 1    # Avg Number Nest count replicates per site 
RecPsite = 10000     # Number of 15min acoustic records per site
P_signif = 0.95     # Desired level of certainty for CI and P values

if (Species=='WTSH') {
  Sigma_r = 0.13       # Process error: standard deviation in trend over years
} else if (Species=='BLNO') {
  Sigma_r = 0.1 # 0.1 without 2015, 0.4 with 2015...
}

# END USER SPECIFIED PARAMETERS ---------------------------------------------
#
# (Should not need to edit anything below here)
# 
# Process Filenames and directory names -------------------------------------
#

simreps = 1000       # Number reps for Power analysis (recomend at least 100)

# Specify results file to use for variance parameter estimates:
Resultsfile = c(paste0("Results_",Species,"_",Yearfocal,"_2_10"))
Convertfile = paste0(Species,'_CallsCountsFxn2_10')
Countsdatfile = c(paste0('CapCays_Counts_',Species,'_2014-2017.csv')) # Name of matching data file with nest count data (OTIONAL, enter blank if no nest counts)

RunFile = paste0(AnalysisFolder,"/",RunFile,".r")
loadfile = paste0(ResultsFolder,"/",Resultsfile,".Rdata")
loadfile2 = paste0(ResultsFolder,"/",Convertfile,".Rdata")
Areasdatfile = paste0(DataFolder,'/CapCays_StrataArea_ALL.csv')
loadCdat = paste0(DataFolder,"/",Countsdatfile)

source(RunFile)
