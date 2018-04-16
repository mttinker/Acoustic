# Script to set up and run Power Analysis to determine ability to detect trends 
#  in seabird colonies using data from Acoustic monitors and nest counts, 
#  based on results from existing analyses

rm(list=ls())

# library(beepr)
library(stringr)

# USER SPECIFIED PARAMETERS -------------------------------------------------

AnalysisFolder = 'D:/CM,Inc/Acoustic'  # Folder path within RootDir where plot code is stored
ResultsFolder = 'D:/CM,Inc/Dropbox (CMI)/CMI_Team/Analysis/2018/Bayesian_2018/results'  # Folder path within RootDir where results files stored
DataFolder = 'D:/CM,Inc/Dropbox (CMI)/CMI_Team/Analysis/2018/Bayesian_2018/data'  
RunFile = 'BayesCallsPower2_10'       # Plotting Script

Species =  'HAPE'  # Species name for data analysis
Yearfocal = 2017 # results file used for variance parameter estimates

Strata_trends = c('Upper Limahuli')

if(Species=='BLNO'|Species=='WTSH') {
  ProjectLocation='CapCays'
} else if (Species=='HAPE'|Species=='NESH') {
  ProjectLocation='Kauai'
} else if (Species=='Aerial'|str_detect(Species,'Growl')) {
  ProjectLocation='Midway'
}

CountType = ''; IndexPeriod = ''; QC_opt = '';
if (Species=='Aerial'|str_detect(Species,'Growl')) {CountType = '_10M'} # _Habitat or _Flyover, _10M or _T
if (Species=='WTSH') {CountType = '_TotalBurrows'; IndexPeriod = '_DecJan'} # '_TotalBurrows' or '_Occupied', '' (blank) or '_DecJan'
if (Species=='HAPE'|Species=='NESH') {QC_opt='_QC1'}

# Set parameters for Power Analysis
NyrsP = 10          # Number of Years of Monitoring
TRUE_r = -0.035     # Desired detectable trend (ie true trend)
NSiteA = 10         # Number of Sites Monitored per year (Acoustic)
Countfreq = 0       # Frequency of nest counts (counts every X years)(0=no counts)
NSiteC = 0         # Number of Sites with nest counts on count years
NcountsPSite = 0    # Avg Number Nest count replicates per site 
RecPsite = 5000     # Number of 15min acoustic records per site
P_signif = 0.95     # Desired level of certainty for CI and P values

if (Species=='WTSH') {
  Sigma_r = 0.13       # Process error: standard deviation in trend over years
} else if (Species=='BLNO') {
  Sigma_r = 0.1 # 0.1 without 2015, 0.4 with 2015...
} else if (Species=='HAPE'|Species=='NESH') {
  Sigma_r = 0.1       # Process error: standard deviation in trend over years
}

# END USER SPECIFIED PARAMETERS ---------------------------------------------
#
# (Should not need to edit anything below here)
# 
# Process Filenames and directory names -------------------------------------
#

simreps = 1500       # Number reps for Power analysis (recomend at least 100)

# Specify results file to use for variance parameter estimates:
Resultsfile = c(paste0(ProjectLocation, '_', Species, '_Results_2_10_', Yearfocal,IndexPeriod,QC_opt))
if (Species!='HAPE'&Species!='NESH') {Convertfile = paste0(ProjectLocation, '_', Species, '_CallsCountsFxn_2_10', CountType, IndexPeriod)} else {Convertfile = ''}
if (Species!='HAPE'&Species!='NESH') {Countsdatfile = c(paste0(ProjectLocation,'_',Species,'_Counts',CountType,'_2014-2017.csv'))} else {Countsdatfile = ''}

RunFile = paste0(AnalysisFolder,"/",RunFile,".r")
loadfile = paste0(ResultsFolder,"/",ProjectLocation,'/',Resultsfile,".Rdata")
loadfile2 = paste0(ResultsFolder,"/",ProjectLocation,'/',Convertfile,".Rdata")
if (Species!='HAPE'&Species!='NESH') {Areasdatfile = paste0(DataFolder,'/CapCays/CapCays_StrataArea_ALL.csv')} else {Areasdatfile = ''}
loadCdat = paste0(DataFolder,"/",ProjectLocation,'/',Countsdatfile)

source(RunFile)

# beep('mario')
