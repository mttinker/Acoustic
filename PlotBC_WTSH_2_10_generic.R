# Set-up for doing plots from BayesCalls analysis
# Purpose of this file is for user to specify parameters needed for plotting results
# NOTE: Assumed that this script is in same directory as Results file
# USER SPECIFIED PARAMETERS -------------------------------------------------
Resultsfile = "Results_WTSH_2016_2_10"
ResultsFolder = 'CapCays/results'  # Folder path within RootDir where results files stored
# Set root directory path... enter absolute path or relative,
RootDir =  "above1"  # Examples "current" or "above1" or "C:/Users/XXXX/BayesianMethods"
AnalysisFolder = 'Acoustic2'  # Folder path within RootDir where plot code is stored
DataFolder = 'CapCays/data'  # Folder path within RootDir where raw data files stored
RunFile = 'BayesCallsPlots2_10'       # Plotting Script
# Next lines used to select sites for making plots of mean expected call rate
#  over a focal period (can be different from peak period)
Nfocalsites = 3 # Number of sites defined below... generally 10 or less
Site_focal = character()
Site_focal[1] = 'NW2'  # Site name for detailed plots
Site_focal[2] = 'NW3'  # Site name for detailed plots
Site_focal[3] = 'NW4'  # Site name for detailed plots
# Define focal period of interest
DateFocalStrt = "February 1"
DateFocalStop = "February 28"
TimeFocalStrt = -120  # Time boundary 1, minutes relative to a reference event (sunrise or sunset)
TimeFocalStop =  -60  # Time boundary 2, always > than boundary1 (minutes relative to event) 
Time_ref =  2  # Reference event: 1 = after sunset, 2 = before sunrise
# Specify name of Data file with Areas of each strata
Areasdatfile = c(paste0('QPWS_CapCays_Strata_Area.csv')) # Name of matching data file with nest count data (OTIONAL, enter blank if no nest counts)

# END USER SPECIFIED PARAMETERS ---------------------------------------------
#
# (Should not need to edit anything below here)
# 
# Process Filenames and directory names -------------------------------------
#
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

RunFile = paste0(RootDir,"/",AnalysisFolder,"/",RunFile,".r")
ResultsFolder = paste0(RootDir,"/",ResultsFolder)
DataFolder = paste0(RootDir,"/",DataFolder)
Areasdatfile = paste0(DataFolder,"/",Areasdatfile)

loadfile1 = paste0(ResultsFolder,"/",Resultsfile,".rdata")
rm(thisdir)
#
source(RunFile)
