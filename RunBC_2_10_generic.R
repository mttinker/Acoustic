# RunBC_EDIT_THIS_PART = User File to Run "BayesCalls" Program to analyze Acoustic call data 
#    using a Bayesian State Space model (BayesCalls)
#    (***filename needs to begin with "RunBC_")
# Purpose of this file is for user to specify parameters needed to run analysis
# NOTE: Results file will get same name as this script, with "RunBC" replaced by "Results"

rm(list = ls())
gc()

existing_Packages<-as.list(installed.packages()[,1])
# Add new packages you might need to this line, to check if they're installed and install missing packages
required_Packages<-c("rstudioapi")
missing_Packages<- required_Packages[!required_Packages%in% existing_Packages]
if(length(missing_Packages)>0)install.packages(pkgs =  missing_Packages)
invisible(lapply(required_Packages, require, character.only=T,quietly = T))

start.time <- Sys.time()  # start a timer

# USER SPECIFIED PARAMETERS -------------------------------------------------
#
# First provide a "Root Directory" within which all other folders, subfolders & files are contained
#   NOTE: enter "current" for current folder, "above1" for 1 folder above, "above2" for 2 above, 
#   otherwise enter path with "/" separator for sub-folders
RootDir =  "above1"  # Examples "current" or "above1" or "C:/Users/XXXX/BayesianMethods"
AnalysisFolder = 'Acoustic2'  # Folder path within RootDir where analysis code is stored
RunFile = 'BayesCalls2_10'       # Version of BayesCalls to run
DataFolder = 'CapCays/data'  # Folder path within RootDir where raw data files stored
ResultsFolder = 'CapCays/results'  # Folder path within RootDir where results files stored
ProjectName =  'Bayesian_2014'  # Name of project (for user reference)
Species =  'WTSH'  # Species name for data analysis
Countsdatfile = c(paste0('Counts_CapCays_',Species,'_2014-2017.csv')) # Name of matching data file with nest count data (OTIONAL, enter blank if no nest counts)
Datafile =  paste0('BayesCalls2_02_',Species,'_2014-2017_NoAudit.RData')  #Name of data file for analysis 
ProjectLocation =  'Queensland'  # Island name for data analysis
ProjectYear =  2014  # Year of analysis project 
Yearfocal =  2014  # Focal year for Bayesian analysis

TS_strt =  1  # Starting time step (beginning of range to load data)
TS_stop =  57  # Ending timestep (end of range to load data)
TimeStep1 =  '5:00PM'  # Clock time (24hr) of timestep1
TimeStepIntvl =  15  # Number of minutes in each timestep
subsamp =  5  # Level of Sub-sampling of entire data set: use every nth record

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
  peakdates_strt = 50
  if (Yearfocal==2016) { peakdates_end = 109 } else { peakdates_end = 109 }
  calendar_pk_opt = 1 # 1 = all in one year, 2 = peak spans new year
} else if (Species=='BLNO') {
  # use wide "index" period to capture any peak
  if (Yearfocal==2017) { peakdates_strt = 345 } else { peakdates_strt = 344 } # Dec 10
  if (Yearfocal==2016) { peakdates_end = 121 } else { peakdates_end = 120 } # April 30
  calendar_pk_opt = 2 # 1 = all in one year, 2 = peak spans new year
}

# DayOfYear_strt =  70  # Day of Year (integer) to start reading data
# DayOfYear_end =  280  # Day of Year (integer) to stop reading data
# peakdates_strt =  112  # Day of Year that Peak period is considered to start
# peakdates_end =  172  # Day of Year that Peak period is considered to stop

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

# peaktimes_strt =  30  # Peak time boundary 1, minutes relative to a reference event (sunrise or sunset)
# peaktimes_stop =  90  # Peak time boundary 2, always > than boundary1 (minutes relative to event) 
# peaktimes_ref =  2  # Reference event: 1 = after sunset, 2 = before sunrise, 3 = sunrise AND sunset 
# peaktimes_strt2 =  45  # OPTIONAL: if peaktimes_ref = 3, this is second Peak time boundary 1 
# peaktimes_stop2 =  120  # OPTIONAL: if peaktimes_ref = 3, this is second Peak time boundary 2 (always > boundary1) 

calendar_opt =  2  # year range: 1 = All data within calendar year, 2 = data spans New Year
data_opt =  1  # Data Option: 1 = Calls Only, 2 = Calls plus Nest Counts
QC_opt = 0   # QC option: 0 = filter/do not adjust for QC, 1 = adjust call rate w. fitted QC fxn
prior_opt = 1   # Priors: 1 = uninformed, 2 = informed (must supply results file)
PriorResultsFile =  c()  # OPTIONAL: if prior_opt = 2, Rdata file containing parameter priors

Nchains = 20
Nburnin =  1500  # Number of burn-in reps Total reps = (Nsim-Nburnin) * (num Cores)
Nadapt =  100  # Number of adapting reps, default 100
Totalreps = 5000 # Total desired reps (ie # simulations making up posterior)
#
# END USER SPECIFIED PARAMETERS ---------------------------------------------
#
# (Should not need to edit anything below here)
# 
Nsim =  Totalreps/Nchains + Nburnin  # Total # MCMS sims: Actual saved reps = (Nsim-Nburnin) * (num Cores)
# Process Filenames and directory names -------------------------------------
#
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

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
#
if (AnalysisFolder=='') {
    RunFile = paste0(RootDir,"/",RunFile,".R")
    AnalysisFolder = getwd()
} else {
    RunFile = paste0(RootDir,"/",AnalysisFolder,"/",RunFile,".R")
    AnalysisFolder = paste0(RootDir,"/",AnalysisFolder)
}
#
if (DataFolder=='') {
    loadfile1 = paste0(RootDir,"/",Datafile)
} else {
    loadfile1 = paste0(RootDir,"/",DataFolder,"/",Datafile)
}
#
if (data_opt>1){
  if (DataFolder=='') {
    loadfile2 =  paste0(RootDir,"/",Countsdatfile)
  } else {
    loadfile2 =  paste0(RootDir,"/",DataFolder,"/",Countsdatfile)
  }
}

DataFolder = paste0(RootDir,"/",DataFolder)
ResultsFolder = paste0(RootDir,"/",ResultsFolder)
#
#  Function to get the name of this current user script 
callscript <- function() {
    # http://stackoverflow.com/a/32016824/2292993
    cmdArgs = commandArgs(trailingOnly = FALSE)
    needle = "--file="
    match = grep(needle, cmdArgs)
    if (length(match) > 0) {
        # Rscript via command line
        return(normalizePath(sub(needle, "", cmdArgs[match])))
    } else {
        ls_vars = ls(sys.frames()[[1]])
        if ("fileName" %in% ls_vars) {
            # Source'd via RStudio
            return(normalizePath(sys.frames()[[1]]$fileName)) 
        } else {
            if (!is.null(sys.frames()[[1]]$ofile)) {
                # Source'd via R console
                return(normalizePath(sys.frames()[[1]]$ofile))
            } else {
                # RStudio Run Selection
                # http://stackoverflow.com/a/35842176/2292993  
                return(normalizePath(rstudioapi::getActiveDocumentContext()$path))
            }
        }
    }
}
# Get the name of current script file, and create matching SaveResults filename 
CallingScript = callscript()
# SaveResults = CallingScript
# SaveResults = gsub("RunBC","Results",CallingScript)
# SaveResults = paste0(SaveResults,"data")
# 
# SaveResults = gsub("BayesCalls","Results",RunFile)

substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}
SaveResults = substrRight(RunFile, 6)
SaveResults = paste0(ResultsFolder,'/Results', '_', Species, '_', ProjectYear, '_', SaveResults,"data")

rm('callscript')
rm('substrRight')
#
# Run Bayesian Analysis -----------------------------------------------------
# Source BayesCalls program (specify version as X_XX, e.g. BayesCalls2_01) 
#
source(RunFile)

Totalend.time <- Sys.time()
Totaltime.taken <- start.time - Totalend.time
Totaltime.taken

