# Set-up for doing trend analysis from BayesCalls analysis
# Purpose of this file is for user to specify parameters for doing trend analysis
#
# USER SPECIFIED PARAMETERS -------------------------------------------------
# Set root directory path... enter absolute path or relative,
RootDir =  "above1"  # Examples "current" or "above1" or "C:/Users/XXXX/BayesianMethods"
AnalysisFolder = 'Acoustic2'  # Folder path within RootDir where analysis code is stored
ResultsFolder = 'CapCays/results'  # Folder path within RootDir where results files stored
RunFile = 'BayesCallsTrends2_10'       # Plotting Script
# Specify results files to be included (one for each year):
Resultsfiles = c("Results_WTSH_2014_2_10",
                 "Results_WTSH_2015_2_10",
                 "Results_WTSH_2016_2_10",
                 "Results_WTSH_2017_2_10")
# Specify focal strata for estimating trends: 
#  NOTE: these strata must be present in each years data!!
Strata_trends = c('North West Pisonia')
# Can either estimate density for all sites each year (FocalSites = 0)
#  OR for a select sequence of sites present each year (FocalSites = 1)
FocalSites = 0
# If FocalSites = 1, specify sites within focal strata for estimating trends: 
#  NOTE: these must be sites in above-named strata and present in each years data!!
Sites_trends = c('')

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

rm(thisdir)
#
source(RunFile)
