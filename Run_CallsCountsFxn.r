# Run_CallsCounts
# Set-up for estimating Calls vs Counts conversion function
# (allows estimating density from call rate)
# 

rm(list=ls())
gc()

# NOTE: Assumed that this script is in same directory as Results files
rm(list = ls())
# USER SPECIFIED PARAMETERS -------------------------------------------------
# Set root directory path... enter absolute path or relative,
RootDir =  "above1"  # Examples "current" or "above1" or "C:/Users/XXXX/BayesianMethods"
AnalysisFolder = 'Acoustic2'  # Folder path within RootDir where analysis code is stored
ResultsFolder = 'CapCays/results'  # Folder path within RootDir where results files stored
DataFolder = 'CapCays/data'  # Folder path within RootDir where raw data files stored
RunFile = 'CallsCountsFxn2_10'       # Plotting Script
Species =  'BLNO'  # Species name for data analysis
# Specify results files to be included (one for each year):
Resultsfiles = c(paste0("Results_", Species,"_2014_2_10"),
                 paste0("Results_", Species,"_2015_2_10"),
                 paste0("Results_", Species,"_2016_2_10"))
# Specify name of Data file with Areas of each strata
Countsdatfile = c(paste0('Counts_CapCays_',Species,'_2014-2017.csv')) # Name of matching data file with nest count data (OTIONAL, enter blank if no nest counts)
Areasdatfile = c(paste0('QPWS_CapCays_Strata_Area.csv')) # Name of matching data file with nest count data (OTIONAL, enter blank if no nest counts)
Seasondefine = c(2,2,1,1,1,3,3,3,3,3,3,2)
Nchains = 20
Nburnin =  7000  # Number of burn-in reps Total reps = (Nsim-Nburnin) * (num Cores)
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
  Resultsfiles = c(Resultsfiles,paste0('Results_',Species,'_',i,'_2_10'))
}

Countsdatfile = c(paste0('CapCays_Counts_',Species,'_',StartYear,'-',StopYear,'.csv')) # Name of matching data file with nest count data (OTIONAL, enter blank if no nest counts)
Areasdatfile = c(paste0('CapCays_StrataArea_ALL.csv')) # Name of matching data file with nest count data (OTIONAL, enter blank if no nest counts)
Seasondefine = c(2,2,1,1,1,3,3,3,3,3,3,2)

savename = paste0(Species, "_", RunFile, ".Rdata")
RunFile = paste0(AnalysisFolder,"/",RunFile,".r")
loadfiles = paste0(ResultsFolder,"/",Resultsfiles,".Rdata")
loaddat = paste0(DataFolder,"/",Countsdatfile)
loadAdat = paste0(DataFolder,"/",Areasdatfile)
SaveResults = paste0(ResultsFolder,"/",savename)

source(RunFile)

#