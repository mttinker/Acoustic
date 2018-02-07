# Set-up for doing trend analysis from BayesCalls analysis
# Purpose of this file is for user to specify parameters for doing trend analysis
#
rm(list = ls())

# USER SPECIFIED PARAMETERS -------------------------------------------------

AnalysisFolder = 'D:/CM,Inc/Acoustic'
ResultsFolder = 'D:/CM,Inc/Dropbox (CMI)/CMI_Team/Analysis/2018/Bayesian_2018/results'
DataFolder = 'D:/CM,Inc/Dropbox (CMI)/CMI_Team/Analysis/2018/Bayesian_2018/data'
RunFile = 'BayesCallsTrends2_10'

Species =  'WTSH'  # Species name for data analysis
# Specify results files to be included (one for each year):
Resultsfiles = c(paste0("Results_", Species,"_2014_2_10"),
                 paste0("Results_", Species,"_2015_2_10"),
                 paste0("Results_", Species,"_2016_2_10"),
                 paste0("Results_", Species,"_2017_2_10"))
# Trendtype option: If type = 1, calculate trend for call rate, if 2 calculate for density,
#  if 3 calculate for "TOTAL" abundance, either whole-Island or whole-region
#   (assumes ALL strata sampled each year)
Trendtype = 2 
# "Strata_trends" character string used to specify focal strata for estimating trends: 
#   NOTE: all named strata must be present in each years data UNLESS
#    Trendtype = 3 (whole Island/region), in which case missing strata will be 
#    interpolated, although at least 1 strata (first listed) must be present all years
Strata_trends = c('North West Pisonia') #,'North West Fringe','North West Rock'
# FocalSites Option: can either estimate density for all sites each year (FocalSites = 0)
#  OR for a select sequence of sites that are present each year (FocalSites = 1)
FocalSites = 0
# If FocalSites = 1, specify sites within focal strata for estimating trends: 
#  NOTE: these must be sites in above-named strata and present in each years data!!
Sites_trends = c('')
# Folder path within RootDir where raw data files stored
DataFolder = 'CapCays/data'  
# Specify name of Data files with Conversion function and Areas of each strata
Convertfile = paste0(Species,'_CallsCountsFxn2_10') # Results file with conversion function
# Name of matching data file with nest count data (OTIONAL, enter blank if no nest counts)
Areasdatfile = 'QPWS_CapCays_Strata_Area.csv' 
#
# Params for JAGS
Nchains = 8
Nburnin =  7500  # Number of burn-in reps Total reps = (Nsim-Nburnin) * (num Cores)
Nadapt =  100  # Number of adapting reps, default 100
Totalreps = 10000 # Total desired reps (ie # simulations making up posterior)

# END USER SPECIFIED PARAMETERS ---------------------------------------------
#
# (Should not need to edit anything below here)
# 
# Process Filenames and directory names -------------------------------------
#
Nsim =  Totalreps/Nchains + Nburnin  # Total # MCMS sims: Actual saved reps = (Nsim-Nburnin) * (num Cores)

Resultsfiles = c()
for (i in Years) {
  Resultsfiles = c(Resultsfiles,paste0('Results_',Species,'_',i,'_2_10'))
}

Convertfile = paste0(Species,'_CallsCountsFxn2_10')
Countsdatfile = c(paste0('CapCays_Counts_',Species,'_2014-2017.csv')) # Name of matching data file with nest count data (OTIONAL, enter blank if no nest counts)

RunFile = paste0(AnalysisFolder,"/",RunFile,".r")
loadfiles = paste0(ResultsFolder,"/",Resultsfiles,".Rdata")
loadfile2 = paste0(ResultsFolder,"/",Convertfile,".Rdata")
Areasdatfile = paste0(DataFolder,'/CapCays_StrataArea_ALL.csv')

source(RunFile)
