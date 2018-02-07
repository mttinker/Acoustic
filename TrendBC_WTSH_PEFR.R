# Set-up for doing trend analysis from BayesCalls analysis
# Purpose of this file is for user to specify parameters for doing trend analysis
#
rm(list = ls())

# USER SPECIFIED PARAMETERS -------------------------------------------------

AnalysisFolder = 'D:/CM,Inc/Acoustic'
ResultsFolder = 'D:/CM,Inc/Dropbox (CMI)/CMI_Team/Analysis/2018/Bayesian_2018/results'
DataFolder = 'D:/CM,Inc/Dropbox (CMI)/CMI_Team/Analysis/2018/Bayesian_2018/data'
RunFile = 'BayesCallsTrends2_10'

Species =  'BLNO'  # Species name for data analysis
Years = c(2014,2015,2016,2017)

# Trendtype option: If type = 1, calculate trend for call rate, if 2 calculate for density,
#  if 3 calculate for "TOTAL" abundance, either whole-Island or whole-region
#   (assumes ALL strata sampled each year)
Trendtype = 2

# Specify focal strata for estimating trends: 
#   NOTE: all named strata must be present in each years data!!
#   ALSO: If Trendtype = 3 then specify Strata for whole Island, or whole Region
Strata_trends = c('North West Pisonia')

# Can either estimate density for all sites each year (FocalSites = 0)
#  OR for a select sequence of sites present each year (FocalSites = 1)
FocalSites = 0

if (FocalSites==1) {
  # Specify sites within focal strata for estimating trends: 
  #  NOTE: these must be sites in above-named strata and present in each years data!!
  Sites_trends = c('')
}

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
