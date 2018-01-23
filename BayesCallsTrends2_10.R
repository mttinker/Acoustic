# Program Summary ---------------------------------------------------------
# Script to estimate trends using analyis results from Bayesian Analyses of
#  acoustic data (and count data) on seabird calls
simsamp = 500;
# 
# Load necessary libraries (may need to install some of these)
library(coda)
library(mcmcplots)
library(rjags)
library(jagsUI)
library(gdata)
library(gtools)
library(lattice)
library(grid)
library(ggplot2)
library(doParallel)
# Load Data ---------------------------------------------------------------

Nyrs = length(loadfiles)
Nstrat = length(Strata_trends)
Nobs = simsamp*Nyrs*Nstrat
Year = numeric(length = Nobs)
YearN = numeric(length = Nobs)
Strat = character(length = Nobs)
StratN = numeric(length = Nobs)
CR = numeric(length = Nobs)
for (i in 1:Nyrs){
  attach(loadfiles[i]); 
  outdf <- outdf;
  vn <- vn
  Stratalist <- Stratalist
  Sitelist <- Sitelist
  Yearfocal <- Yearfocal
  detach(paste0('file:',loadfiles[i]),character.only = TRUE)
  if (FocalSites == 1){
    for (s in 1:Nstrat){
      idx = seq(((i-1)*Nstrat*simsamp+(s-1)*simsamp+1),((i-1)*Nstrat*simsamp+(s-1)*simsamp+simsamp))
      sitereps = numeric(length = simsamp)
      for (ss in 1:length(Sites_trends)){
        ii = which(as.character(Sitelist$Strata)==Strata_trends[s] & 
                     as.character(Sitelist$SPIDc)==Sites_trends[ss])
        if (length(ii)==1){
          iii = Sitelist$Sitenum[ii]
          sitereps = cbind(sitereps,sample(outdf[,which(vn==paste0('Cs[',ii,']'))],simsamp))
        }
      }
      sitereps = sitereps[,-1]
      CR[idx] = rowMeans(sitereps)
      Year[idx] = rep(Yearfocal,simsamp)
      Strat[idx] = rep(Strata_trends[s],simsamp)
      YearN[idx] = rep(i,simsamp)
      StratN[idx] = rep(s,simsamp)     
    }
  }else{
    for (s in 1:Nstrat){
      idx = seq(((i-1)*Nstrat*simsamp+(s-1)*simsamp+1),((i-1)*Nstrat*simsamp+(s-1)*simsamp+simsamp))
      ii = which(as.character(Stratalist$StratName)==Strata_trends[s])
      Year[idx] = rep(Yearfocal,simsamp)
      Strat[idx] = rep(Strata_trends[s],simsamp)
      YearN[idx] = rep(i,simsamp)
      StratN[idx] = rep(s,simsamp)
      if (max(Stratalist$Stratnum)>1){
        CR[idx] = sample(outdf[,which(vn==paste0('C[',ii,']'))],simsamp)
      }else{
        CR[idx] = sample(outdf[,which(vn=='C')],simsamp)
      }
    }
  }
  rm(outdf)
  rm(Stratalist)
  rm(Sitelist)
  rm(Yearfocal)
  rm(vn)
}
dfT = data.frame(Year=Year,Strata=Strat,YearN=YearN,StratN=StratN,CR=CR)
Yearvals = sort(unique(dfT$Year))
  
# Examine data ---------------------------------------------------------

pl1 = ggplot(data=dfT) + 
  geom_boxplot( aes(x=factor(YearN), y=CR, fill=factor(Strata)), position=position_dodge(1)) +
  scale_x_discrete(breaks=seq(1,Nyrs), labels=as.character(unique(Year)))
print(pl1)
# Process data for analysys --------------------------------------------
setwd(AnalysisFolder)

TauVals = matrix(nrow = Nyrs, ncol = Nstrat)
Vvals = matrix(nrow = Nyrs, ncol = Nstrat)
Cmean = matrix(nrow = Nyrs, ncol = Nstrat)
rhat =  matrix(nrow = Nyrs-1, ncol = Nstrat)
for (i in 1:Nyrs){
  for (j in 1:Nstrat){
    TauVals[i,j] = 1/var(CR[YearN==i & StratN==j])
    Vvals[i,j] = var(CR[YearN==i & StratN==j])
    Cmean[i,j] = mean(CR[YearN==i & StratN==j])
    if(i>1){
      rhat[i-1,j] = log(Cmean[i,j]/Cmean[i-1,j])
    }
  }
}
# Set up for JAGS -----------------------------------------------------
set.seed(123)
# For parallel (comment out for serial)
cores<-detectCores()
cores = min(cores, Nchains)
cl <- makeCluster(cores[1])
registerDoParallel(cl)
# Data structure
data <- list(CR=CR,Nobs=Nobs,Nyrs=Nyrs,Nstrat=Nstrat,
             YearN=YearN,StratN=StratN,Vvals=Vvals,
             Cmean=Cmean,TauVals=TauVals) 

# Inits: Best to generate initial values using function
inits <- function(){
  list(rmean=runif(1,-.01,.01),sigR=runif(1,0.05,0.1),r=rhat)
}
# List of parameters to monitor:
# Model to be run:
if(Nstrat==1){
  modfile = 'Jags_Trend1.jags'  
  params <- c('rmean','sigR','r','C')  
}else{
  modfile = 'Jags_Trend.jags'  
  params <- c('rmean','sigR','sigS','rY','r','C')  
}

#
# Run JAGS ----------------------------------------------------------------
#
# For detailed output stats & DIC use jags(), but do not inlcude Temporal or E
#  in params since they take up too much memory and it crashes
# To get Temporal matrix, use jags.basic() to save just basic mcmc list
#  which uses much less memory but does not save associated stats & DIC
out <- jags.basic(data = data,
                  inits = inits,
                  parameters.to.save = params,
                  model.file = modfile,
                  n.chains = Nchains,
                  n.adapt = Nadapt,
                  n.iter = Nsim,
                  n.burnin = Nburnin,
                  n.thin = 1,
                  parallel=TRUE,
                  n.cores=cores)
# Diagnostics -------------------------------------------------------------
stopCluster(cl)
vn = varnames(out)
outmat = as.matrix(out)
reps = dim(outmat); reps = reps[1]
outdf = as.data.frame(outmat); rm(outmat)
s = summary(out) 
s_stats = data.frame(s$statistics)
s_quantiles = data.frame(s$quantiles) 
Np = length(params)
# Diagnostic plots --------------------------------------------------------
for (i in 1:Np){
  parnm = params[i]
  traplot(out,parnm)
}
for (i in 1:Np){
  parnm = params[i]
  denplot(out,parnm,ci=.9,collapse = TRUE)
}
# Plot boxplot of growth rates by strata
SampN = (Nyrs-1)*Nstrat*simsamp
YearR = numeric(length = SampN)
StratR = character(length = SampN)
r_est = numeric(length = SampN)
for (i in 1:(Nyrs-1)){
  for (j in 1:Nstrat){
    idx = seq(((i-1)*Nstrat*simsamp+(j-1)*simsamp+1),((i-1)*Nstrat*simsamp+(j-1)*simsamp+simsamp))
    YearR[idx] = rep(Yearvals[i],simsamp)
    StratR[idx] = rep(Strata_trends[j],simsamp)
    r_est[idx] = sample(outdf[,which(vn==paste0('r[',i,',',j,']'))],simsamp)
  }
}

parnum_r = which(startsWith(params,'r'))
if(Nstrat==1){
  labels_r = c("Mean r overall")
  for(y in 1:(Nyrs-1)){
    labels_r = c(labels_r, paste0("Mean r for ", as.character(Yearvals[y])))
  }
}else{
  labels_r = c("Mean r overall")
  for(y in 1:(Nyrs-1)){
    labels_r = c(labels_r, paste0("Mean r for ", as.character(Yearvals[y])))
  }
  for (j in 1:Nstrat){
      for(y in 1:(Nyrs-1)){
         labels_r = c(labels_r, paste0("Strata r for ",Strata_trends[j]," in ",as.character(Yearvals[y])))
      }
  } 
}
caterplot(out,params[parnum_r],denstrip = FALSE, reorder=FALSE,
          quantiles=list(outer=c(0.025,0.975),inner=c(0.1666,0.8333)),lwd=c(.1,4),
          labels=labels_r, labels.loc = 'above',las = 0, cex.labels = .8)
title(main = "Estimated rate of change", font.main = 4)

dfR = data.frame(Year=YearR,Strata=StratR,r_est = r_est)
pl2 = ggplot(data=dfR) + 
  geom_boxplot( aes(x=factor(YearR), y=r_est, fill=factor(Strata)), position=position_dodge(1)) +
  scale_x_discrete(name = "Year", breaks=Yearvals[1:(Nyrs-1)], labels=as.character(Yearvals[1:(Nyrs-1)])) +
  scale_y_continuous(name = "Estimated growth rate (r)") +
  scale_fill_discrete(name = "Strata")
print(pl2)
