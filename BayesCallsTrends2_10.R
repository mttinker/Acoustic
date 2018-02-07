# Program Summary ---------------------------------------------------------
# Script to estimate trends using analyis results from Bayesian Analyses of
#  acoustic data (and count data) on seabird calls
simsamp = 500;
# 
# Load necessary libraries (may need to install some of these)
existing_Packages<-as.list(installed.packages()[,1])
# Add new packages you might need to this line, to check if they're installed and install missing packages
required_Packages<-c('parallel','doParallel','mvtnorm','tidyverse','reshape2','fitdistrplus','jagsUI','coda','mcmcplots')
missing_Packages<- required_Packages[!required_Packages%in% existing_Packages]
if(length(missing_Packages)>0)install.packages(pkgs =  missing_Packages)
invisible(lapply(required_Packages, require, character.only=T,quietly = T))
# Load Data ---------------------------------------------------------------
dfArea = read.csv(file = Areasdatfile, header = TRUE, sep = ",")
attach(loadfile2); 
Convfxn = Convertfxn
Convplot = Convertplot
detach(paste0('file:',loadfile2),character.only = TRUE)
AB = rmvnorm(n=simsamp, mean=Convfxn$means, sigma=Convfxn$covmat)
alph = AB[,1]; Beta = AB[,2]; rm(AB)
alphMN = Convfxn$means[1]; BetaMN = Convfxn$means[2]
#
Nyrs = length(loadfiles)
Nstrat = length(Strata_trends)
Nobs = simsamp*Nyrs*Nstrat
Year = numeric(length = Nobs)
YearN = numeric(length = Nobs)
Strat = character(length = Nobs)
StratN = numeric(length = Nobs)
CR = numeric(length = Nobs)
DNS = numeric(length = Nobs)
ABND = numeric(length = Nobs)
Rep = numeric(length = Nobs)
RepN = seq(1:simsamp)
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
      DNS[idx] = pmax(.0001,alph*CR[idx]^Beta)
      Area = dfArea$Area[as.character(dfArea$StrataName)==as.character(Strata_trends[s])]
      ABND[idx] = DNS[idx]*Area
      Year[idx] = rep(Yearfocal,simsamp)
      Strat[idx] = rep(Strata_trends[s],simsamp)
      YearN[idx] = rep(i,simsamp)
      StratN[idx] = rep(s,simsamp)
      Rep[idx] = RepN
    }
  }else{
    for (s in 1:Nstrat){
      idx = seq(((i-1)*Nstrat*simsamp+(s-1)*simsamp+1),((i-1)*Nstrat*simsamp+(s-1)*simsamp+simsamp))
      ii = which(as.character(Stratalist$StratName)==Strata_trends[s])
      Year[idx] = rep(Yearfocal,simsamp)
      Strat[idx] = rep(Strata_trends[s],simsamp)
      YearN[idx] = rep(i,simsamp)
      StratN[idx] = rep(s,simsamp)
      Area = dfArea$Area[as.character(dfArea$StrataName)==as.character(Strata_trends[s])]
      if (length(ii)==1){
        if (max(Stratalist$Stratnum)>1){
          CR[idx] = sample(outdf[,which(vn==paste0('C[',ii,']'))],simsamp)
        }else{
          CR[idx] = sample(outdf[,which(vn=='C')],simsamp)
        }
        DNS[idx] = pmax(0.0001,alph*CR[idx]^Beta)
        ABND[idx] = DNS[idx]*Area      
        Rep[idx] = RepN
      }
    }
  }
  rm(outdf)
  rm(Stratalist)
  rm(Sitelist)
  rm(Yearfocal)
  rm(vn)
}
dfT = data.frame(Year=Year,Strata=Strat,Rep=Rep,YearN=YearN,StratN=StratN,CR=CR,DNS=DNS,ABND=ABND)
Yearvals = sort(unique(dfT$Year))

if (Trendtype==3){
  dfIsl = dcast(dfT[dfT$Rep>0,c(3,4,5,8)],YearN + Rep ~ StratN, value.var='ABND')
  dfIsl$Total = apply(dfIsl[3:dim(dfIsl)[2]],1,sum,na.rm=TRUE)
  Nobs = length(dfIsl$Total)
  # Adjust for years with not all strata measured
  if(length(which(dfT$Rep==0))>1){
    for (j in 2:Nstrat){
      ii = which(!is.na(dfIsl[,2+j]))
      fitprp = fitdist(dfIsl[ii,2+j]/dfIsl[ii,1+j],'lnorm')
      ii = which(is.na(dfIsl[,2+j]))
      dfIsl[ii,2+j] = dfIsl[ii,1+j]*rlnorm(ii,fitprp$estimate[1],fitprp$estimate[2])
    }
    dfIsl$Total = apply(dfIsl[3:(2+Nstrat)],1,sum,na.rm=TRUE)
  }
}
  
# Examine data ---------------------------------------------------------

pl1 = ggplot(data=dfT) + 
  geom_boxplot( aes(x=factor(YearN), y=CR, fill=factor(Strata)), position=position_dodge(1)) +
  scale_x_discrete(breaks=seq(1,Nyrs), labels=as.character(unique(Year))) + ylab("Call Rate")
print(pl1)

pl1b = ggplot(data=dfT) + 
  geom_boxplot( aes(x=factor(YearN), y=DNS, fill=factor(Strata)), position=position_dodge(1)) +
  scale_x_discrete(breaks=seq(1,Nyrs), labels=as.character(unique(Year))) + ylab("Mean Density")
print(pl1b)

pl1c = ggplot(data=dfT) + 
  geom_boxplot( aes(x=factor(YearN), y=ABND, fill=factor(Strata)), position=position_dodge(1)) +
  scale_x_discrete(breaks=seq(1,Nyrs), labels=as.character(unique(Year))) + ylab("Abundance")
print(pl1c)

if (Trendtype==3){
  Nstrat = 1
  pl1d = ggplot(data=dfIsl) + 
    geom_boxplot( aes(x=factor(YearN), y=Total),fill=3) +
    scale_x_discrete(breaks=seq(1,Nyrs), labels=as.character(unique(Year))) +
    ylab("Abundance") + ggtitle("Whole-Island Abundance")
  print(pl1d)
}

# Process data for analysys --------------------------------------------
setwd(AnalysisFolder)

TauVals = matrix(nrow = Nyrs, ncol = Nstrat)
Vvals = matrix(nrow = Nyrs, ncol = Nstrat)
Cmean = matrix(nrow = Nyrs, ncol = Nstrat)
Dmean = matrix(nrow = Nyrs, ncol = Nstrat)
Amean = matrix(nrow = Nyrs, ncol = Nstrat)
rhat =  matrix(nrow = Nyrs-1, ncol = Nstrat)
for (i in 1:Nyrs){
  for (j in 1:Nstrat){
    if (Trendtype==1){
      stattmp = CR[YearN==i & StratN==j]
    }else if(Trendtype==2){
      stattmp = DNS[YearN==i & StratN==j]
    }else if(Trendtype==3){
      stattmp = dfIsl$Total[dfIsl$YearN==i]  
    }
    TauVals[i,j] = 1/var(stattmp)
    Vvals[i,j] = var(stattmp)
    if(Trendtype==3){
      Amean[i,j] = mean(stattmp)
      if(i>1){
        rhat[i-1,j] = log(Amean[i,j]/Amean[i-1,j])
      }
    }else{
      Cmean[i,j] = mean(CR[YearN==i & StratN==j])
      Dmean[i,j] = alphMN*Cmean[i,j]^BetaMN
      if(i>1){
        if (Trendtype==1){
          rhat[i-1,j] = log(Cmean[i,j]/Cmean[i-1,j])
        }else if(Trendtype==2){
          rhat[i-1,j] = log(Dmean[i,j]/Dmean[i-1,j])
        }
      }
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
if (Trendtype==1){
  stattmp = CR
  statmn = Cmean
}else if(Trendtype==2){
  stattmp = DNS
  statmn = Dmean
}else if(Trendtype==3){
  stattmp = dfIsl$Total
  statmn = Amean
  YearN = dfIsl$YearN
  StratN = numeric(length = length(YearN))+1
}

data <- list(CR=stattmp,Nobs=Nobs,Nyrs=Nyrs,Nstrat=Nstrat,
             YearN=YearN,StratN=StratN,Vvals=Vvals,
             Cmean=statmn,TauVals=TauVals) 
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
if(Nstrat>1){
  YearRR = numeric(length = (Nyrs-1)*simsamp)
  ry_est = numeric(length = (Nyrs-1)*simsamp)
}
for (i in 1:(Nyrs-1)){
  if(Nstrat>1){
    idxx = seq(((i-1)*simsamp+1),((i-1)*simsamp+simsamp))
    YearRR[idxx] = rep(Yearvals[i],simsamp)
    if (Nyrs < 3){
      ry_est[idxx] = sample(outdf[,which(vn=='rY')],simsamp)
    }else{
      ry_est[idxx] = sample(outdf[,which(vn==paste0('rY[',i,']'))],simsamp)
    }
  }
  for (j in 1:Nstrat){
    idx = seq(((i-1)*Nstrat*simsamp+(j-1)*simsamp+1),((i-1)*Nstrat*simsamp+(j-1)*simsamp+simsamp))
    YearR[idx] = rep(Yearvals[i],simsamp)
    if (Trendtype<3){
      StratR[idx] = rep(Strata_trends[j],simsamp)
    }else{
      StratR[idx] = rep("Whole Island",simsamp)
    }
    if (Nyrs < 3 & Nstrat==1){
      r_est[idx] = sample(outdf[,which(vn=='r')],simsamp)
    }else{
      r_est[idx] = sample(outdf[,which(vn==paste0('r[',i,',',j,']'))],simsamp)
    }
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

if(Nstrat==1){
  pl2 = ggplot(data=dfR) + 
    geom_boxplot( aes(x=factor(Year), y=r_est), fill=3) +
    scale_x_discrete(name = "Year", breaks=Yearvals[1:(Nyrs-1)], labels=as.character(Yearvals[1:(Nyrs-1)])) +
    scale_y_continuous(name = "Estimated growth rate (r)") +
    ggtitle(paste0('Trend(s) for ', as.character(dfR$Strata[1])))
}else{
  pl2 = ggplot(data=dfR) + 
  geom_boxplot( aes(x=factor(Year), y=r_est, fill=factor(Strata)), position=position_dodge(1)) +
  scale_x_discrete(name = "Year", breaks=Yearvals[1:(Nyrs-1)], labels=as.character(Yearvals[1:(Nyrs-1)])) +
  scale_y_continuous(name = "Estimated growth rate (r)") +
  scale_fill_discrete(name = "Strata") + ggtitle('Trend(s) by Strata')
}
print(pl2)

if(Nstrat>1){
  dfRR = data.frame(Year=YearR,r_est = ry_est)
  pl3 = ggplot(data=dfRR) + 
    geom_boxplot( aes(x=factor(Year), y=r_est)) +
    scale_x_discrete(name = "Year", breaks=Yearvals[1:(Nyrs-1)], labels=as.character(Yearvals[1:(Nyrs-1)])) +
    scale_y_continuous(name = "Mean Estimated growth rate (r)") + 
    ggtitle('Trend(s) Averaged across Strata') 
  print(pl3)
}

