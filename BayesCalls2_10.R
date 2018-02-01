# Program Summary ---------------------------------------------------------
# Script to analyze acoustic data on seabird calls (and possibly nest counts)
#  using a Bayesian State Space model, executed using JAGS
#  NOTE: this should be sourced from a script file that sets user parameters
#
# LOAD NECESSARY LIBRARIES-----------------------------------------
existing_Packages<-as.list(installed.packages()[,1])
# Add new packages you might need to this line, to check if they're installed and install missing packages
required_Packages<-c("readxl","lubridate", "stringr", "dplyr", "gtools","coda",'lattice','parallel','fitdistrplus',"rjags","jagsUI","doParallel")
missing_Packages<- required_Packages[!required_Packages%in% existing_Packages]
if(length(missing_Packages)>0)install.packages(pkgs =  missing_Packages)
invisible(lapply(required_Packages, require, character.only=T,quietly = T))

# LOAD RAW DATA --------------------------------------------------------------------
load(loadfile1)
#
# Fit Priors if Desired ------------------------------------------------------------
#
if(prior_opt==2 & data_opt==1){
  # Load posteriors in from designated results data file
  attach(PriorResultsFile); 
  outdfPr <- outdf;
  vnPr <- vn
  detach(paste0('file:',PriorResultsFile),character.only = TRUE)  
  # Fit priors for Dispers, phi, psi, theta, and variance params
  #  sigT, sigD, sigN, sigS and sigC
  Infpriors = matrix(nrow = 5, ncol = 2)
  mdl = fitdist(outdfPr[,which(vnPr=="Dispers")],"lnorm")
  Infpriors[1,] = as.numeric(mdl$estimate); Infpriors[1,2] = 1/Infpriors[1,2]^2
  mdl = fitdist(outdfPr[,which(vnPr=="theta")],"norm")
  Infpriors[2,] = as.numeric(mdl$estimate); Infpriors[2,2] = 1/Infpriors[2,2]^2    
  mdl = fitdist(outdfPr[,which(vnPr=="sigT")],"lnorm")
  Infpriors[3,] = as.numeric(mdl$estimate); Infpriors[3,2] = 1/Infpriors[3,2]^2 
  mdl = fitdist(outdfPr[,which(vnPr=="sigN")],"lnorm")
  Infpriors[4,] = as.numeric(mdl$estimate); Infpriors[4,2] = 1/Infpriors[4,2]^2 
  mdl = fitdist(outdfPr[,which(vnPr=="sigS")],"lnorm")
  Infpriors[5,] = as.numeric(mdl$estimate); Infpriors[5,2] = 1/Infpriors[5,2]^2 
  rm(outdfPr)
  rm(vnPr)
  rm(mdl)
}else if(prior_opt==2 & data_opt==2){
  # Load posteriors in from designated results data file
  attach(PriorResultsFile); 
  outdfPr <- outdf;
  vnPr <- vn
  detach(paste0('file:',PriorResultsFile),character.only = TRUE)  
  # Fit priors for Dispers, phi, psi, theta, and variance params
  #  sigT, sigD, sigN, sigS and sigC
  Infpriors = matrix(nrow = 9, ncol = 2)
  mdl = fitdist(outdfPr[,which(vnPr=="Dispers")],"lnorm")
  Infpriors[1,] = as.numeric(mdl$estimate); Infpriors[1,2] = 1/Infpriors[1,2]^2
  mdl = fitdist(outdfPr[,which(vnPr=="phi")],"norm")
  Infpriors[2,] = as.numeric(mdl$estimate); Infpriors[2,2] = 1/Infpriors[2,2]^2
  mdl = fitdist(outdfPr[,which(vnPr=="psi")],"norm")
  Infpriors[3,] = as.numeric(mdl$estimate); Infpriors[3,2] = 1/Infpriors[3,2]^2  
  mdl = fitdist(outdfPr[,which(vnPr=="theta")],"norm")
  Infpriors[4,] = as.numeric(mdl$estimate); Infpriors[4,2] = 1/Infpriors[4,2]^2    
  mdl = fitdist(outdfPr[,which(vnPr=="sigT")],"lnorm")
  Infpriors[5,] = as.numeric(mdl$estimate); Infpriors[5,2] = 1/Infpriors[5,2]^2  
  mdl = fitdist(outdfPr[,which(vnPr=="sigD")],"lnorm")
  Infpriors[6,] = as.numeric(mdl$estimate); Infpriors[6,2] = 1/Infpriors[6,2]^2 
  mdl = fitdist(outdfPr[,which(vnPr=="sigN")],"lnorm")
  Infpriors[7,] = as.numeric(mdl$estimate); Infpriors[7,2] = 1/Infpriors[7,2]^2 
  mdl = fitdist(outdfPr[,which(vnPr=="sigS")],"lnorm")
  Infpriors[8,] = as.numeric(mdl$estimate); Infpriors[8,2] = 1/Infpriors[8,2]^2 
  mdl = fitdist(outdfPr[,which(vnPr=="sigC")],"lnorm")
  Infpriors[9,] = as.numeric(mdl$estimate); Infpriors[9,2] = 1/Infpriors[9,2]^2   
  rm(outdfPr)
  rm(vnPr)
  rm(mdl)
}
# Select sub-set of data for analysis ----------------------------------------------
attach(BayesianData)
if (calendar_opt == 1){
  ii = which(year==Yearfocal & DayOfYear>=DayOfYear_strt & DayOfYear<=DayOfYear_end & TimeStepN>=TS_strt & TimeStepN<=TS_stop)
}else if(calendar_opt == 2){
  ii = which(((year==Yearfocal-1 & DayOfYear>=DayOfYear_strt) | (year==Yearfocal & DayOfYear<=DayOfYear_end)) & TimeStepN>=TS_strt & TimeStepN<=TS_stop)
}
ii = ii[seq(1,length(ii),subsamp)]
# Save sub-set of data for analysis to data frame "dat"
detach(BayesianData)
ncols = dim(BayesianData)[2]
dat = cbind(BayesianData[ii,1],BayesianData[ii,2:ncols])
rm('BayesianData')
attach(dat)
#

setwd(AnalysisFolder)

# Process data ------------------------------------------------------
maxTS = max(TimeStepN)
TS = TimeStepN
TS1 = parse_date_time(TimeStep1,c('%I:%M %p'))
print(paste0("Timestep 1 = ",format(TS1, format="%H:%M")))
TS1_txt = as.character(format(TS1, format="%H:%M"))
Wk = WeekN
Dy = DayN
ii = which(DayOfYear==min(DayOfYear))
Date1 = ymd(Date[ii[1]]-DayN[ii[1]]+1)
Calls = dat$hits
minutes = dat$minutes
NWeeks = max(Wk)
NTsteps = max(TS)
NDays = max(Dy)
NObs = length(Calls)
Year = dat$year
YearN = Year - min(Year) + 1
Nyrs = max(YearN)
Moon = 2*dat$Illu-1
# identify peak times, and Create index to Temporal matrix for peak times
pk = numeric(length=length(DayOfYear))
if (calendar_pk_opt == 1) {
  if (peaktimes_ref == 1){
    ii = which(DayOfYear>=peakdates_strt & DayOfYear<=peakdates_end & 
                 c(TimeStepN - SunsetTimeStepN)>=peaktimes_strt/TimeStepIntvl &
                 c(TimeStepN - SunsetTimeStepN)<=peaktimes_stop/TimeStepIntvl)
  }else if (peaktimes_ref == 2){ 
    ii = which(DayOfYear>=peakdates_strt & DayOfYear<=peakdates_end & 
                 c(TimeStepN-SunriseTimeStepN)>=peaktimes_strt/TimeStepIntvl &
                 c(TimeStepN-SunriseTimeStepN)<=peaktimes_stop/TimeStepIntvl)
  }else if (peaktimes_ref == 3){
    ii1 = which(DayOfYear>=peakdates_strt & DayOfYear<=peakdates_end & 
                  c(TimeStepN - SunsetTimeStepN)>=peaktimes_strt/TimeStepIntvl &
                  c(TimeStepN - SunsetTimeStepN)<=peaktimes_stop/TimeStepIntvl)
    ii2 = which(DayOfYear>=peakdates_strt & DayOfYear<=peakdates_end & 
                  c(TimeStepN-SunriseTimeStepN)>=peaktimes_strt2/TimeStepIntvl &
                  c(TimeStepN-SunriseTimeStepN)<=peaktimes_stop2/TimeStepIntvl)    
    ii = c(ii1,ii2)
  }
} else if (calendar_pk_opt == 2) {
  if (peaktimes_ref == 1){
    ii = which((DayOfYear>=peakdates_strt | DayOfYear<=peakdates_end) & 
                 c(TimeStepN - SunsetTimeStepN)>=peaktimes_strt/TimeStepIntvl &
                 c(TimeStepN - SunsetTimeStepN)<=peaktimes_stop/TimeStepIntvl)
  }else if (peaktimes_ref == 2){ 
    ii = which((DayOfYear>=peakdates_strt | DayOfYear<=peakdates_end) & 
                 c(TimeStepN-SunriseTimeStepN)>=peaktimes_strt/TimeStepIntvl &
                 c(TimeStepN-SunriseTimeStepN)<=peaktimes_stop/TimeStepIntvl)
  }else if (peaktimes_ref == 3){
    ii1 = which((DayOfYear>=peakdates_strt | DayOfYear<=peakdates_end) & 
                  c(TimeStepN - SunsetTimeStepN)>=peaktimes_strt/TimeStepIntvl &
                  c(TimeStepN - SunsetTimeStepN)<=peaktimes_stop/TimeStepIntvl)
    ii2 = which((DayOfYear>=peakdates_strt | DayOfYear<=peakdates_end) & 
                  c(TimeStepN-SunriseTimeStepN)>=peaktimes_strt2/TimeStepIntvl &
                  c(TimeStepN-SunriseTimeStepN)<=peaktimes_stop2/TimeStepIntvl)    
    ii = c(ii1,ii2)
  }
}
pk[ii] = 1
Temppeak = matrix(data=0,nrow = NWeeks, ncol = NTsteps)
for (i in 1:NWeeks) {
  for (j in 1:NTsteps) {
    ii = which(Wk==i & TS==j)
    if(sum(pk[ii])>0){
      Temppeak[i,j]=1
    }
  }
}
pkindx=which(Temppeak==1)
lngthpeak = length(pkindx)
TmprlMax = numeric(length=100)+6 # Ensures temporal matrix is 0-1
# maxpkWk = median(Wk[Calls>0.95*max(Calls[pk==1])])
# maxpkTS = median(TS[Calls>0.95*max(Calls[pk==1])])
# Temppeak[maxpkWk,maxpkTS]=1.01

# PLOT HEATMAP OF PEAK TIMES MATRIX
image(seq(1,NWeeks),seq(1,NTsteps),Temppeak, col = heat.colors(100),
      xlab="Week",ylab="Time Step", main="Peak Times")
# Create Site list and associated stats (unique Site/Year combos)
SPIDs = as.character(dat[,1])
SPIDc = factor(SPIDs)
if(exists("StrataName")){
  Strata = factor(StrataName)
}else{
  Strata = factor(Region)
}
Isl = factor(Island) 
Site = as.numeric(SPIDc)
tmp = unique(data.frame(SPIDc, Site, Strata, Isl))
Sitelist = tmp[order(tmp$Strata,tmp$Site),1:4];
NSite = dim(Sitelist); NSite = NSite[1]
Sitelist[1:NSite,5] = c(1:NSite)
colnames(Sitelist)[5] <- "Sitenum"
SiteN = Site
Strtweek = numeric(length=NSite)
Lastweek = numeric(length=NSite)
Numweeks = numeric(length=NSite)
for (ii in 1:NSite){
  i = which(Site==Sitelist$Site[ii])
  SiteN[i] = Sitelist$Sitenum[ii]
  Strtweek[ii] = min(WeekN[i])
  Lastweek[ii] = max(WeekN[i])
  Numweeks[ii] = Lastweek[ii] - Strtweek[ii] + 1
}
Stratalist = unique(data.frame(Sitelist$Strata,Sitelist$Isl))
Nstrata = dim(Stratalist)[1]; 
Stratalist[1:Nstrata,3] = c(1:Nstrata)
colnames(Stratalist)[1] <- "StratName"
colnames(Stratalist)[2] <- "IslName"
colnames(Stratalist)[3] <- "Stratnum"
strata = dat$Strata
stratnum = Sitelist$Sitenum
for (ii in 1:Nstrata){
  i = which(dat$StrataName==Stratalist$StratName[ii])
  strata[i] = Stratalist$Stratnum[ii]
  i = which(Sitelist$Strata==Stratalist$StratName[ii])
  stratnum[i] = Stratalist$Stratnum[ii]
}
Sitelist[1:NSite,6] = stratnum
colnames(Sitelist)[6] <- "Stratnum"

indxS1 = numeric()
indxS2 = numeric()
for (ii in 1:Nstrata){
  i = which(Sitelist$Stratnum==ii)
  indxS1[ii] = min(Sitelist$Sitenum[i])
  indxS2[ii] = max(Sitelist$Sitenum[i])
}

if (data_opt==2){
  # Load count data (datC) and process variables
  # NOTE: still need to edit this to make it generic,
  # including user input to identify "seasons"
  setwd(DataFolder)
  dfC = read.csv(file = Countsdatfile, header = TRUE, sep = ",")
  setwd(AnalysisFolder)
  SPIDsN = as.character(dfC$SPID)
  SPIDcN = factor(SPIDsN)
  StrataNC = factor(dfC$StrataName)
  YearNest = dfC$contract_year
  Densmeas = dfC$Density
  SiteNC = numeric()
  StratNC = numeric()
  DensObsNC = numeric()
  DensObsNCnoA = numeric()   
  SiteNCnoA = numeric()
  StratNCnoA = numeric()
  SitecNCnoA = character()
  cntrNC = 0
  cntrNCnoA = 0
  for (i in 1:length(SPIDcN)){
    tmpS = as.character(SPIDcN[i])
    tmpH = as.character(StrataNC[i])
    tmpY = YearNest[i]  
    ii = which(as.character(Stratalist$StratName)==tmpH)
    iii = which(as.character(Sitelist$SPIDc)==tmpS)
    if ((tmpY == Yearfocal & length(ii)>0 & length(iii)>0)==1){
      cntrNC = cntrNC+1
      SiteNC[cntrNC] = Sitelist$Sitenum[iii] 
      StratNC[cntrNC] = Sitelist$Stratnum[iii] 
      DensObsNC[cntrNC] = Densmeas[i]+.0001
    }else if ((tmpY == Yearfocal & length(ii)>0 & length(iii)==0)==1){
      cntrNCnoA = cntrNCnoA +1
      SiteNCnoA[cntrNCnoA] = cntrNCnoA
      SitecNCnoA[cntrNCnoA] = tmpS
      StratNCnoA[cntrNCnoA] = Stratalist$Stratnum[ii]
      DensObsNCnoA[cntrNCnoA] = Densmeas[i]
    }else{
    }
  }  
  Ncounts = cntrNC
  NSiteNoCall = cntrNCnoA
  # Next lines set the priors for the density-call rate coversion fxn
  SwCN = sort(unique(SiteNC))
  x = numeric(length = length(SwCN))
  y = numeric(length = length(SwCN))
  # adjustpk = mean(Calls[pk==1])/as.numeric(quantile(Calls[pk==1],.75)) 
  adjustpk = 0.95
  for (i in 1:length(SwCN)){
    iii = which(SiteNC==SwCN[i])
    x[i] = mean(DensObsNC[iii])
    iii = which(pk==1 & SiteN==SwCN[i])
    y[i] = mean(Calls[iii])/adjustpk
  }
  if (length(SwCN)>5){
    ii = which(is.na(y))
    if (length(ii)>0){
      x = x[-ii]
      y = y[-ii]
    }
    plot(x,y,xlab="Density",ylab="Peak Call Rate")
    a_start = max(y)/max(x)
    b_start = 1
    fxn = nls(y~a*x^b, start=list(a=a_start,b=b_start))
    s = seq(min(x),max(x),length.out= 100)
    lines(s, predict(fxn, list(x = s)), lty = 2, col = "red")
    summary(fxn)
    coeff = as.numeric(summary(fxn)$coefficients)
    fxnprior=c(max(1,coeff[1]),1/coeff[3]^2,coeff[2],1/coeff[4]^2)
  }else{
    fxnprior = c(100,.01,1,.01)
  }
}
# Get Acoustic Recording quality vars for QC function:
FS = flux_sensitive   # Flux sensitive variable as-is
LA = level_absolute+5 # Level absolute variable made positive by adding 5 
CL = log(1+click)     # Click variable re-scaled, log of click+1 
BU = log(1+burst)     # Burst variable re-scaled, log of burst+1 

detach(dat)

# Priors for Call Quality -------------------------------------------------------------------
# Evaluate Call Quality params using logistic regression
i = sample(length(pk),10000,replace = TRUE)

df = data.frame(hits=pmin(1,Calls[i]),FS = FS[i], LA = LA[i], CL = CL[i],BU = BU[i],
                FS2 = FS[i]^2, FS3 = FS[i]^3, LA2 = LA[i]^2, LA3 = LA[i]^3, FSLA = FS[i]*LA[i])
model <- glm(hits ~ FS + FS2 + FS3 + LA + BU + CL + FSLA,
             family=binomial(link='logit'),data=df)
summary(model)
df$prob = predict(model, newdata = df, type = "response")
plot(df$FS[df$hits==0],df$LA[df$hits==0],col="blue",ylim = c(0,4),xlim = c(0,100))
points(df$FS[df$hits==1],df$LA[df$hits==1],col="red",ylim = c(0,4),xlim = c(0,100))
newdat = df[1:800,]
Prob1 = matrix(nrow=20,ncol=20)
Prob2 = matrix(nrow=20,ncol=20)
cntr = 0
cntrC = 0
for(j in c(0.001,.03)){
  cntrx = 0
  cntrC = cntrC +1
  for(f in seq(10,100,length.out = 20)){
    cntrx = cntrx+1
    cntry = 0    
    for(l in seq(.5,4,length.out = 20)){
      cntry = cntry+1
      cntr = cntr+1
      newdat$hits[cntr]=0
      newdat$FS[cntr]=f
      newdat$FS2[cntr]=f^2
      newdat$FS3[cntr]=f^3
      newdat$LA[cntr]=l
      newdat$LA2[cntr]=l^2
      newdat$BU[cntr]=j
      newdat$CL[cntr]=.001
      newdat$FSLA[cntr]=f*l
      if (cntrC == 1){
        Prob1[cntrx,cntry] = predict(model, newdata = newdat[cntr,], type = "response")
      }else{
        Prob2[cntrx,cntry] = predict(model, newdata = newdat[cntr,], type = "response")
      }
    }
  }
}
thrdvar = 'Burst'
newdat$prob = predict(model, newdata = newdat, type = "response")
xx = seq(10,100,length.out = 20); yy = seq(.5,4,length.out = 20)
library(lattice)
levelplot(Prob1, data = NULL, aspect = "fill",
          xlim = c(0,100),ylim = c(0.5,4),
          row.values = xx, column.values = yy,
          xlab = 'Flux Sensitive', ylab = 'Level Absolute',
          main= paste0(thrdvar, '= low'))
levelplot(Prob2, data = NULL, aspect = "fill",
          xlim = c(0,100),ylim = c(0.5,4),
          row.values = xx, column.values = yy,
          xlab = 'Flux Sensitive', ylab = 'Level Absolute',
          main= paste0(thrdvar, '= high'))

# Extract coefficients and std erros as priors for Bayesian model
Bpar1 =  summary(model)$coefficients[, 1] #
newdat = data.frame(hits=pmin(1,Calls),FS = FS, LA = LA, CL = CL, BU = BU,
                    FS2 = FS^2, FS3 = FS^3, LA2 = LA^2, LA3 = LA^3, FSLA = FS*LA)
TMP = predict(model, newdata = newdat, type = "response")
# Select data with >25% the average chance of hits
Q = which(TMP>mean(newdat$hits)*.25)  # IF QC_opt = 0 USE THIS TO FILTER DATA
NObsQ = length(Q)
Bpar1[1] = Bpar1[1]+0.5 # Increase intercept such that max prob ~ 0.95
Bpar2 =  summary(model)$coefficients[, 2]
# Bpar2[1] = Bpar2[1]
Bpar2 = 1/Bpar2^2

# Prepare to run JAGS -----------------------------------------------------

set.seed(123)
# For 8 cores, set next lines to 8, 2750, 1500, 100 to get 10,000 reps
# Nsim = 2120      # 2750
# Nburnin = 1500  # 2000
# Nadapt = 100

# For parallel (comment out for serial)
cores<-detectCores()
cores = min(cores, Nchains)
cl <- makeCluster(cores[1])
registerDoParallel(cl)

# Set up data, inits and params structures for JAGS,
# Note: analysis depends on user options: data type, priors and QC filter or adjust
#
if (QC_opt==0){
  
  if (data_opt==1 & prior_opt==1){
    # Data: A named list of the objects needed by JAGS
    data <- list(Calls=Calls[Q],SiteN=SiteN[Q],Minutes=minutes[Q],
                 Wk=Wk[Q],TS=TS[Q],NObs=NObsQ,NSite=NSite, 
                 NWeeks=NWeeks,NTsteps=NTsteps,
                 moon=Moon[Q],Nstrata=Nstrata,strat=stratnum,
                 lngthpeak=lngthpeak,TP=Temppeak,
                 TmprlMax=TmprlMax,Numweeks=Numweeks,Strtweek=Strtweek,
                 indxS1=indxS1,indxS2=indxS2) 
    
    # Inits: Best to generate initial values using function
    inits <- function(){
      list(sigT=runif(1,0.3,0.6),sigN=runif(1,1,5),sigS=runif(1,2,10),sigW=runif(1,.1,.5),
           theta=runif(1,-.1,.1),Dispers=runif(1,.15,.25))
    }
    # List of parameters to monitor:
    params <- c('theta','sigT','sigS','sigN','sigW','Dispers','peakTemp',
                'C0','C','Cs','Csite','Temp','eps') # 
    #     NOTE: to save "E" matrix, need to run in serial not parallel
    #           due to memory limitations of parallel
    
    # Model to be run:
    modfile = 'Jags_calls_only_Q.jags'
  }else if(data_opt==1 & prior_opt==2){
    data <- list(Calls=Calls[Q],SiteN=SiteN[Q],Minutes=minutes[Q],
                 Wk=Wk[Q],TS=TS[Q],NObs=NObsQ,NSite=NSite, 
                 NWeeks=NWeeks,NTsteps=NTsteps,
                 moon=Moon[Q],Nstrata=Nstrata,strat=stratnum,
                 lngthpeak=lngthpeak,TP=Temppeak,
                 TmprlMax=TmprlMax,Numweeks=Numweeks,Strtweek=Strtweek,
                 indxS1=indxS1,indxS2=indxS2,
                 Infpriors=Infpriors) 
    
    # Inits: Best to generate initial values using function
    inits <- function(){
      list(Dispers=runif(1,.2,.25))
    }
    # List of parameters to monitor:
    params <- c('theta','sigT','sigN','sigS','sigW','Dispers','peakTemp',
                'C0','C','Cs','Csite','Temp','eps') # 
    #     NOTE: to save "E" matrix, need to run in serial not parallel
    #           due to memory limitations of parallel
    
    # Model to be run:
    modfile = 'Jags_calls_only_infoprior_Q.jags'  
  }else if(data_opt==2 & prior_opt==1){
    data <- list(Calls=Calls[Q],SiteN=SiteN[Q],Minutes=minutes[Q],
                 Wk=Wk[Q],TS=TS[Q],NObs=NObsQ,NSite=NSite, 
                 NWeeks=NWeeks,NTsteps=NTsteps,
                 moon=MoonQ,Nstrata=Nstrata,strat=stratnum,
                 Ncounts=Ncounts,NSiteNoCall=NSiteNoCall,
                 SiteNC=SiteNC,StratNCnoA=StratNCnoA,
                 Numweeks=Numweeks,Strtweek=Strtweek,
                 DensObsNC=DensObsNC,DensObsNCnoA=DensObsNCnoA,
                 indxS1=indxS1,indxS2=indxS2, fxnprior=fxnprior,
                 TP=Temppeak,lngthpeak=lngthpeak,TmprlMax=TmprlMax) 
    # Inits: Best to generate initial values using function
    inits <- function(){
      list(sigT=runif(1,0.3,0.6),sigN=runif(1,.1,.5),sigS=runif(1,.1,.5),
           sigD=runif(1,0.1,0.5),sigC=runif(1,.1,.2),sigW=runif(1,.1,.5),
           psi=runif(1,.6,.9),phi0=runif(1,80,100),
           theta=runif(1,-.1,.1),Dispers=runif(1,.15,.25))
    }
    # List of parameters to monitor:
    params <- c('theta','sigT','sigD','sigN','sigS','sigC', 'sigW', 
                'Dispers','peakTemp','psi','phi','Dstrat',
                'C0','C','Cs','Csite','Dens','DensN','DensA',
                'Temp','eps') # 
    #     NOTE: to save "E" matrix, need to run in serial not parallel
    #           due to memory limitations of parallel
    # Model to be run:
    modfile = 'Jags_calls_counts_Q.jags'
    #
  }else if(data_opt==2 & prior_opt==2){
    data <- list(Calls=Calls,SiteN=SiteN,Minutes=minutes,
                 Wk=Wk,TS=TS,NObs=NObs,NSite=NSite, 
                 NWeeks=NWeeks,NTsteps=NTsteps,
                 moon=Moon,Nstrata=Nstrata,strat=stratnum,
                 Ncounts=Ncounts,NSiteNoCall=NSiteNoCall,
                 SiteNC=SiteNC,StratNCnoA=StratNCnoA,
                 Numweeks=Numweeks,Strtweek=Strtweek,
                 DensObsNC=DensObsNC,DensObsNCnoA=DensObsNCnoA,
                 indxS1=indxS1,indxS2=indxS2, Infpriors=Infpriors,
                 TP=Temppeak,lngthpeak=lngthpeak,TmprlMax=TmprlMax) 
    # Inits: Best to generate initial values using function
    inits <- function(){
      list(Dispers=runif(1,.2,.25))
    }
    # List of parameters to monitor:
    params <- c('theta','sigT','sigD','sigN','sigS','sigC','sigW',  
                'Dispers','peakTemp','psi','phi','Dstrat',
                'C0','C','Cs','Csite','Dens','DensN','DensA',
                'Temp','eps') # 
    #     NOTE: to save "E" matrix, need to run in serial not parallel
    #           due to memory limitations of parallel
    # Model to be run:
    modfile = 'Jags_calls_counts_infoprior_Q.jags'  
  }
  
}else{
  
  if (data_opt==1 & prior_opt==1){
    # Data: A named list of the objects needed by JAGS
    data <- list(Calls=Calls,SiteN=SiteN,Minutes=minutes,
                 Wk=Wk,TS=TS,NObs=NObs,NSite=NSite, 
                 NWeeks=NWeeks,NTsteps=NTsteps,
                 moon=Moon,Nstrata=Nstrata,strat=stratnum,
                 lngthpeak=lngthpeak,TP=Temppeak,
                 FS=FS,LA=LA,CL=CL,BU=BU,Bpar1=Bpar1,
                 Bpar2=Bpar2,TmprlMax=TmprlMax,
                 Numweeks=Numweeks,Strtweek=Strtweek,
                 indxS1=indxS1,indxS2=indxS2) 
    
    # Inits: Best to generate initial values using function
    inits <- function(){
      list(sigT=runif(1,0.3,0.6),sigN=runif(1,1,5),sigS=runif(1,2,10),sigW=runif(1,.1,.5),
           theta=runif(1,-.1,.1),Dispers=runif(1,.15,.25))
    }
    # List of parameters to monitor:
    params <- c('theta','sigT','sigS','sigN','sigW','Dispers','peakTemp',
                'C0','C','Cs','Csite','B','Temp','eps') # 
    #     NOTE: to save "E" matrix, need to run in serial not parallel
    #           due to memory limitations of parallel
    
    # Model to be run:
    modfile = 'Jags_calls_only.jags'
  }else if(data_opt==1 & prior_opt==2){
    data <- list(Calls=Calls,SiteN=SiteN,Minutes=minutes,
                 Wk=Wk,TS=TS,NObs=NObs,NSite=NSite, 
                 NWeeks=NWeeks,NTsteps=NTsteps,
                 moon=Moon,Nstrata=Nstrata,strat=stratnum,
                 lngthpeak=lngthpeak,TP=Temppeak,
                 FS=FS,LA=LA,CL=CL,BU=BU,Bpar1=Bpar1,
                 Bpar2=Bpar2,TmprlMax=TmprlMax,
                 indxS1=indxS1,indxS2=indxS2,
                 Numweeks=Numweeks,Strtweek=Strtweek,
                 Infpriors=Infpriors) 
    
    # Inits: Best to generate initial values using function
    inits <- function(){
      list(Dispers=runif(1,.2,.25))
    }
    # List of parameters to monitor:
    params <- c('theta','sigT','sigN','sigS','sigW','Dispers','peakTemp',
                'C0','C','Cs','Csite','B','Temp','eps') # 
    #     NOTE: to save "E" matrix, need to run in serial not parallel
    #           due to memory limitations of parallel
    
    # Model to be run:
    modfile = 'Jags_calls_only_infoprior.jags'  
  }else if(data_opt==2 & prior_opt==1){
    data <- list(Calls=Calls,SiteN=SiteN,Minutes=minutes,
                 Wk=Wk,TS=TS,NObs=NObs,NSite=NSite, 
                 NWeeks=NWeeks,NTsteps=NTsteps,
                 moon=Moon,Nstrata=Nstrata,strat=stratnum,
                 FS=FS,LA=LA,CL=CL,BU=BU,Bpar1=Bpar1,Bpar2=Bpar2,
                 Ncounts=Ncounts,NSiteNoCall=NSiteNoCall,
                 SiteNC=SiteNC,StratNCnoA=StratNCnoA,
                 Numweeks=Numweeks,Strtweek=Strtweek,
                 DensObsNC=DensObsNC,DensObsNCnoA=DensObsNCnoA,
                 indxS1=indxS1,indxS2=indxS2, fxnprior=fxnprior,
                 TP=Temppeak,lngthpeak=lngthpeak,TmprlMax=TmprlMax) 
    # Inits: Best to generate initial values using function
    inits <- function(){
      list(sigT=runif(1,0.3,0.6),sigN=runif(1,.1,.5),sigS=runif(1,.1,.5),
           sigD=runif(1,0.1,0.5),sigC=runif(1,.1,.2),sigW=runif(1,.1,.5),
           psi=runif(1,.6,.9),phi0=runif(1,80,100),
           theta=runif(1,-.1,.1),Dispers=runif(1,.15,.25))
    }
    # List of parameters to monitor:
    params <- c('theta','sigT','sigD','sigN','sigS','sigC','sigW',  
                'Dispers','peakTemp','psi','phi','Dstrat',
                'C0','C','Cs','Csite','Dens','DensN','DensA',
                'B','Temp','eps') # 
    #     NOTE: to save "E" matrix, need to run in serial not parallel
    #           due to memory limitations of parallel
    # Model to be run:
    modfile = 'Jags_calls_counts.jags'
    #
  }else if(data_opt==2 & prior_opt==2){
    data <- list(Calls=Calls,SiteN=SiteN,Minutes=minutes,
                 Wk=Wk,TS=TS,NObs=NObs,NSite=NSite, 
                 NWeeks=NWeeks,NTsteps=NTsteps,
                 moon=Moon,Nstrata=Nstrata,strat=stratnum,
                 FS=FS,LA=LA,CL=CL,BU=BU,Bpar1=Bpar1,Bpar2=Bpar2,
                 Ncounts=Ncounts,NSiteNoCall=NSiteNoCall,
                 SiteNC=SiteNC,StratNCnoA=StratNCnoA,
                 Numweeks=Numweeks,Strtweek=Strtweek,
                 DensObsNC=DensObsNC,DensObsNCnoA=DensObsNCnoA,
                 indxS1=indxS1,indxS2=indxS2, Infpriors=Infpriors,
                 TP=Temppeak,lngthpeak=lngthpeak,TmprlMax=TmprlMax) 
    # Inits: Best to generate initial values using function
    inits <- function(){
      list(Dispers=runif(1,.2,.25))
    }
    # List of parameters to monitor:
    params <- c('theta','sigT','sigD','sigN','sigS','sigC','sigW',  
                'Dispers','peakTemp','psi','phi','Dstrat',
                'C0','C','Cs','Csite','Dens','DensN','DensA',
                'B','Temp','eps') # 
    #     NOTE: to save "E" matrix, need to run in serial not parallel
    #           due to memory limitations of parallel
    # Model to be run:
    modfile = 'Jags_calls_counts_infoprior.jags'  
  }  
  
}
  
# Nsim =  1000  # Total # MCMS sims: Actual saved reps = (Nsim-Nburnin) * (num Cores)
# Nburnin =  500  # Number of burn-in reps: Actual reps = (Nsim-Nburnin) * (num Cores)
# Nadapt =  100  # Number of adapting reps, default 100

# Run JAGS ----------------------------------------------------------------

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
# 
# rhat = gelman.diag(out); rhat = data.frame(rhat$psrf);
# Deviance = s_stats["deviance",1]
# Vardev = s_stats["deviance",2]^2; pd = Vardev/2;
# DIC = Deviance + 2*pd
save(list = ls(all.names = TRUE),file=SaveResults)
