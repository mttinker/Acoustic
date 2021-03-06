
# Load packages ------------------------------------------------------------------
library(readxl)
library(lubridate)
library(stringr)
library(gtools)
library(stats)
library(coda)
library(gdata)
library(lattice)
library(rjags)
library(jagsUI)
library(parallel)
library(doParallel)
library(fitdistrplus)
library(ggplot2)
library(mcmcplots)
# install.packages("dplyr", dependencies = TRUE)
# library(dplyr)

# LOAD RAW DATA --------------------------------------------------------------------
setwd(DataFolder)
dfC = read.csv(file = loaddat, header = TRUE, sep = ",")
dfA = read.csv(file = loadAdat, header = TRUE, sep = ",")
setwd(AnalysisFolder)
SPIDnc = as.character(dfC$SPID)
SPIDnf = factor(SPIDnc)
StrataNC = as.character(dfC$StrataName)
StrataNCf = factor(StrataNC)
YearNest = dfC$contract_year
Densmeas = dfC$Density
Monthmeas = month(parse_date_time(as.character(dfC$Count_Date),"mdy"))
Seasmeas = Seasondefine[Monthmeas]
Radiusmeas = dfC$Density_Radius
dfCounts <- data.frame(Year=numeric(),
                 Site=character(), 
                 SiteNC=numeric(), 
                 Radius = numeric(), 
                 Season = numeric(), 
                 DensObsNC = numeric(),
                 stringsAsFactors=FALSE) 
dfCalls <- data.frame(Year=numeric(),
                 Site=character(), 
                 SiteN=numeric(), 
                 CR = numeric(),
                 stringsAsFactors=FALSE) 
Vvals = numeric()
NCmn =  numeric()
CRmn =  numeric()
Yrmn = numeric()
Nst = 0
Nyrs = length(loadfiles)
for (i in 1:Nyrs){
  attach(loadfiles[i]); 
  outdf <- outdf;
  vn <- vn
  Stratalist <- Stratalist
  Sitelist <- Sitelist
  Yearfocal <- Yearfocal
  detach(paste0('file:',loadfiles[i]),character.only = TRUE)
  for (s in 1:length(Sitelist$Sitenum)){
    ii = Sitelist$Sitenum[s]
    j = which(YearNest==Yearfocal & StrataNC==as.character(Sitelist$Strata[s]) & 
                SPIDnc==as.character(Sitelist$SPIDc[s]))
    if(length(j)>0  & !(Species == "WTSH" & Yearfocal==2016 & as.character(Sitelist$SPIDc[s])== "NW29")){  # & !(Yearfocal==2016 & as.character(Sitelist$SPIDc[s])== "NW29")
      Nst = Nst+1
      # Sample Call rate (peak period for standardization)
      samptmp = sample(outdf[,which(vn==paste0('Cs[',ii,']'))],simsamp)
      meanCR = mean(samptmp)
      for (jj in 1:length(j)){
        dfCounts = rbind(dfCounts,data.frame(Year=Yearfocal,
                                             Site=as.character(Sitelist$SPIDc[s]), 
                                             SiteNC=Nst, 
                                             Season = Seasmeas[j[jj]],
                                             Radius = Radiusmeas[j[jj]],
                                             DensObsNC = Densmeas[j[jj]],
                                             CRmean = meanCR))
        NCmn[Nst] = mean(Densmeas[j[jj]])
        Yrmn[Nst] = Yearfocal
      }
      
      dfCalls = rbind(dfCalls,data.frame(Year=Yearfocal,
                                         Site=as.character(Sitelist$SPIDc[1]), 
                                         SiteN=Nst, 
                                         CR = samptmp))
      Vvals[Nst] = var(samptmp)
      CRmn[Nst] = mean(samptmp)
    }
  }
  rm(outdf)
  rm(Stratalist)
  rm(Sitelist)
  rm(Yearfocal)
  rm(vn)
}
NSite = Nst
NObs = dim(dfCalls)[1]
Ncounts = dim(dfCounts)[1]
Year = dfCounts$Year - min(dfCounts$Year) + 1
NYrs = max(Year)
Radius = dfCounts$Radius/10
NRads = max(Radius)
NSeas = max(dfCounts$Season)
x = CRmn
y = NCmn
#x = NCmn
#y = CRmn
plot(x,y,xlab="Call Rate",ylab="Nest Count Density")
# a_start = .5*max(y)/max(x)
# b_start = .25
fxn = nls(y~a*x^b, start=list(a=0.5,b=.2))
#sigmoid = function(params, x) {
#  params[1] / (1 + exp(-params[2] * (x - params[3])))
#}
#fitmodel <- nls(y~a/(1 + exp(-b * (x-c))), start=list(a=.1,b=.1,c=.5))
# fxn = lm(y~log(x))
# fxn = lm(y~x)
xii = seq(min(x),max(x),length.out= 100)
lines(xii, predict(fxn, list(x = xii)), lty = 2, col = "red")
# lines(s, f_start, lty = 2, col = "purple")
summary(fxn)
coeff = as.numeric(summary(fxn)$coefficients)
fxnprior=c(max(1,coeff[1]),1/coeff[3]^2,coeff[2],1/coeff[4]^2)
# fxnprior=c(max(1,a_start),1/ (a_start*.5)^2,b_start,1/(b_start*1)^2)
dfCC = data.frame(CallRate = CRmn, NestDens = NCmn)
ggplot(dfCC, aes(x=CallRate,y=NestDens)) + 
  geom_point() +
  stat_smooth(method = "lm", col = "red", formula = 'y~x') 

ggplot(dfCC, aes(x=CallRate,y=NestDens)) + 
  geom_point() +
  stat_smooth(method = 'nls', formula = 'y~a*x^b', start = list(a = .01,b=1), se=FALSE) 

# Set up for JAGS -----------------------------------------------------
set.seed(123)
# For parallel (comment out for serial)
cores<-detectCores()
cores = min(cores, Nchains)
cl <- makeCluster(cores[1])
registerDoParallel(cl)
# Data structure
data <- list(NObs=NObs,Ncounts=Ncounts,NSite=NSite,NYrs=NYrs,
             DensObsNC=pmax(dfCounts$DensObsNC,.001),CR=dfCalls$CR,
             SiteNC=dfCounts$SiteNC,SiteN=dfCalls$SiteN,
             Year=Year,Radius=Radius,Season=dfCounts$Season,
             NRads=NRads,NSeas=NSeas,Vvals=Vvals)
# Inits: Best to generate initial values using function
inits <- function(){
  list(Beta=runif(1,.001,.01))
}
# List of parameters to monitor:
params <- c('sigC','sigD','sigR','sigS','sigY','Radeff','Seaseff','Yeareff',
            'alpha','Beta','Csite','DensN','Dens','Err') 
# Model to be run:
modfile = 'Jags_convert_calls_counts.jags'
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
for (i in 1:(Np-4)){
  parnm = params[i]
  traplot(out,parnm)
}
for (i in 1:5){
  parnm = params[i]
  denplot(out,parnm,ci=.9,collapse = TRUE)
}
for (i in 9:10){
  parnm = params[i]
  denplot(out,parnm,ci=.9,collapse = TRUE)
}

denplot(out,"Radeff[2]",ci=.9,collapse = TRUE)
denplot(out,"Seaseff[2]",ci=.9,collapse = TRUE)
for (y in 2:NYrs){
  denplot(out,paste0("Yeareff[",y,"]"),ci=.9,collapse = TRUE)
}

# Compute R2 ------------------------------------------------------------
R2_sims = numeric(length = nrep)
ypred = matrix(nrow=nrep,ncol = NSite)
err = matrix(nrow=nrep,ncol = NSite)
for (i in 1:NSite){
    ypred[,i] = outdf[,vn==paste0("Dens[",i,"]")]
    err[,i] = outdf[,vn==paste0("Err[",i,"]")]
}
for (r in 1:Totalreps){
  R2_sims[r] = var(ypred[r,])/(var(ypred[r,])+var(err[r,]))
}
R2 = median(R2_sims)
plot(density(R2_sims),main = paste0("Function R2 value, ",format(R2,digits=3))
     ,xlab = "Bayesian R2",ylab = "Posterior density")
abline(v=R2)
# Function plots --------------------------------------------------------
xx =  s_stats[which(startsWith(vn,'Csite')),1]
yy = s_stats[which(startsWith(vn,'DensN')),1]
#fx = s_stats[which(startsWith(vn,'Dens[')),1]
xxL =  s_quantiles[which(startsWith(vn,'Csite')),1]
yyL = s_quantiles[which(startsWith(vn,'DensN')),1]
#fxL = s_quantiles[which(startsWith(vn,'Dens[')),1]
xxH =  s_quantiles[which(startsWith(vn,'Csite')),5]
yyH = s_quantiles[which(startsWith(vn,'DensN')),5]
#fxH = s_quantiles[which(startsWith(vn,'Dens[')),5]

a_post = outdf$alpha
b_post = outdf$Beta

alpha = s_stats[which(vn=='alpha'),1]
Beta = s_stats[which(vn=='Beta'),1]  
sigD = s_stats[which(vn=='sigD'),1]
Convertfxn = list(means = c(alpha,Beta), covmat = cov(cbind(a_post,b_post)))
Vd = sigD^2
xi = seq(min(CRmn),max(CRmn),length.out = 100)
# yi = alpha+xi*Beta
yi = alpha*xi^Beta
# yi = pmax(0.001,alpha+log(xi)*Beta)
fxL = numeric(length=100)
fxH = numeric(length=100)
for(i in 1:100){
  # fxL[i] = quantile(a_post+xi[i]*b_post ,0.025)
  # fxH[i] = quantile(a_post+xi[i]*b_post ,0.975)
  fxL[i] = quantile(a_post*xi[i]^b_post ,0.025)
  fxH[i] = quantile(a_post*xi[i]^b_post ,0.975)
  # fxL[i] = pmax(0,quantile(a_post+log(xi[i])*b_post ,0.025))
  # fxH[i] = pmax(0,quantile(a_post+log(xi[i])*b_post ,0.975))
}
mu = log(yi/sqrt(1+Vd/yi^2))
sig=sqrt(log(1+Vd/yi^2))
pxL=qlnorm(0.05,mu,sig)
pxH=qlnorm(0.95,mu,sig)

dfSites = data.frame(xx=xx,yy=yy,xxL=xxL,xxH=xxH,yyL=yyL,yyH=yyH,xobs=CRmn,
                     yobs=NCmn)
dfFxn = data.frame(xi=xi,yi=yi,fxL=fxL,fxH=fxH,pxL=pxL,pxH=pxH)

Convertplot = ggplot()+
  geom_ribbon(data=dfFxn,aes(ymin=pxL, ymax=pxH, x=xi), alpha = 0.15)+
  geom_ribbon(data=dfFxn,aes(ymin=fxL, ymax=fxH, x=xi), alpha = 0.25)+
  geom_line(data=dfFxn,aes(x=xi, y=yi))+
  geom_point(data=dfSites,aes(x=xx,y=yy,size=1))+
  geom_point(data=dfCounts,aes(x=CRmean,y=DensObsNC, colour = "red"))+
  geom_errorbar(data=dfSites,aes(x=xx,ymax=yyH,ymin=yyL,width=0))+
  geom_errorbarh(data=dfSites,aes(x=xx,y=yy,xmax=xxH,xmin=xxL))+
  xlab("Call Rate") + ylab("Nest Density") +
  theme(legend.position='none')
print(Convertplot)
save(list = ls(all.names = TRUE),file=SaveResults)
