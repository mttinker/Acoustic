# Program Summary ---------------------------------------------------------
# Script to examine analyis results from a Bayesian Analysis of
#  acoustic data on seabird calls
#
simsamp = 1000;
# 
# Load necessary libraries (may need to install some of these)
library(coda)
library(mcmcplots)
library(rjags)
library(Matrix)
library(matrixStats)
library(mvtnorm)
library(Hmisc)
library(ggplot2)
library(lubridate)
library(gdata)
library(gtools)
library(lattice)
library(reshape2)
library(grid)
# Load Data ---------------------------------------------------------------
load(loadfile1)
dfArea = read.csv(file = Areasdatfile, header = TRUE, sep = ",")
# Specify year to analyze
FYear = Yearfocal
startday = DayOfYear_strt
pathout = paste0(ResultsFolder,"/",species,"_summaries/")

attach(loadfile2); 
Convfxn = Convertfxn
Convplot = Convertplot
detach(paste0('file:',loadfile2),character.only = TRUE)

# Diagnostic Plots ------------------------------------------------------------------

pfp = c(which(params=='theta'),which(startsWith(params,'sig')),which(params=='Dispers'),
        which(params=='peakTemp'),which(params=='C0'),which(params=='C'),which(params=='psi'),
        which(params=='phi'),which(params=='Dstrat'))
            
for (i in pfp){
  parnm = params[i]
  traplot(out,parnm)
  denplot(out,parnm,ci=.9,collapse = TRUE)
}

# Caterpiller plot for Strata  ----------------------------------------
stratparnum = which(params=='C')
labelsstrat = Stratalist$StratName
caterplot(out,params[stratparnum],denstrip = FALSE, reorder=FALSE,
          quantiles=list(outer=c(0.025,0.975),inner=c(0.1666,0.8333)),lwd=c(.1,4),
          labels=labelsstrat, labels.loc = 'above',las = 0, cex.labels = .8)
          title(main = "Mean Call Rate by Strata", font.main = 4)
#
# if (data_opt==2){
#   stratparnum = which(params=='Dstrat')
#   labelsstrat = Stratalist$StratName
#   caterplot(out,params[stratparnum],denstrip = FALSE, reorder=FALSE,
#             quantiles=list(outer=c(0.025,0.975),inner=c(0.1666,0.8333)),lwd=c(.1,4),
#             labels=labelsstrat, labels.loc = 'above',las = 0, cex.labels = .8)
#   title(main = "Mean Density by Strata", font.main = 4)
# }          
#           
#          
# Caterpiller plot for sites, by strata  ----------------------------------------
siteparnum = which(params=='Cs')
stratlist <- Stratalist$StratName
for (sn in 1:length(stratlist)){
  Stratfocal = stratlist[sn]
  ii = which(Sitelist$Strata==Stratfocal)
  tmplabs = as.character(Sitelist$SPIDc[ii])
  tmplist = paste0(params[siteparnum],"[",ii[1],"]")
  for (i in 2:length(ii)){
    tmplist = c(tmplist,paste0(params[siteparnum],"[",ii[i],"]"))
  }
  caterplot(out,tmplist,denstrip = FALSE, reorder=FALSE,
            quantiles=list(outer=c(0.025,0.975),inner=c(0.1666,0.8333)),lwd=c(.1,4),
            labels=tmplabs, labels.loc = 'above',las = 0, cex.labels = .8)
  title(main = paste0("Mean Call Rate by Site, ",Stratfocal,", ", as.character(Yearfocal)), font.main = 4)
}

# QC call detction probability -----------------------------------------
if (QC_opt==1){
  B = s_stats$Mean[which(startsWith(vn,"B["))]
  intcpt = numeric(length = 800)
  FS1 = intcpt; FS2 = intcpt; FS3 = intcpt; CL1 = intcpt; 
  LA1 = intcpt; BU = intcpt; FSLA = intcpt; Prob = intcpt; 
  intcpt = 1+intcpt 
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
        FS1[cntr]=f
        FS2[cntr]=f^2
        FS3[cntr]=f^3
        LA1[cntr]=l
        BU[cntr]=j
        CL1[cntr]=.001
        FSLA[cntr]=f*l
        Prob[cntr] = min(1,.25+inv.logit(B[1]+B[2]*FS1[cntr]+B[3]*FS2[cntr]+B[4]*FS3[cntr]+
                                           B[5]*LA1[cntr]+B[6]*BU[cntr]+B[7]*CL1[cntr]+B[8]*FSLA[cntr]))
        if (cntrC == 1){
          Prob1[cntrx,cntry] = Prob[cntr]
        }else{
          Prob2[cntrx,cntry] = Prob[cntr]
        }
      }
    }
  }
  xx = seq(10,100,length.out = 20); yy = seq(.5,4,length.out = 20)
  plt = levelplot(Prob1, data = NULL, aspect = "fill",
                  xlim = c(10,100),ylim = c(0.5,4),
                  row.values = xx, column.values = yy,
                  xlab = 'Flux Sensitive', ylab = 'Level Absolute+5',
                  main="Call Detection Prob, log(1+Burst)=0.001")
  print(plt)
  
  plt = levelplot(Prob2, data = NULL, aspect = "fill",
                  xlim = c(10,100),ylim = c(0.5,4),
                  row.values = xx, column.values = yy,
                  xlab = 'Flux Sensitive', ylab = 'Level Absolute+5',
                  main="Call Detection Prob, log(1+Burst)=0.03")
  print(plt)
}
# Temporal Matrix -----------------------------------
#
# Generate the "Temporal Effect" matrix from mean values
TemporalMn = matrix(0,nrow=NWeeks,ncol=NTsteps)
for (i in 1:NWeeks) {
  for (j in 1:NTsteps) {
    # TemporalMn[i,j] = mean(outdf[,which(vn==paste0('Temporal[',i,',',j,']'))])
    TemporalMn[i,j] = s_stats[which(vn==paste0('Temp[',i,',',j,']')),"Mean"]
  }
}
# Plot heat map of the Temporal effect:
Ntticks = 8
TS_ticks = signif(seq(1,NTsteps,len=Ntticks),3)
timelabels = as.character(format(TS1, format="%H:%M"))
for (t in 2:Ntticks){
  timelabels = c(timelabels,
                 as.character(format(TS1+(TS_ticks[t]-1)*15*60, format="%H:%M")))
}
Startdate = Date1
datevec = seq(Startdate+7, Startdate+NDays-1, by="3 weeks")
datelabels = format.Date(datevec,"%b")
# FortNtlabels = c('')

# Plot Image with color bar
layout(matrix(data=c(1,2), nrow=1, ncol=2), widths=c(5,1), heights=c(1,1))
par(mar = c(3,5,2.5,2))
image(seq(1,NWeeks),seq(1,NTsteps),TemporalMn, col = heat.colors(100),
      xlab="Month",ylab="", yaxt="n", xaxt="n", cex.lab=.8)
title(main = "Temporal Effects on Call Rate", font.main = 4)
# add categorical labels to y-axis
axis(1, at=seq(2,NWeeks,by=21/7), labels=datevec, las=HORIZONTAL<-1, cex.axis=0.6)
axis(2, at=TS_ticks, labels=timelabels, las=HORIZONTAL<-1, cex.axis=0.6)
title(ylab = "Time of Day", line = 2.5, cex.lab=.8)

ColorLevels <- seq(0, 1, length=length(heat.colors(100)))
par(mar = c(3,2.5,2.5,2))
image(1, ColorLevels,
      matrix(data=ColorLevels, ncol=length(ColorLevels),nrow=1),
      col = heat.colors(100),
      xlab="",ylab="",
      xaxt="n", cex.axis=0.8, cex.lab=.8)
title(main = "Color Scale", font.main = 1)
title(xlab = "Proportion max rate", line = .5, cex.lab=.8)
layout(1)

# Plot selected sites over focal time/date period ------------------

D1 = parse_date_time(paste0(Yearfocal," ", DateFocalStrt), "%y%m%d")
D2 = parse_date_time(paste0(Yearfocal," ", DateFocalStop), "%y%m%d")
if (Time_ref == 1){
  iii = which(dat$Date>=D1 & dat$Date<=D2 & 
               c(dat$TimeStepN - dat$SunsetTimeStepN)>=TimeFocalStrt/TimeStepIntvl &
               c(dat$TimeStepN - dat$SunsetTimeStepN)<=TimeFocalStop/TimeStepIntvl)
}else if (Time_ref == 2){ 
  iii = which(dat$Date>=D1 & dat$Date<=D2 & 
               c(dat$TimeStepN - dat$SunriseTimeStepN)>=TimeFocalStrt/TimeStepIntvl &
               c(dat$TimeStepN - dat$SunriseTimeStepN)<=TimeFocalStop/TimeStepIntvl)
}

jj = which(vn=='sigW') 
if (length(jj)==1){
  sigW = s_stats[jj,"Mean"]
}else{
  sigW = 0.25
}

tmp1 = Wk[iii]
tmp2 = TS[iii]
tmp3 = Moon[iii]
Moonsmp = numeric()
Wksmp = numeric()
TSsmp = numeric()
for (w in seq(min(tmp1), max(tmp1))){
  tm1 = min(tmp2[tmp1==w]); tm2 = max(tmp2[tmp1==w])
  m = mean(tmp3[tmp1==w])
  for (t in seq(tm1,tm2)){
    Wksmp = c(Wksmp, w)
    TSsmp = c(TSsmp, t)
    Moonsmp = c(Moonsmp, m)
  }
}
Nsmp = length(Moonsmp)
post = outdf[sample(nrow(outdf),simsamp,replace = TRUE),]
predmat = matrix(nrow = simsamp,ncol = Nsmp)
sitemns = matrix(nrow = simsamp,ncol = Nfocalsites)
Tmpeff = matrix(nrow = simsamp,ncol = Nsmp)
for (j in 1:Nsmp){
  Tmpeff[,j] = post[,which(vn==paste0("Temp[",Wksmp[j],",",TSsmp[j],"]"))]
}

Thta = post[,which(vn=="theta")]
for (i in 1:Nfocalsites){
  sn = which(Sitelist$SPIDc==Site_focal[i])
  s = Sitelist$Sitenum[sn]
  Ceff = post[,which(vn==paste0("Csite[",s,"]"))]
  for (j in 1:Nsmp){
    jj = which(vn==paste0("eps[", Wksmp[j], ",", s,"]"))
    if(length(jj)==1){
      Ceps = post[,jj]
    }else{
      Ceps = rlnorm(simsamp, log(1/sqrt(1+sigW^2)), sqrt(log(1+sigW^2)))
    }
    predmat[,j] = Ceff*Tmpeff[,j]*exp(Thta*Moonsmp[j])*Ceps
  }
  sitemns[,i] = rowMeans(predmat)
}
dfFSites = data.frame(Site = Site_focal[1], Mean = sitemns[,1])
for (i in 2:Nfocalsites){
  dfFSites = rbind(dfFSites, 
                   data.frame(Site = Site_focal[i], Mean = sitemns[,i]))
}

plt1 = ggplot(dfFSites, aes(x = Site, y = Mean)) +
                geom_boxplot(fill = "light blue", colour = "black",
                             alpha = 0.7) +
                scale_y_continuous(name = "Mean Expected Call Rate") +
                scale_x_discrete(name = "Site") +
                ggtitle("Expected Call Rates, Select Sites in Focal Period")

print(plt1)

# Plot Estimated Call rate and abundance by strata and Island ----------------------
# Call rate vs Nest Counts
print(Convplot)
# Abundance calculations using conversion function (with uncertainty)
AB = rmvnorm(n=simsamp, mean=Convfxn$means, sigma=Convfxn$covmat)
alph = AB[,1]; Beta = AB[,2]; rm(AB)
repvec = seq(1,simsamp)
dfStrat = data.frame(Rep = repvec,Strata = Stratalist$StratName[1],
                     Island = Stratalist$IslName[1],
                     CallRate = post[,vn==paste0('C[', Stratalist$Stratnum[1],']')],
                     Density = alph + Beta*post[,vn==paste0('C[', Stratalist$Stratnum[1],']')],
                     Area = dfArea$Area[as.character(dfArea$StrataName)==as.character(Stratalist$StratName[1])])
dfStrat$Total = dfStrat$Density*dfStrat$Area
for (i in 2:Nstrata){
  tmp = data.frame(Rep = repvec,Strata = Stratalist$StratName[i],
                   Island = Stratalist$IslName[i],
                   CallRate = post[,vn==paste0('C[', Stratalist$Stratnum[i],']')],
                   Density = alph + Beta*post[,vn==paste0('C[', Stratalist$Stratnum[i],']')],
                   Area = dfArea$Area[as.character(dfArea$StrataName)==as.character(Stratalist$StratName[i])])
  tmp$Total = tmp$Density*tmp$Area
  dfStrat = rbind(dfStrat,tmp)
}

plt2 = ggplot(dfStrat, aes(x=Strata, y=CallRate)) +
  geom_boxplot(fill = "light blue", colour = "black",
               alpha = 0.7) +
  scale_y_continuous(name = "Mean Expected Call Rate") +
  scale_x_discrete(name = "Strata") +
  ggtitle(paste0("Estimated Peak Call Rates by Strata, ", Yearfocal)) +
  theme(axis.text.x=element_text(angle=45,hjust=1)) 
print(plt2)

plt3 = ggplot(dfStrat, aes(x=Strata, y=Density)) +
  geom_boxplot(fill = "light blue", colour = "black",
               alpha = 0.7) +
  # coord_trans(y = "log2") +
  coord_cartesian(ylim = c(0,0.5)) +
  scale_y_continuous(name = "Mean Expected Density") +
  scale_x_discrete(name = "Strata") +
  ggtitle(paste0("Estimated Density by Strata, ", Yearfocal)) +
  theme(axis.text.x=element_text(angle=45,hjust=1)) 
print(plt3)

plt4 = ggplot(dfStrat, aes(x=Strata, y=Total)) +
  geom_boxplot(fill = "light blue", colour = "black",
               alpha = 0.7) +
  # coord_trans(y = "log2") +
  coord_cartesian(ylim = c(0,1.05*as.numeric(quantile(dfStrat$Total,.99)))) +
  scale_y_continuous(name = "Mean Expected Abundance",labels = scales::comma) +
  scale_x_discrete(name = "Strata") +
  ggtitle(paste0("Estimated Abundance by Strata, ", Yearfocal)) +
  theme(axis.text.x=element_text(angle=45,hjust=1)) 
print(plt4)

NumI = length(unique(Stratalist$IslName))
dfIsl = dcast(dfStrat[,c(1,2,3,7)],Island + Rep ~ Strata)
dfIsl$Total = apply(dfIsl[3:dim(dfIsl)[2]],1,sum,na.rm=TRUE)
plt5 = ggplot(dfIsl, aes(x=Island, y=Total)) +
  geom_boxplot(fill = "light blue", colour = "black",
               alpha = 0.7) +
  # coord_trans(y = "log2") +
  coord_cartesian(ylim = c(0,1.05*as.numeric(quantile(dfIsl$Total,.985)))) +
  scale_y_continuous(name = "Mean Expected Abundance",labels = scales::comma) +
  scale_x_discrete(name = "Island") +
  ggtitle(paste0("Estimated Abundance by Islands, ", Yearfocal)) +
  theme(axis.text.x=element_text(angle=45,hjust=1)) 
print(plt5)