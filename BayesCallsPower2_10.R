# Program Summary ---------------------------------------------------------
# Script to run Power Analysis to determine ability to detect trends 
#  in seabird colonies using data from Acoustic monitors and nest counts, 
#  based on results from existing analyses
# NOTE: this script must be called up from "set-up" script that sets params
#
#
# Load necessary libraries (may need to install some of these) ------------
existing_Packages<-as.list(installed.packages()[,1])
# Add new packages you might need to this line, to check if they're installed and install missing packages
required_Packages<-c('lme4','mvtnorm','tidyverse','data.table','stargazer','boot')
missing_Packages<- required_Packages[!required_Packages%in% existing_Packages]
if(length(missing_Packages)>0)install.packages(pkgs =  missing_Packages)
invisible(lapply(required_Packages, require, character.only=T,quietly = T))
# Load Data ---------------------------------------------------------------
# Load results from Acoustic analysis (param values)
attach(loadfile); 
s_statsA <- s_stats;
Stratalist <- Stratalist
vnA <- vn
detach(paste0('file:',loadfile),character.only = TRUE)
# rm(vn)
# Load Call-Count conversion function 
attach(loadfile2); 
Convfxn = Convertfxn
Convplot = Convertplot
s_statsC <- s_stats;
vnC = vn
detach(paste0('file:',loadfile2),character.only = TRUE)
AB = rmvnorm(n=simreps, mean=Convfxn$means, sigma=Convfxn$covmat)
alphR = AB[,1]; BetaR = AB[,2]; rm(AB)
# Load count data
dfArea = read.csv(file = Areasdatfile, header = TRUE, sep = ",")
dfC = read.csv(file = loadCdat, header = TRUE, sep = ",")
#
# Get params for simulations ----------------------------------------------
#
# Fit linear mixed model to nest counts to get estimates of variance 
# for sites within year and sequential counts within site
ii = which(dfC$StrataName==Strata_trends)
dfNC = dfC[ii,]
dfNC$logNC = log(dfNC$Density+1.0001)
lmm <- lmer(Density ~ contract_year + (1 | SPID), data = dfNC,
            REML = FALSE)
# summary(lmm)
# Test = Anova(lmm) # get P value for linear mixed model
tmp = as.data.frame(VarCorr(lmm)) # extract variance components
sigN = tmp$sdcor[1]
sigC = tmp$sdcor[2]
Yrcounts = unique(dfNC$contract_year)
# Dens0 = mean(dfNC$Density[dfNC$contract_year==min(Yrcounts)])  # Initial nest density
ii = which(Stratalist$StratName==Strata_trends)
CR0 = s_statsA$Mean[which(startsWith(vnA,paste0("C[",ii)))] 
minCsite = min(s_statsA$Mean[which(startsWith(vnA,paste0("Csite[",ii)))] )
Dispers = s_statsA$Mean[which(startsWith(vnA,"Dispers"))] 
sigS = s_statsA$Mean[which(startsWith(vnA,"sigS"))]  # variance CR across sites
sigD = s_statsC$Mean[which(startsWith(vnC,"sigD"))]  # Variance in dens est from calls
alph = s_statsC$Mean[which(startsWith(vnC,"alpha"))] # Param 1 for call-count convers
Beta= s_statsC$Mean[which(startsWith(vnC,"Beta"))]  # Param 2 for call-count convers
Dens0 = alph*CR0^Beta
#rm(s_statsC)
# rm(s_statsA)

Vn = sigN^2
Vc = sigC^2
Vs = sigS^2
Vd = sigD^2
NSite = NSiteA
Nyrs = NyrsP
Years = 1:Nyrs  
Ncnts = NSiteC

# Initialize Variables
# Stats for calls
r_est = numeric(length=simreps)
Pval = numeric(length=simreps)
Psig = numeric(length=simreps)
r_CI = matrix(nrow=simreps,ncol=2)
# Stats for density est by acoustic data
rA_est = numeric(length=simreps)
PvalA = numeric(length=simreps)
PsigA = numeric(length=simreps)
rA_CI = matrix(nrow=simreps,ncol=2)
# Stats for density est by nest counts
rC_est = numeric(length=simreps)
PvalC = numeric(length=simreps)
PsigC = numeric(length=simreps)
rC_CI = matrix(nrow=simreps,ncol=2)
C = matrix(0,nrow = simreps,ncol = Nyrs)
D = matrix(0,nrow = simreps,ncol = Nyrs)
DhatA = matrix(0,nrow = simreps,ncol = Nyrs)
DhatAsd = matrix(0,nrow = simreps,ncol = Nyrs)
DhatC = matrix(0,nrow = simreps,ncol = Nyrs)
DhatCsd = matrix(0,nrow = simreps,ncol = Nyrs)
Ahat = matrix(0,nrow = simreps,ncol = Nyrs)
Ahatsd = matrix(0,nrow = simreps,ncol = Nyrs)

# Some matrices that get over-written
EstD = matrix(0,nrow = NSite, ncol = Nyrs)
EstA = matrix(0,nrow = NSite, ncol = Nyrs)
EstNC = matrix(0,nrow = Ncnts, ncol = Nyrs)
NCT = numeric(length=NcountsPSite)
# Nste = numeric(length=Ncnts)
# Run Sims
for (r in 1:simreps){
  D[r,1] = Dens0
  C[r,1] = CR0
  for (t in 2:Nyrs){
    D[r,t] = D[r,t-1]*exp(rnorm(1,TRUE_r,Sigma_r))
    # C[r,t] = C[r,t-1]*exp(rnorm(1,TRUE_r,Sigma_r))
    C[r,t] = (D[r,t]/alph)^(1/Beta)
    # D[r,t] = alph*C[r,t]^Beta
  }
  cntr = 0;
  for (t in 1:Nyrs){
    cntr = cntr + 1
    muS = log(C[r,t]/sqrt(1+Vs/C[r,t]^2))
    sgS = sqrt(log(1+ Vs/C[r,t]^2))
    muN = log(D[r,t]/sqrt(1+Vn/D[r,t]^2))
    sgN = sqrt(log(1+ Vn/D[r,t]^2))    
    Csite = numeric(length=NSite)
    DensN = numeric(length=Ncnts)
    if (cntr == Countfreq){
      for (s in 1:Ncnts){
        DensN[s] = rlnorm(1,muN,sgN)
        muC = log(DensN[s]/sqrt(1+Vc/DensN[s]^2))
        sgC = sqrt(log(1+ Vc/DensN[s]^2))
        for (c in 1:NcountsPSite){
           NCT[c] = rlnorm(1,muC,sgC)
        }
        EstNC[s,t] = mean(NCT[c])
      }
      DhatC[r,t] = mean(EstNC[,t])
      DhatCsd[r,t] = sd(EstNC[,t])
      cntr = 0;
    }else{
      DhatC[r,t] = NA
      DhatCsd[r,t] = NA
    }
    for (s in 1:NSite){
      Csite[s] = rlnorm(1,muS,sgS)
      mu = Csite[s]
      vr = ifelse(mu<0,0.0000001, mu + (mu^2)/Dispers)
      p = max(0.00000001,1-(vr-mu)/vr)
      Calls = rnbinom(RecPsite,Dispers,p) 
      EstA[s,t] = max(minCsite/2,mean(Calls))
      expD = alph*EstA[s,t]^Beta
      muD = log(expD/sqrt(1+Vd/expD^2))
      sgD = sqrt(log(1+ Vd/expD^2))      
      EstD[s,t] = rlnorm(1,muD,sgD) 
      # EstD[s,t] = expD
    }
    Ahat[r,t] = mean(EstA[,t])
    Ahatsd[r,t] = sd(EstA[,t])
    # DhatA[r,t] = mean(EstD[,t])
    DhatA[r,t] = alphR[r]*Ahat[r,t]^BetaR[r]
    DhatAsd[r,t] = sd(EstD[,t])    
    tmp=C[r,t]-Ahat[r,t]
  }
  # Stats for Call rate trend est
  lgN = log(Ahat[r,])
  fit = lm(lgN ~ Years)
  # summary(fit)
  r_est[r] = as.numeric(fit$coefficients[2])
  # Pval[r] = lmp(fit)
  Pval[r] = as.numeric(summary(fit)$coefficients[2,4])
  r_CI[r,] = as.numeric(confint(fit,level=P_signif)[2,])
  if (sign(r_CI[r,1])==sign(TRUE_r) & sign(r_CI[r,2])==sign(TRUE_r)){
    Psig[r] = 1
  }else{
    Psig[r] = 0
  }
  # Stats for acoustic data density est
  lgNA = log(DhatA[r,])
  fitA = lm(lgNA ~ Years, na.action=na.omit)
  # summary(fitA)
  # PvalC[r] = lmp(fitC)
  rA_est[r] = as.numeric(fitA$coefficients[2])
  PvalA[r] = as.numeric(summary(fitA)$coefficients[2,4])
  rA_CI[r,] = as.numeric(confint(fitA,level=P_signif)[2,])  
  if (sign(rA_CI[r,1])==sign(TRUE_r) & sign(rA_CI[r,2])==sign(TRUE_r)){
    PsigA[r] = 1
  }else{
    PsigA[r] = 0
  }
  # Stats for nest count density est 
  npts = sum(!is.na(DhatC[r,]))
  if(npts>2){
    lgNC = log(DhatC[r,])
    fitC = lm(lgNC ~ Years, na.action=na.omit)
    # summary(fit)
    # PvalC[r] = lmp(fitC)
    rC_est[r] = as.numeric(fitC$coefficients[2])
    PvalC[r] = as.numeric(summary(fitC)$coefficients[2,4])
    rC_CI[r,] = as.numeric(confint(fitC,level=P_signif)[2,])  
    if (sign(rC_CI[r,1])==sign(TRUE_r) & sign(rC_CI[r,2])==sign(TRUE_r)){
      PsigC[r] = 1
    }else{
      PsigC[r] = 0
    }
  }
}
# Summarize sim stats ---------------------------------------------------
D_true = colMeans(D, na.rm = "T")
D_true_CIL = apply(D,2,quantile,.025)
D_true_CIH = apply(D,2,quantile,.975)
C_true = colMeans(C, na.rm = "T")
C_true_CIL = apply(C,2,quantile,.025)
C_true_CIH = apply(C,2,quantile,.975)
CR_estA = colMeans(Ahat, na.rm = "T")
CR_estA_CIL = colMeans(Ahat-1.96*(Ahatsd/sqrt(NSite)), na.rm = "T")
CR_estA_CIH = colMeans(Ahat+1.96*(Ahatsd/sqrt(NSite)), na.rm = "T")
# CR_estA_CIL = apply(Ahat,2,quantile,.05)
# CR_estA_CIH = apply(Ahat,2,quantile,.95)
D_estA = colMeans(DhatA, na.rm = "T")
D_estA_CIL = colMeans(DhatA-1.96*(DhatAsd/sqrt(NSite)), na.rm = "T")
D_estA_CIH = colMeans(DhatA+1.96*(DhatAsd/sqrt(NSite)), na.rm = "T")
# D_estA_CIL = apply(DhatA,2,quantile,.05)
# D_estA_CIH = apply(DhatA,2,quantile,.95)
D_estC = colMeans(DhatC, na.rm = "T")
D_estC_CIL = colMeans(DhatC-1.96*(DhatCsd/sqrt(Ncnts)), na.rm = "T")
D_estC_CIH = colMeans(DhatC+1.96*(DhatCsd/sqrt(Ncnts)), na.rm = "T")
# D_estC_CIL = apply(DhatC,2,quantile,.05,na.rm = "T")
# D_estC_CIH = apply(DhatC,2,quantile,.95,na.rm = "T")
x = seq(1,Nyrs)
# Trend plots:
# Call rate estimates
dat = data.frame(Year=x,Esimate=CR_estA,lower=CR_estA_CIL,upper=CR_estA_CIH,True=C_true)
pltR1 = ggplot() + 
  geom_errorbar(data=dat, mapping=aes(x=Year, ymax=upper, ymin=lower), width=0.2, size=1, color="blue") + 
  geom_point(data=dat, mapping=aes(x=Year, y=Esimate), size=4, shape=21, fill="white") +
  geom_line(data=dat, mapping=aes(x=Year, y=True),linetype = "dashed")+
  labs(x="Year", y = "Estimated Call Rate") +
  labs(title = paste0("Estimated Call Rate Over Time, r = ",TRUE_r, ", process error = ", Sigma_r,
                      ", "),
       subtitle = paste0("Monitor ", Nyrs," Years with ", NSite, " Sites and ",RecPsite, 
                         " Acoustic Records per site (Dashed line = Actual Trend)")) 
print(pltR1)
#
# Density Estimates, Acoustic and Counts
dat = data.frame(Year=x,Esimate=D_estA,lower=D_estA_CIL,upper=D_estA_CIH,True=D_true)
pltR2 = ggplot() + 
  geom_errorbar(data=dat, mapping=aes(x=Year, ymax=upper, ymin=lower), width=0.2, size=1, color="blue") + 
  geom_point(data=dat, mapping=aes(x=Year, y=Esimate), size=4, shape=21, fill="white") +
  geom_line(data=dat, mapping=aes(x=Year, y=True),linetype = "dashed")+
  labs(x="Year", y = "Estimated Density based on Call-Count Conversion") +
  labs(title = paste0("Estimated Density Over Time, Acoustic Data, r = ",TRUE_r, ", process error = ", Sigma_r,
                      ", "),
       subtitle = paste0("Monitor ", Nyrs," Years with ", NSite, " Sites and ",RecPsite, 
                         " Acoustic Records per site (Dashed line = Actual Trend)")) 
print(pltR2)
#
# Density Estimates, Counts only (if possible)
npts = sum(!is.na(D_estC))
if(npts>0){
  dat = data.frame(Year=x,Esimate=D_estC,lower=D_estC_CIL,upper=D_estC_CIH,True=D_true)
  pltR3 = ggplot() + 
    geom_errorbar(data=dat, mapping=aes(x=Year, ymax=upper, ymin=lower), width=0.2, size=1, color="blue") + 
    geom_point(data=dat, mapping=aes(x=Year, y=Esimate), size=4, shape=21, fill="white") +
    geom_line(data=dat, mapping=aes(x=Year, y=True),linetype = "dashed")+
    labs(x="Year", y = "Estimated Density from Nest Counts") +
    labs(title = paste0("Estimated Density Over Time, Nest Counts, r = ",TRUE_r, ", process error = ", Sigma_r,
                        ", "),
         subtitle = paste0("Monitor ", Nyrs, " Years, Nest counts at ", Ncnts, 
                           " sites, ", NcountsPSite, " reps per site, every ",  
          Countfreq, " Years (Dashed line = Actual Trend)")) 
  print(pltR3)
}

# Power Stats Summaries -------------------------------------------------------------
# 1) Power to Detect Trends in Call rates
sample_Rmn <- function(x, d) {
  return(rnorm(1,mean(x[d]), sd(x[d])/sqrt(NSite)))
}
Power_mn <- function(x, d) {
  return(100*(length(subset(x[d],x[d]<=(1-P_signif)))/length(x[d])))
}
PowerCR = 100*(length(subset(Pval,Pval<=(1-P_signif)))/length(Pval))
PowerCR_samp = boot(Pval, Power_mn, R=1000)  
dfPower = data.frame(Method = "Acoustic Data, Call rate",Power=PowerCR_samp$t)
P_sig = mean(Psig)
r_CIs = colMeans(r_CI)
mean_r_est = mean(r_est)
r_est_bt = boot(r_est, sample_Rmn, R=10000) 
r_est_bt = r_est_bt$t
if (npts > 0){
  Powersum = data.frame(N_Years = Nyrs, True_r = TRUE_r, Sigma_r = Sigma_r,
                        N_Sites = NSite, N_CPM15_st = RecPsite, 
                        Est_r = format(mean_r_est, digits = 3), 
                        CI_r_Lo = format(r_CIs[1], digits = 3),
                        CI_r_Hi = format(r_CIs[2], digits = 3),
                        Power = format(PowerCR, digits=3))
} else {
  Powersum = data.frame(N_Years = Nyrs, True_r = TRUE_r, Sigma_r = Sigma_r,
                        N_Sites = NSite, N_CPM15_st = RecPsite, 
                        Est_r = format(mean_r_est, digits=3), 
                        CI_r_Lo = format(r_CIs[1], digits = 3),
                        CI_r_Hi = format(r_CIs[2], digits = 3),
                        Power = format(PowerCR, digits=3))
}
# print("Power Summary, Trend in Call Rate Estimated from Acoustic Data")
# stargazer(Powersum, type = 'text', out = 'out.txt', summary=FALSE, rownames=FALSE)
# print(" ")
# print(" ")
df = data.frame(N_Sites = NSite, Estimate = r_est_bt)  
plt = ggplot(df, aes(x=Estimate, fill = NSite)) + geom_density(alpha=.3) +
  labs(title = paste0("Probability of Detecting trend in Call Rate"),
       subtitle = paste0("Monitor ", Nyrs," Years with ", NSite, " Sites and ",
                         RecPsite, " Acoustic Records per site" ),
       x="Estimated Trend in Call Rate", y = "Probability of Estimate") +
  geom_vline(xintercept = TRUE_r, colour = 'red') +
  annotate("text", x = TRUE_r, y = 5, label = "True r", colour = 'red', hjust = -.3) +
  geom_vline(xintercept = r_CIs[1], colour = 'magenta', linetype="dashed") +
  geom_vline(xintercept = r_CIs[2], colour = 'magenta', linetype="dashed") +
  theme(legend.position="none")
print(plt)
# ggsave(plt,filename=paste0('PowerAnalysis_',Species,'_NT_31May17.jpg'))


# 2) Power estimating Density Trends, Acoustic Data plus Calls to Counts Conversion Fxn
PowerA = 100*(length(subset(PvalA,PvalA<=(1-P_signif)))/length(PvalA))
PowerA_samp = boot(PvalA, Power_mn, R=1000)  
dfPower = rbind(dfPower, data.frame(Method = "Acoustic-based Density Estimate",Power=PowerA_samp$t))
Psig_A = mean(PsigA)
rA_CIs = colMeans(rA_CI)
mean_r_estA = mean(rA_est)
r_est_densA = density(rA_est)
r_estA_bt = boot(rA_est, sample_Rmn, R=10000) 
r_estA_bt = r_estA_bt$t
if (npts > 0){
  PowersumA = data.frame(N_Years = Nyrs, True_r = TRUE_r, Sigma_r = Sigma_r,
                        N_Sites = NSite, N_CPM15_st = RecPsite, 
                        Est_r = format(mean_r_estA, digits = 3), 
                        CI_r_Lo = format(rA_CIs[1], digits = 3),
                        CI_r_Hi = format(rA_CIs[2], digits = 3),
                        Power = format(PowerA, digits=3))
} else {
  PowersumA = data.frame(N_Years = Nyrs, True_r = TRUE_r, Sigma_r = Sigma_r,
                        N_Sites = NSite, N_CPM15_st = RecPsite,  
                        Est_r = format(mean_r_estA, digits=3), 
                        CI_r_Lo = format(rA_CIs[1], digits = 3),
                        CI_r_Hi = format(rA_CIs[2], digits = 3),
                        Power = format(PowerA, digits=3))
}
# print("Power Summary, Density Estimated from Acoustic Data")
# stargazer(PowersumA, type = 'text', out = 'out.txt', summary=FALSE, rownames=FALSE)
# print(" ")
# print(" ")
#
dfA = data.frame(N_Sites = Ncnts, Estimate = r_estA_bt)  
pltA = ggplot(dfA, aes(x=Estimate, fill = NSite)) + geom_density(alpha=.3) +
  labs(x="Estimated Population Trend", y = "Probability of Estimate") +
  labs(title = paste0("Probability of Detecting Trend in Acoustic-Estimated Density"),
       subtitle = paste0("Monitor ", Nyrs," Years with ", NSite, " Sites, Density estimated from Conversion Fxn" )) +
  geom_vline(xintercept = TRUE_r, colour = 'red') +
  annotate("text", x = TRUE_r, y = 5, label = "True r", colour = 'red', hjust = -.3) +
  geom_vline(xintercept = rA_CIs[1], colour = 'magenta', linetype="dashed") +
  geom_vline(xintercept = rA_CIs[2], colour = 'magenta', linetype="dashed") +
  theme(legend.position="none")
print(pltA)

# 3) Power estimating Density Trends, Counts only (Possible only if 3 or more sets of counts)
if(npts>=3){
    sample_Rmn <- function(x, d) {
      return(rnorm(1,mean(x[d]), sd(x[d])/sqrt(npts)))
    }  
  # Pval_C = median(PvalC)
  PowerC = 100*(length(subset(PvalC,PvalC<=(1-P_signif)))/length(PvalC))
  PowerC_samp = boot(PvalC, Power_mn, R=1000)  
  dfPower = rbind(dfPower, data.frame(Method = "Nest Count Density Estimate",Power=PowerC_samp$t))
  # PowerC = 100*(1-Pval_C)
  Psig_C = mean(PsigC)
  rC_CIs = colMeans(rC_CI)
  mean_r_estC = mean(rC_est)
  r_estC_bt = boot(rC_est, sample_Rmn, R=10000) 
  r_estC_bt = r_estC_bt$t
  PowersumC = data.frame(N_Years = Nyrs, True_r = TRUE_r, Sigma_r = Sigma_r,
                        N_Sites = Ncnts, Yr_bt_Cnts = Countfreq,  
                        RepNC_st = NcountsPSite, 
                        Est_r = format(mean_r_estC, digits=3), 
                        CI_r_Lo = format(rC_CIs[1], digits = 3),
                        CI_r_Hi = format(rC_CIs[2], digits = 3),
                        Power = format(PowerC, digits=3))
  #
  # print("Power Summary, Density estimated from Count Data Only")
  # stargazer(PowersumC, type = 'text', out = 'out.txt', summary=FALSE, rownames=FALSE)
  # #
  dfC = data.frame(N_Sites = Ncnts, Estimate = r_estC_bt)  
  pltC = ggplot(dfC, aes(x=Estimate, fill = NSite)) + geom_density(alpha=.3) +
    labs(x="Estimated Population Trend", y = "Probability of Estimate") +
    labs(title = paste0("Probability of Detecting trend, Counts Only, "),
         subtitle = paste0("Monitor ", Nyrs, " Years, nest counts at ", Ncnts, 
                           " sites, ", NcountsPSite, " reps per site, every ",  Countfreq, " Years" )) +
    geom_vline(xintercept = TRUE_r, colour = 'red') +
    annotate("text", x = TRUE_r, y = 5, label = "True r", colour = 'red', hjust = -.3) +
    geom_vline(xintercept = rC_CIs[1], colour = 'magenta', linetype="dashed") +
    geom_vline(xintercept = rC_CIs[2], colour = 'magenta', linetype="dashed") +
    theme(legend.position="none")
  print(pltC)
}

# Plot methods to compare
pltP = ggplot(dfPower, aes(x=Method, y=Power)) +
  geom_boxplot(fill = "light blue", colour = "black",
               alpha = 0.7) +
  scale_y_continuous(name = "Power to Detect Trend",labels = scales::comma) +
  scale_x_discrete(name = "Method of Analysis") +
  labs(title = paste0("Power Analysis, Comparison of Methods"),
       subtitle = paste0("Acoustic ", Nyrs," Years with ", NSite, " Sites vs. ",
         "Nest Counts at ", Ncnts, " sites, ", NcountsPSite, " reps per site, every ",  Countfreq, " Years" )) +
  theme(axis.text.x=element_text(angle=45,hjust=1)) 
print(pltP)

Powersum$Type='Call trend'
PowersumA$Type='Density trend from acoustics'

setnames(Powersum,'N_Sites','NSiteA')
setnames(PowersumA,'N_Sites','NSiteA')
setnames(Powersum,'N_CPM15_st','RecPsite')
setnames(PowersumA,'N_CPM15_st','RecPsite')

if (exists('PowersumC')) { 
  PowersumC$Type='Density trend from counts only'
  setnames(PowersumC,'N_Sites','NSiteC')
  setnames(PowersumC,'Yr_bt_Cnts','Countfreq')
  setnames(PowersumC,'RepNC_st','NcountsPSite')
  PowersumCombined=bind_rows(Powersum,PowersumA,PowersumC) 
} else {
  PowersumCombined=bind_rows(Powersum,PowersumA)
}
PowersumCombined$P_signif=P_signif
PowersumCombined$Species=Species
PowersumCombined$RunDate=format(Sys.Date(),"%d%b%y")

write.table(PowersumCombined,file='D:/CM,Inc/Dropbox (CMI)/CMI_Team/Analysis/2018/Bayesian_2018/results/tables/PowerTable.csv',row.names=F,sep=',')
# write.table(PowersumCombined,file='D:/CM,Inc/Dropbox (CMI)/CMI_Team/Analysis/2018/Bayesian_2018/results/tables/PowerTable.csv',row.names=F,append=T,sep=',',col.names=F)


