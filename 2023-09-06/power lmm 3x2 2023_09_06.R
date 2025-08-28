
#############################################
#############################################

rm(list=ls())
library(simr) # directly calculates power for LMM via simulation
library(lme4) # fit LMM
library(lmerTest) # good for adding p-values to lmer coefficients
library(MASS) # simulates correlated data
library(lavaan) # easy tool for writing correlation matrices
library(ggplot2) # great tool for plotting
library(effects) # tools for extracting effects from models

#############################################
#############################################

#### POWER ANALYSIS FOR 3 (BETWEEN) x 2 (WITHIN) INTERACTION
## simulation based on mean value in each cell

# define parameters
N = 45
sigmaID = sqrt(0.5) # consistent with the idea of (between-individual) z-score (which is not the actual z-score!)
sigmaResidual = sqrt(0.5) # interpreted as variability due to (un)reliability of measures
(sigmatot = sqrt(sigmaID^2+sigmaResidual^2)) # total standard deviation (net of fixed effects)
G1T1 = 0.0; G2T1 = 0.1; G3T1 = 0.1
G1T2 = -0.25; G2T2 = 0.1; G3T2 = 0.1
(ICC = sigmaID^2 / (sigmaID^2 + sigmaResidual^2)) # assumed intraclass correlation / test-retest stability

# visualize hypothesized effect
dg = data.frame(score=c(G1T1,G1T2,G2T1,G2T2,G3T1,G3T2),
                time=rep(c("T1","T2"),times=3),
                group=rep(c("G1","G2","G3"),each=2))
pd = position_dodge(width=0.3)
ggplot(dg,aes(x=time,y=score,group=group,color=group,shape=group))+
  geom_point(size=5,position=pd)+
  geom_line(size=1,position=pd)+
  theme(text=element_text(size=24))+
  ylab("z-score")

# RUN POWER SIMULATION
niter=500
pvals = rep(NA,niter)
for(i in 1:niter){
  # simulate data
  ID = rep(1:(N*3),times=2)
  randomIntercept = rep(rnorm(N*3,0,sigmaID),times=2)
  Group = rep(c("G1","G2","G3"),each=N,times=2)
  Time = rep(c("T1","T2"),each=N*3)
  residualerror = rnorm(N*3*2,0,sigmaResidual)
  # create outcome/dependent variable
  Score = randomIntercept + residualerror
  Score[Group=="G1"&Time=="T1"] = Score[Group=="G1"&Time=="T1"] + G1T1
  Score[Group=="G1"&Time=="T2"] = Score[Group=="G1"&Time=="T2"] + G1T2
  Score[Group=="G2"&Time=="T1"] = Score[Group=="G2"&Time=="T1"] + G2T1
  Score[Group=="G2"&Time=="T2"] = Score[Group=="G2"&Time=="T2"] + G2T2
  Score[Group=="G3"&Time=="T1"] = Score[Group=="G3"&Time=="T1"] + G3T1
  Score[Group=="G3"&Time=="T2"] = Score[Group=="G3"&Time=="T2"] + G3T2
  # put simulated data in data frame
  d = data.frame(ID,Group,Time,Score)
  # fit model
  fit1 = lmer(Score ~ Time * Group + (1|ID), data=d,REML=F)
  fit0 = lmer(Score ~ Time + Group + (1|ID), data=d,REML=F)
  #plot(allEffects(fit1,x.var="Time"), multiline=T, ci.style="bars")
  #summary(fit1)
  pvals[i] = anova(fit1,fit0)$"Pr(>Chisq)"[2]
  print(round(mean(pvals<0.05,na.rm=T),3))
}


########

# calculate intra-class correlation from a fitted model

cor(Score[Group=="G1"&Time=="T1"],Score[Group=="G1"&Time=="T2"]) # brutal way to have an estimate of ICC
performance::icc(fit1) # take adjusted ICC for controlling for fixed effects
# let's say my ICC is 0.7, and I set SigmaResidual = 1, what is SigmaID?

#############################################
#############################################

#### NOW THE SAME... BUT ADDING THE COVARIATE AND THE GROUP X COVARIATE INTERACTION

# define parameters
N = 1000
sigmaID = 1.0 # consistent with the idea of (between-individual) z-score
sigmaResidual = 0.5 # interpreted as variability due to (un)reliability of measures
(sigmatot = sqrt(sigmaID^2+sigmaResidual^2))
G1T1 = 0.0; G2T1 = 0.0; G3T1 = 0.0
G1T2 = 0.0; G2T2 = 0.5; G3T2 = 1.0
beta1G1 = 0.10; beta1G2 = 0.10; beta1G3 = 0.30

# simulate data
ID = rep(1:(N*3),times=2)
randomIntercept = rep(rnorm(N*3,0,sigmaID),times=2)
Group = rep(c("G1","G2","G3"),each=N,times=2)
Time = rep(c("T1","T2"),each=N*3)
covariata1_z = rep(rnorm(N*3,0,1),times=2)
residualerror = rnorm(N*3*2,0,sigmaResidual)

# create outcome/dependent variable
Score = randomIntercept + residualerror
Score[Group=="G1"&Time=="T1"] = Score[Group=="G1"&Time=="T1"] + G1T1
Score[Group=="G1"&Time=="T2"] = Score[Group=="G1"&Time=="T2"] + G1T2
Score[Group=="G2"&Time=="T1"] = Score[Group=="G2"&Time=="T1"] + G2T1
Score[Group=="G2"&Time=="T2"] = Score[Group=="G2"&Time=="T2"] + G2T2
Score[Group=="G3"&Time=="T1"] = Score[Group=="G3"&Time=="T1"] + G3T1
Score[Group=="G3"&Time=="T2"] = Score[Group=="G3"&Time=="T2"] + G3T2
Score[Group=="G1"] = Score[Group=="G1"] + beta1G1*covariata1_z[Group=="G1"]
Score[Group=="G2"] = Score[Group=="G2"] + beta1G2*covariata1_z[Group=="G2"]
Score[Group=="G3"] = Score[Group=="G3"] + beta1G3*covariata1_z[Group=="G3"]

# put simulated data in data frame
d = data.frame(ID,Group,covariata1_z,Score)

# fit model
fit1 = lmer(Score ~ Group*Time + Group*covariata1_z + (1|ID), data=d)
plot(allEffects(fit1)$"Group:covariata1_z", multiline=T, ci.style="bands")
summary(fit1)

# check ICC, R2, and cohen's f
cor(Score[Group=="G1"&Time=="T1"],Score[Group=="G1"&Time=="T2"])
performance::icc(fit1)
fit0 = lmer(Score ~ Group*Time + Group + covariata1_z + (1|ID), data=d)
(deltaR2 = as.numeric(performance::r2(fit1)$R2_marginal - performance::r2(fit0)$R2_marginal))
(cohensf = sqrt(deltaR2/(1-deltaR2)))


#############################################
#############################################

