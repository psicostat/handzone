
################################

# Clean workspace and import packages
rm(list=ls())
library(lme4)
library(lmerTest)
library(effects)
library(ggplot2)
library(lattice)

################################

# VISUALIZE DATA AND FIT MODELS ON SLEEPSTUDY

# import example dataset from lme4
d = lme4::sleepstudy

# plot data
ggplot(data=d,aes(x=Days,y=Reaction,group=Subject,color=Subject))+
  geom_point(size=3,alpha=.8)+geom_line(linewidth=1,alpha=.5)+
  scale_x_continuous(breaks=seq(0,10,2))+
  theme(text=element_text(size=18))

# simple lm
fitLM = lm(Reaction ~ Days, data=d)
summary(fitLM)

# lmm with random intercept for subject
fitLMMri = lmer(Reaction ~ Days + (1|Subject), data=d)
summary(fitLMMri)
lattice::dotplot(ranef(fitLMMri))

# lmm with random intercept and random slope
fitLMMrs = lmer(Reaction ~ Days + (Days|Subject), data=d)
summary(fitLMMrs)
lattice::dotplot(ranef(fitLMMrs))

# distribution of random intercepts
rInt = data.frame(ri=ranef(fitLMMrs)$Subject$"(Intercept)" + fitLMMrs@beta[1])
ggplot(rInt)+
  geom_density(aes(x=ri),fill="blue",alpha=.5,color=NA)+
  theme(text=element_text(size=24))+
  geom_vline(xintercept=fitLMMrs@beta[1],linewidth=2,color="red",linetype=2)+
  xlab("Estimated individual intercepts")

# distribution of random slopes
rSlo = data.frame(rs=ranef(fitLMMrs)$Subject$"Days" + fitLMMrs@beta[2])
ggplot(rSlo)+
  geom_density(aes(x=rs),fill="blue",alpha=.5,color=NA)+
  theme(text=element_text(size=24))+
  geom_vline(xintercept=fitLMMrs@beta[2],linewidth=2,color="red",linetype=2)+
  xlab("Estimated individual slopes")


################################

# SIMULATE DATA on performance variable Y in z-scores

set.seed(1002)

# set basic parameters 
N = 25 # number of participants
k = 10 # number of days of sleep deprivation
fixedIntercept = 0 # mean score at day 0
fixedSlope = -.2 # mean effect per day of sleep deprivation
randomInterceptSD = 1 # SD of of scores at day 0 across subjects
randomSlopeSD = .07 # SD of effect per day of sleep deprivation across subjects
sigma = .3 # SD of model residuals ()

# generate data
id = rep(paste("s",1:N,sep=""),each=k)
randInt = rep(rnorm(N,0,randomInterceptSD),each=k)
randSlope = rep(rnorm(N,0,randomSlopeSD),each=k)
days = rep(0:(k-1),times=N)
error = rnorm(N*k,0,sigma)
# generate outcome
y = fixedIntercept + fixedSlope * days + randInt + randSlope * days + error

# put everything in a dataset
d = data.frame(id, days, y)

# visualize data
ggplot(data=d,aes(x=days,y=y,group=id,color=id))+
  geom_point(size=3,alpha=.8)+geom_line(linewidth=1,alpha=.5)+
  scale_x_continuous(breaks=seq(0,10,2))+
  theme(text=element_text(size=18))

#### Fit various models and perform model selection:

# no effects at all
fit0 = lm(y ~ 1, data=d)

# days has a general fixed effect, which is the same for all subjects, and subjects have no variability in mean scores
fit1 = lm(y ~ days, data=d)

# days has a general fixed effect, which is the same for all subjects, but subjects still have different mean levels (random intercepts)
fit2 = lmer(y ~ days + (1|id), data=d) 

# days has no general fixed effect, but just has a different unpredictable effect on each subject
fit3 = lmer(y ~ 1 + (days|id), data=d)

# days has a general fixed effect, and this effect is also heterogeneous across subjects
fit4 = lmer(y ~ days + (days|id), data=d)

# model comparison
anova(fit4,fit3,fit2,fit1,fit0)

# let's have a look at the summary of the "real" model, i.e., fit4
summary(fit4)

# let's have a look at the random effects
lattice::dotplot(ranef(fit4))
lattice::dotplot(ranef(fit4))$"id"[1]
lattice::dotplot(ranef(fit4),xlim=c(-.3,.3))$"id"[2]

# plot data with the fixed effect superimposed on it
eff = data.frame(allEffects(fit4)$"days")
ggplot()+
  geom_point(data=d,aes(x=days,y=y,group=id,color=id),size=3,alpha=.8)+
  geom_line(data=d,aes(x=days,y=y,group=id,color=id),linewidth=1,alpha=.5)+
  geom_line(data=eff,aes(x=days,y=fit),linewidth=1)+
  geom_ribbon(data=eff,aes(x=days,ymin=lower,ymax=upper),alpha=.22)+
  scale_x_continuous(breaks=seq(0,10,2))+
  theme(text=element_text(size=18))

################################


