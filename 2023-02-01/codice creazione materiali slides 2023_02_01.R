
#####################################

set.seed(24) # set seed for reproducing the exact same random values

rm(list=ls()) # empty workspace before beginning

# load packages
library(ggplot2)
library(gridExtra)
library(effects)
library(lme4)
library(lmerTest)
library(lm.beta)

#####################################

# ESEMPIO MODELLO Condition x Age (2 x 2)

# define N (per condition x age)
N = 50
print(paste("Total N =",N*4))

# simulate the data
d = data.frame(Condition=rep(c("2.DownSyndrome","1.TypDev"),each=N,times=2),
               Age=rep(c("08yo","10yo"),each=N*2),
               IQ=rnorm(N*4,mean=100,sd=15))
d$IQ[d$Condition=="2.DownSyndrome"] = d$IQ[d$Condition=="2.DownSyndrome"] - 40
d$IQ[d$Condition=="2.DownSyndrome"&d$Age=="10yo"] = d$IQ[d$Condition=="2.DownSyndrome"&d$Age=="10yo"]-18

# correct model
fit1 = lm(IQ ~ Condition * Age, data=d)
summary(fit1)$coef

# plot INTERACTION with ggplot
eff = data.frame(effect("Condition:Age",fit1))
pd = position_dodge(width=.2)
ggplot(eff,aes(y=fit,x=Age,group=Condition,color=Condition,linetype=Condition))+
  geom_point(size=5,position=pd)+
  geom_line(linewidth=1.5,position=pd)+
  geom_errorbar(aes(ymin=lower,ymax=upper),linetype=1,linewidth=1.5,width=.2,position=pd)+
  scale_color_manual(values=c("blue","red"))+
  scale_y_continuous(breaks=seq(40,160,15))+
  theme(text=element_text(size=20))+
  xlab("Age")+ylab("mean IQ")

# model comparisons
fit0 = lm(IQ ~ Condition + Age, data=d)
fit0.C = lm(IQ ~ Age, data=d)
anova(fit0,fit0.C) # compare between two nested models
fit1 = lm(IQ ~ Condition * Age, data=d)
fit1 = lm(IQ ~ Condition + Age + Condition:Age, data=d)
anova(fit0,fit1) # compare between two nested models
anova(fit1) # test anova for all model terms
AIC(fit0,fit0.C,fit1) # competing models AICs (lower is better)
BIC(fit0,fit0.C,fit1) # competing models BICs (lower is better)

# now plot ONLY MAIN EFFECTS
effC = data.frame(effect("Condition",fit0))
(ggC = ggplot(effC,aes(y=fit,x=Condition,group=1))+
  geom_point(size=5,position=pd)+
  geom_line(linewidth=1.1,position=pd)+
  geom_errorbar(aes(ymin=lower,ymax=upper),linetype=1,linewidth=1.5,width=.2,position=pd)+
    scale_y_continuous(breaks=seq(40,160,15),limits=c(min(effC$fit)-3,max(effC$fit)+3))+
  theme(text=element_text(size=20))+
  xlab("Condition")+ylab("mean IQ")) 
effA = data.frame(effect("Age",fit0))
(ggA = ggplot(effA,aes(y=fit,x=Age,group=1))+
    geom_point(size=5,position=pd)+
    geom_line(linewidth=1.1,position=pd)+
    geom_errorbar(aes(ymin=lower,ymax=upper),linetype=1,linewidth=1.5,width=.2,position=pd)+
    scale_y_continuous(breaks=seq(40,160,15),limits=c(min(effC$fit)-3,max(effC$fit)+3))+
    theme(text=element_text(size=20))+
    xlab("Age")+ylab("mean IQ")) 
grid.arrange(ggC,ggA,ncol=2)

# MAIN EFFECTS model coefficients
fit0 = lm(IQ ~ Condition + Age, data=d)
summary(fit0)$coef

#####################################

# ESEMPIO MODELLO Condition x Age (2 x Continuous)

# define N (per condition)
N = 100

# simulate the data
TD = data.frame(Condition="1.TypDev",
                Age=runif(N,6,12),
                IQ=rnorm(N,100,15))

DS = data.frame(Condition="2.DownSyndrome",
                Age=runif(N,6,12),
                IQ=rnorm(N,50,15))
DS$IQ = 30 + DS$IQ -4*DS$Age
d = rbind(TD,DS)

# model comparisons
fit0 = lm(IQ ~ Condition+Age, data=d)
fit1 = lm(IQ ~ Condition*Age, data=d)
anova(fit0,fit1)

# see interaction model coefficients
summary(fit1)$coef

# plot INTERACTION with ggplot
eff = data.frame(allEffects(fit1,xlevels=list(Age=seq(6,12,.1)))$"Condition:Age")
ggplot(eff,aes(y=fit,x=Age,group=Condition,color=Condition,linetype=Condition,fill=Condition))+
  geom_line(linewidth=1.5)+
  geom_ribbon(aes(ymin=lower,ymax=upper),color=NA,alpha=.3)+
  geom_point(data=d,aes(y=IQ,shape=Condition),alpha=.4,size=3)+
  scale_color_manual(values=c("blue","red"))+
  scale_fill_manual(values=c("blue","red"))+
  scale_y_continuous(breaks=seq(10,160,15))+
  theme(text=element_text(size=24))+
  xlab("Age")+ylab("IQ")


#####################################

