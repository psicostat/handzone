
################################

rm(list=ls())
library(effects)
library(glmmTMB)
library(lme4)
library(car)
library(performance)

################################

# PROVA CON MIXED-MODEL

N = 3000
id = rep(1:N,each=3)
rInt = rep(rnorm(N,0,1),each=3)
time1 = rep(c(0,1,0),times=N)
time2 = rep(c(0,0,1),times=N)
group = rep(c(0,1),each=3*(N/2))
error = rnorm(N*3,0,1)
score = 0 + 0.3*time1 + 0.1*time2 + 0*group + 1*time1*group + 1.1*time2*group + rInt + error
time = paste0(time2,time1)
group = as.factor(group)
df = data.frame(id,time,group,score)
fit1 = lmer(score~group*time+(1|id),data=df)
plot(allEffects(fit1))
icc(fit1)

################################

# POWER ANALYSIS CON MIXED-MODEL

niter = 500
p = rep(NA,niter)

for(i in 1:niter){
  
  N = 30
  id = rep(1:N,each=3)
  rInt = rep(rnorm(N,0,1),each=3)
  time1 = rep(c(0,1,0),times=N)
  time2 = rep(c(0,0,1),times=N)
  group = rep(c(0,1),each=3*(N/2))
  error = rnorm(N*3,0,1)
  score = 0 + 0.3*time1 + 0.1*time2 + 0*group + 1*time1*group + 1.1*time2*group + rInt + error
  time = paste0(time2,time1)
  group = as.factor(group)
  df = data.frame(id,time,group,score)
  
  #fit0 = glmmTMB(score~group+time+(1|id),data=df)
  #fit1 = glmmTMB(score~group*time+(1|id),data=df)
  #plot(allEffects(fit1))
  fit1 = lmer(score~group*time+(1|id),data=df)
  p[i] = Anova(fit1)$`Pr(>Chisq)`[3]
  print(mean(p<.05,na.rm=T))
  
}

mean(p<.05,na.rm=T)


################################

