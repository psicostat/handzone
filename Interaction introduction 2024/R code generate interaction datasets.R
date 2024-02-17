
###############################

rm(list=ls())
library(lme4)
library(ggplot2)
library(effects)
library(performance)
cohens_f = function(fit1,fit0=NULL){
  if(is.null(fit0)){
    cf = sqrt(summary(fit1)$r.squared/(1-summary(fit1)$r.squared))
  }else{
    cf = sqrt( (summary(fit1)$r.squared-summary(fit0)$r.squared)/(1-summary(fit1)$r.squared) )
  }
  return(cf)
}
cohens_f_mixed = function(fit1,fit0,fit00){
  cf = sqrt( (r2_nakagawa(fit1)$R2_marginal-r2_nakagawa(fit0)$R2_marginal)/(1-r2_nakagawa(fit1)$R2_marginal) )
  return(cf)
}
N = 480

###############################

# IT'S ALL BETWEEN-SUBJECT FOR SIMPLICITY 
# BUT IN MANY CASES THIS IS NOT REALISTIC

###############################

# FULL CROSS 2x2

set.seed(3)
x = rep(c(0,1),each=N/2)
w = rep(c(0,1),each=N/4,times=2)
y = 0 + 0.5*x + 0.3*w -0.974*x*w + rnorm(N,0,1)
d1 = data.frame(x,w,y)
d1$x = as.factor(d1$x)
d1$w = as.factor(d1$w)
fit0 = lm(y~x+w,data=d1)
fit1 = lm(y~x*w,data=d1)
plot(allEffects(fit1),multiline=T)
cohens_f(fit1,fit0)
d22 = d1

###############################

# TREATMENT-LIKE 2x2

set.seed(1)
x = rep(c(0,1),each=N/2)
w = rep(c(0,1),each=N/4,times=2)
y = 0 + .1*x + .0*w + 0.96*x*w + rnorm(N,0,1)
d1 = data.frame(x,w,y)
d1$x = as.factor(d1$x)
d1$w = as.factor(d1$w)
fit0 = lm(y~x+w,data=d1)
fit1 = lm(y~x*w,data=d1)
plot(allEffects(fit1),multiline=T)
cohens_f(fit1,fit0)
dt22 = d1

# with repeated measures
# set.seed(1)
# id = rep(1:(N/2),each=2)
# rint = rep(rnorm(N/2,0,1.5),each=2)
# x = rep(c(0,1),each=N/2)
# w = rep(c(0,1),each=N/4,times=2)
# y = 0 + 0*x + .0*w + 0.96*x*w + rint + rnorm(N,0,1)
# d1 = data.frame(x,w,y,id)
# d1$x = as.factor(d1$x)
# d1$w = as.factor(d1$w)
# fit00 = lmer(y~1+(1|id),data=d1)
# fit0 = lmer(y~x+w+(1|id),data=d1)
# fit1 = lmer(y~x*w+(1|id),data=d1)
# plot(allEffects(fit1),multiline=T)
# cohens_f_mixed(fit1,fit0,fit00)

###############################

# TREATMENT-LIKE 2x3

set.seed(0)
x0 = rep(c(0,1,0),each=N/3)
x1 = rep(c(0,0,1),each=N/3)
w = rep(c(0,1),each=N/6,times=3)
y = 0 + 0*x0 + 0.3*x1 + .0*w +1.16*x0*w + 1.1*x1*w+ rnorm(N,0,1)
x = paste0(x0,x1)
d1 = data.frame(x,w,y)
d1$x = as.factor(d1$x)
d1$w = as.factor(d1$w)
fit0 = lm(y~x+w,data=d1)
fit1 = lm(y~x*w,data=d1)
plot(allEffects(fit1),multiline=T)
cohens_f(fit1,fit0)
dt23 = d1

###############################

# 2 x continuous

set.seed(0)
x = rnorm(N,0,1)
w = rep(c(0,1),each=N/4,times=2)
y = 0 + .4*x +2*w + 0.504*x*w + rnorm(N,0,1)
d1 = data.frame(x,w,y)
d1$w = as.factor(d1$w)
fit0 = lm(y~x+w,data=d1)
fit1 = lm(y~x*w,data=d1)
plot(allEffects(fit1),multiline=T)
cohens_f(fit1,fit0)
d2c = d1

###############################

# continuous x continuous

set.seed(0)
x = rnorm(N,0,1)
w = rnorm(N,0,1)
y = 0.1 + 0.3*x +0.3*w + 0.261*x*w + rnorm(N,0,1)
d1 = data.frame(x,w,y)
fit0 = lm(y~x+w,data=d1)
fit1 = lm(y~x*w,data=d1)
plot(allEffects(fit1),multiline=T)
cohens_f(fit1,fit0)
dcc = d1

eff = data.frame(allEffects(fit1,xlevels=list(x=seq(-2,2,.1),w=c(-1.5,-1,-0.5,0,0.5,1,1.5)))$"x:w")
eff$w = as.factor(eff$w)
tsz = 20
ggplot(eff,aes(x=x,y=fit,group=w,color=w,fill=w))+
  geom_ribbon(aes(ymin=lower,ymax=upper),linetype=1,linewidth=1,alpha=.2,color=NA)+
  geom_line(linewidth=1)+
  theme(text=element_text(size=tsz))+
  ylab("y (outcome, z-score)")

###############################

save(d22,dt22,dt23,d2c,dcc,cohens_f,file="interaction_datasets.RData")

###############################
###############################
###############################
###############################

