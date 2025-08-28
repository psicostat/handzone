
#######################################

rm(list=ls())
library(lme4)
library(lmerTest)
library(lavaan)
library(ggplot2)

#######################################

# BASIC SIMPSON PARADOX

set.seed(123)

# simulate data
N = 6
k = 10
context = rep(1:N,each=k)
x = runif(N*k,1,5)+context*6
err = rnorm(N*k)
y = 0 + .5*x - context*4 + err
d = data.frame(context,x,y)
d$context = letters[d$context]

# visualize data
ggplot(d,aes(x=x,y=y,group=context,color=context))+
  geom_point(size=8,alpha=.7)+
  theme(text=element_text(size=24))

# fit models
fitLM = lm(y ~ x, data=d)
summary(fitLM)
fitLMERri = lmer(y ~ x + (1|context), data=d)
summary(fitLMERri)

# sem multilivello
m = "
Level: 1
y ~ x
Level: 2
y ~ x
"
fit = sem(m, data=d, cluster="context")
summary(fit)

#######################################

# CHILDREN IN CLASSROOMS

set.seed(956)

# generate data
nClasses = 8
kChildren = 15
class = rep(1:nClasses,each=kChildren)
read = rnorm(nClasses*kChildren,0,.5)+((class-(nClasses/2)-.5)*.25)
math = rnorm(nClasses*kChildren,0,.5)+((class-(nClasses/2)-.5)*.25) + read*.3
d = data.frame(class,read,math)
d$class = letters[d$class]

# visualize data naively (without considering classes)
ggplot(d,aes(x=read,y=math))+
  geom_point(size=8,alpha=.7)+
  theme(text=element_text(size=24))

# naively fit linear model
fit0 = lm(math ~ read, data=d)
summary(fit0)

# visualize data in the correct way!
ggplot(d,aes(x=read,y=math,group=class,color=class))+
  geom_point(size=8,alpha=.7)+
  theme(text=element_text(size=24))

# fit mixed model
fit1 = lmer(math ~ read + (1|class), data=d)
summary(fit1)

# R2 of lmer
library(MuMIn)
MuMIn::r.squaredGLMM(fit1)

#######################################

# RESPONDENTS IN CITIES

set.seed(205)

# generate data
nCities = 6
kRespondents = 40
city = rep(1:nCities,each=kRespondents)
pg = rnorm(nCities*kRespondents,0,.7)+((city-(nCities/2)-.5)*.40)
wb = rnorm(nCities*kRespondents,0,.7)+((city-(nCities/2)-.5)*.40)
d = data.frame(city,pg,wb)
d$city = letters[d$city]

# visualize data naively (without considering cities) and fit LM through it
ggplot(d,aes(x=pg,y=wb))+
  geom_point(size=4,alpha=.7)+
  theme(text=element_text(size=24))+
  geom_smooth(color="red",method="lm",linewidth=1.5)+
  xlab("Perceived green (z)")+ylab("Well-being (z)")
# see summary of simple LM 
summary(lm(wb~pg,data=d))
  
# visualize data considering cities
ggplot(d,aes(x=pg,y=wb,color=city))+
  geom_point(size=4,alpha=.8)+
  theme(text=element_text(size=24))+
  xlab("Perceived green (z)")+ylab("Well-being (z)")
# see summary of the same effect but with random intercept for cities
summary(lmer(wb~pg+(1|city),data=d))

# sem multilivello
m = "
Level: 1
wb ~ pg
Level: 2
wb ~ pg
"
fit = sem(m, data=d, cluster="city")
summary(fit)

#######################################

