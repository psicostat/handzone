
#####################################

set.seed(4) # set seed for reproducing the exact same random values

rm(list=ls()) # empty workspace before beginning

# load packages
library(ggplot2)
library(effects)
library(lme4)
library(lmerTest)

#####################################

# ESEMPIO INIZIALE MODELLO 3 (TEMPO) x 2 (GRUPPO)

# define the sample size
N = 100

# simulate the data
d = data.frame(subj = rep(1:(N*2),times=3), 
               r.int = rep(rnorm(N*2),times=3),
               time = rep(c("1.PRE","2.POST","3.FOLLOWUP"),each=N*2),
               group = rep(c("Controllo","Trattamento"),each=N,times=3),
               score = NA)
d$dummyPost = ifelse(d$time=="2.POST",1,0)
d$dummyFU = ifelse(d$time=="3.FOLLOWUP",1,0)
d$dummyTrat = ifelse(d$group=="Trattamento",1,0)
d$score = 35+d$dummyPost*8+d$dummyFU*4+d$dummyTrat*0+d$dummyTrat*d$dummyPost*10+d$dummyTrat*d$dummyFU*4+d$r.int+rnorm(nrow(d),0,10)

# fit the model
fit = lmer(score ~ group*time + (1|subj), data=d)
summary(fit)$coef

# plot the effect with basic plot
plot(allEffects(fit),multiline=T,ci.style="bars")

# plot the effect with ggplot
eff = data.frame(effect("group:time",fit))
pd = position_dodge(width=.3)
ggplot(eff,aes(y=fit,x=time,group=group,color=group,linetype=group))+
  geom_point(size=5,position=pd)+
  geom_line(linewidth=1.5,position=pd)+
  geom_errorbar(aes(ymin=lower,ymax=upper),linetype=1,linewidth=1.5,width=.2,position=pd)+
  theme(text=element_text(size=20))+
  scale_y_continuous(breaks=seq(0,80,4))+
  xlab("Time")+ylab("Score")

#####################################

# ESERCIZIO PRINCIPALE MATH ANXIETY - ACHIEVEMENT

N = 1e5
d = data.frame(gender = sample(c(0,1),N,.5),
               MA = rnorm(N,24.5,6.2), ach = NA)
d$MA = d$MA - 2*d$gender
d$ach = 59.58 - 1.02*d$gender - 0.46*d$MA + 0.10*d$gender*d$MA + rnorm(nrow(d),0,10.03)

#Trasformazioni
d$gender = ifelse(d$gender==0,"female","male")
d$gender = as.factor(d$gender)

# fit model
fit = lm(ach ~ gender * MA, data = d)
summary(fit)$coef
sigma(fit)
confint(fit)

# plot the effect with ggplot ... in the prevalent range
eff = data.frame(effect("gender*MA",fit,xlevels=list(MA=seq(16,32,1))))
ggplot(eff,aes(x=MA,y=fit,group=gender,color=gender,fill=gender,linetype=gender))+
  geom_line(linewidth=1.5)+
  geom_ribbon(aes(ymin=lower,ymax=upper),color=NA,alpha=.25)+
  theme(text=element_text(size=20))+
  xlab("Math Anxiety (raw score)")+ylab("Math Achievement (T)")

# plot the effect with ggplot ... until the intercept
eff = data.frame(effect("gender*MA",fit,xlevels=list(MA=seq(-2,35,1))))
ggplot(eff,aes(x=MA,y=fit,group=gender,color=gender,fill=gender,linetype=gender))+
  geom_line(linewidth=2)+
  geom_ribbon(aes(ymin=lower,ymax=upper),color=NA,alpha=.25)+
  theme(text=element_text(size=20))+
  xlab("Math Anxiety (raw score)")+ylab("Math Achievement (T)")+
  geom_vline(xintercept=0,linewidth=1,linetype=3)+
  scale_x_continuous(breaks=seq(-10,100,5))+
  scale_y_continuous(breaks=seq(0,100,2))

#####################################

