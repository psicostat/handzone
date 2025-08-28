
#################################

rm(list=ls())
library(lme4)
library(lmerTest)
library(effects)
library(ggplot2)
library(lattice)
library(influence.ME)


#################################

# IAPS example

set.seed(572)

# generate data
Nsubj<-8
nrep<-20
d<-data.frame(id=rep(letters[1:Nsubj],nrep*2),Condition=rep(c("Neut","Neg"),each=Nsubj*nrep,times=2))
d$item<-rep(1:nrep,each=Nsubj)
d$rInt<-rep(rnorm(Nsubj,0,.5),times=nrep*2)
d$Conduct<-d$rInt+rnorm(Nsubj*nrep*2,0,.5)
d$Conduct[d$id==letters[Nsubj]&d$Condition=="Neg"] = d$Conduct[d$id==letters[Nsubj]&d$Condition=="Neg"]+0.6
d$Condition = factor(d$Condition,levels=c("Neut","Neg"))

# visualize data with boxplots
ggplot(data=d,aes(x=id,y=Conduct,color=Condition,fill=Condition))+geom_boxplot(size=1)+
  scale_fill_manual(values=c("#888888","#FF5555"))+
  scale_color_manual(values=c("#000000","#880000"))+
  theme(axis.title=element_text(size=25),axis.text=element_text(size=25),panel.grid.minor=element_blank(),
        text=element_text(size=18))+
  xlab("id")+ylab("Conductance")

#######

# LMER with random intercept alone
fit0 = lmer(Conduct~Condition+(1|id)+(1|item),data=d)
summary(fit0)

# influent cases in fit0: Cook's distances
cookdID = cooks.distance(influence.ME::influence(fit0,group="id"))
boxplot(cookdID,ylab="Cook's distance",pch=19,pars=list(boxlwd=3,whisklwd=3,staplelwd=3,outcex=2,cex.lab=2,cex.axis=1.5))

# influent cases in fit0: visualize leave-one-out range of variation of fixed effects
range = data.frame(id = levels(as.factor(d$id)), eff = NA, lower = NA, upper = NA)
for(i in 1:nrow(range)){
  fit0X = lmer(Conduct~Condition+(1|id)+(1|item),data=d[d$id!=range$id[i],])
  su = summary(fit0X)
  range$eff[i] = su$coefficients["ConditionNeg","Estimate"]
  stderr = su$coefficients["ConditionNeg","Std. Error"]
  range$lower[i] = range$eff[i] + stderr*qnorm(.025)
  range$upper[i] = range$eff[i] + stderr*qnorm(.975)
  # alternativa più precisa ma più lenta per i CIs
  # cis = confint(fit0X)
  # range$lower[i] = cis["ConditionNeg","2.5 %"]
  # range$upper[i] = cis["ConditionNeg","97.5 %"]
}
ggplot(range,aes(x=id,y=eff,ymin=lower,ymax=upper,group=1))+
  geom_hline(yintercept=0,size=1.5,linetype=2,color="red")+
  geom_hline(yintercept=fit0@beta[2],size=1.5,linetype=3)+
  geom_point(size=5)+geom_line(size=1.5)+
  geom_ribbon(alpha=.2,color="black")+
  theme(text=element_text(size=28),axis.text.x=element_text(size=32))+
  xlab("id (if removed)")+ylab("Fixed effect of Condition")

#######

# LMER with both random intercept and random slope for subject
fit1 = lmer(Conduct~Condition+(Condition|id)+(1|item),data=d)
summary(fit1)

# see caterpillar plot for random effects
lattice::dotplot(ranef(fit1),scales=list(cex=c(1.2)),cex=1.2)$id[1] # subject random intercepts
lattice::dotplot(ranef(fit1),scales=list(cex=c(1.2)),cex=1.2)$id[2] # subject random slopes

#################################


