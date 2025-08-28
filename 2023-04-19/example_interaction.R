rm(list=ls())

## Example: Interaction

library(emmeans)
library(effects)
library(ggplot2)
library(lmerTest)

# Upload data
mydf=read.table(file.choose(),sep = ";",dec = ",",header = T)

## We are interested in testing the following differences:
# - Bambini vs Ragazzi
# - Ragazzi vs Adulti
# - Adulti vs Anziani

## Let's use the default contrasts in R (treatment contrasts)
model_default=lmer(Weight~Treat*Time+(1|subj),data=mydf)

## The output
summary(model_default)

## The first plot
eff1 = data.frame(effect("Treat:Time",model_default))
pd = position_dodge(width=.3)
(gg = ggplot(eff1,aes(y=fit,x=Treat,group=Time,color=Time,linetype=Time,shape=Time))+
    geom_point(size=8,position=pd)+
    geom_line(linewidth=1.5,position=pd)+
    geom_errorbar(aes(ymin=lower,ymax=upper),linetype=1,linewidth=1.5,width=.2,position=pd)+
    theme(text=element_text(size=26))+
    xlab("Condition")+ylab("Score")
)


## Since it is not exaclty what we want, we may be tempted to use pots-hoc comparisons
pairs(emmeans(model_default,~Treat*Time))

## Let's examine in details the default contrast matrix
mydf$Treat=factor(mydf$Treat,levels = c("Cont","CBT","FT"))
contrasts(mydf$Treat)

mydf$Time=factor(mydf$Time,levels=c("PRE","POST"))
contrasts(mydf$Time)

## Let's assign the contrast matrix we are looking for (repreated contrasts)
contrasts(mydf$Treat)=matrix(c( 2, 0,
                               -1, 1,
                               -1,-1),3,2,byrow=T)
contrasts(mydf$Treat)

contrasts(mydf$Time)=contr.sum(2)/2
contrasts(mydf$Time)


## RUn the right model
model_contr=lmer(Weight~Treat*Time+(1|subj),data=mydf)
summary(model_contr)

eff2 = data.frame(effect("Treat:Time",model_contr))
pd = position_dodge(width=.3)
(gg = ggplot(eff2,aes(y=fit,x=Treat,group=Time,color=Time,linetype=Time,shape=Time))+
    geom_point(size=8,position=pd)+
    geom_line(linewidth=1.5,position=pd)+
    geom_errorbar(aes(ymin=lower,ymax=upper),linetype=1,linewidth=1.5,width=.2,position=pd)+
    theme(text=element_text(size=26))+
    xlab("Condition")+ylab("Score")
)

## If we want to correct the p values
summary(model_contr)$coefficients
summary(model_contr)$coefficients[,5]
p.adjust(summary(model_contr)$coefficients[,5],method = "bonferroni")
round(p.adjust(summary(model_contr)$coefficients[,5],method = "bonferroni"),3)
