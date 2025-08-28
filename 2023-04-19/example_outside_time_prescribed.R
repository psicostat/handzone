rm(list=ls())

## Example: outside prescribed time

library(emmeans)
library(effects)
library(ggplot2)

# Upload data
mydf=read.table(file.choose(),sep = ";",dec = ".",header = T)
str(mydf)
## We are interested in testing the following differences:
# - Attending Life science (LS) vs Social Science & Humanities (SH) faculties is associated with different 
#   chances of being outside prescribed time
# - Different chances should be observed also within each macroarea:
#    1) Giurisprudenza (Law) vs Scieze Umane (Human Sciences)
#    2) Agraria (Agricultural Sciences) Vs Sciense Naturali (Natural Sciences)

## Let's use the default contrasts in R (treatment contrasts)
model_default=glm(FuoriCorso~Corso,data=mydf,family="binomial")

## The output
summary(model_default)

## odds ratios and 95% CI
exp(cbind(OR = coef(model_default), confint(model_default)))

## The first plot
eff1 = data.frame(allEffects(model_default)$"Corso")
(gg = ggplot(data=eff1,aes(y=fit,x=Corso,ymin=lower,ymax=upper,group=1))+
    geom_point(size=7)+geom_line(size=1.3)+geom_errorbar(size=1.3,width=.3)+
    theme(text=element_text(size=28))+
    ylab("Probabilità stimata fuori corso")
)


## Since it is not exaclty what we want, we may be tempted to use pots-hoc comparisons
pairs(emmeans(model_default,~Corso))

## Let's examine in details the default contrast matrix
mydf$Corso=factor(mydf$Corso)
contrasts(mydf$Corso)

## Let's assign the contrast matrix we are looking for (repreated contrasts)
contrasts(mydf$Corso,how.many=3)=matrix( c( 1/2,1/2,-1/2,-1/2,1/2,-1/2,0,0,0,0,1/2,-1/2 ) , 4 , 3 )
contrasts(mydf$Corso)


## To examine the hypothesis matrix
round(MASS::ginv(contrasts(mydf$Corso)),2)

## RUn the right model
model_contr=lm(FuoriCorso~Corso,data=mydf)
summary(model_contr)

## odds ratios and 95% CI
exp(cbind(OR = coef(model_contr), confint(model_contr)))

## The final plot
eff2 = data.frame(allEffects(model_contr)$"Corso")
(gg = ggplot(data=eff2,aes(y=fit,x=Corso,ymin=lower,ymax=upper,group=1))+
    geom_point(size=7)+geom_line(size=1.3)+geom_errorbar(size=1.3,width=.3)+
    theme(text=element_text(size=28))+
    ylab("Probabilità stimata fuori corso")
)


summary(model_contr)$coefficients
summary(model_contr)$coefficients[,4]
p.adjust(summary(model_contr)$coefficients[,4],method = "bonferroni")
round(p.adjust(summary(model_contr)$coefficients[,4],method = "bonferroni"),3)
