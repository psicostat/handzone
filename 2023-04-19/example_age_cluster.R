rm(list=ls())

## Example: Age clusters

library(emmeans)
library(effects)
library(ggplot2)

# Upload data
mydf=read.table(file.choose(),sep = ";",dec = ".",header = T)

## We are interested in testing the following differences:
# - Bambini vs Ragazzi
# - Ragazzi vs Adulti
# - Adulti vs Anziani

## Let's use the default contrasts in R (treatment contrasts)
model_default=lm(Score~GruppoAge,data=mydf)

## The output
summary(model_default)

## The first plot
eff1 = data.frame(allEffects(model_default)$"GruppoAge")
(gg = ggplot(data=eff1,aes(y=fit,x=GruppoAge,ymin=lower,ymax=upper,group=1))+
    geom_point(size=7)+geom_line(size=1.3)+geom_errorbar(size=1.3,width=.3)+
    theme(text=element_text(size=28))+
    ylab("Estimated mean score")
)

## Since it is not exaclty what we want, we may be tempted to use pots-hoc comparisons
pairs(emmeans(model_default,~GruppoAge))

## Let's examine in details the default contrast matrix
mydf$GruppoAge=factor(mydf$GruppoAge)
contrasts(mydf$GruppoAge)

## Let's assign the contrast matrix we are looking for (repreated contrasts)
contrasts(mydf$GruppoAge)=MASS::contr.sdif(4)
contrasts(mydf$GruppoAge)

## To examine the hypothesis matrix
round(MASS::ginv(contrasts(mydf$GruppoAge)))

## RUn the right model
model_contr=lm(Score~GruppoAge,data=mydf)
summary(model_contr)

## Let's exmaine the raw means
with(mydf,tapply(Score, GruppoAge, mean))

38.51-33.63
43.87-38.51
40.41-43.87

## The final plot
eff2 = data.frame(allEffects(model_contr)$"GruppoAge")
(gg = ggplot(data=eff2,aes(y=fit,x=GruppoAge,ymin=lower,ymax=upper,group=1))+
    geom_point(size=7)+geom_line(size=1.3)+geom_errorbar(size=1.3,width=.3)+
    theme(text=element_text(size=28))+
    ylab("Estimated mean score")
)

## If we want to correct the p values
summary(model_contr)$coefficients
summary(model_contr)$coefficients[,4]
p.adjust(summary(model_contr)$coefficients[2:4,4],method = "bonferroni")
round(p.adjust(summary(model_contr)$coefficients[,4],method = "bonferroni"),3)
