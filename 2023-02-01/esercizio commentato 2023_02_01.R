
#####################################

# ESERCIZIO: ALTRO MODO PER SIMULARE DATI CON GENERE x MATH ANXIETY

#####################################

rm(list=ls())
library(MASS)
library(lavaan)
library(ggplot2)
library(effects)

#####################################

## SIMULATE POPULATIONS

# Simulate Females
# create correlation matrix with 2 variables (r = -0.30)
corF = lav_matrix_lower2full(c(
  1,
  -.30,1
)) 
colnames(corF) = rownames(corF) = c("MathAnxiety","MathAchievement") # set variable names
FF = data.frame( mvrnorm(n=1e6,mu=c(+.20,-.12),Sigma=corF) ) # generate n = 1 million observations
FF$Gender = "F" # add info on gender

# Simulate males
# create correlation matrix with 2 variables (r = -0.20)
corM = lav_matrix_lower2full(c(
  1,
  -.20,1
))
colnames(corM) = rownames(corM) = c("MathAnxiety","MathAchievement") # set variable names
MM = data.frame( mvrnorm(n=1e6,mu=c(-.20,+.12),Sigma=corM) ) # generate n = 1 million observations
MM$Gender = "M" # add info on gender

# Merge female and male populations to get only one combined population
pop = rbind(FF,MM)

#####################################

## SIMULATE STUDY

# Set sample size
N = 500

# Sample data from the previously created population
d = pop[ sample(nrow(pop),N) , ]

# Model comparisons
# first test the main effect of MathAnxiety (irrespective of gender)
fit0 = lm(MathAchievement ~ 1, data=d)
fit1 = lm(MathAchievement ~ MathAnxiety, data=d)
anova(fit0,fit1)
# now add the main effect of gender and test it
fit2 = lm(MathAchievement ~ MathAnxiety + Gender, data=d)
anova(fit1,fit2)
# now add the interaction between the variables and test it
fit3 = lm(MathAchievement ~ MathAnxiety * Gender, data=d)
anova(fit2,fit3)
# now perform model comparisons using AIC (Akaike Information Criterion) and BIC (Bayesian Information Criterion)
# (in both cases, lower is better)
AIC(fit0,fit1,fit2,fit3)
BIC(fit0,fit1,fit2,fit3)

# see summary of model with the interaction
summary(fit3)

# Plot the interaction, simple way
plot( allEffects(fit3), multiline=T,ci.style="bands",colors=c("red","black") )

# Plot interaction in more complex way with ggplot
eff = data.frame(allEffects(fit3)$"MathAnxiety:Gender")
eff$Gender = factor(eff$Gender,levels=c("F","M"))
ggplot(eff,aes(x=MathAnxiety,y=fit,group=Gender,color=Gender,fill=Gender))+
  geom_ribbon(aes(ymin=lower,ymax=upper),alpha=.3,color=NA)+
  geom_line(linewidth=1)+
  scale_color_manual(values=c("red","black"))+
  scale_fill_manual(values=c("red","black"))+
  theme(text=element_text(size=18))+
  ylab("Math Achievement") + xlab("Math Anxiety")+
  coord_cartesian(xlim=c(-2,2),ylim=c(-2,2))+
  if(nrow(d)<=1000){geom_point(data=d,aes(y=MathAchievement),size=2,alpha=.5)
  }else{geom_point(data=d[sample(nrow(d),1000),],aes(y=MathAchievement),size=2,alpha=.5)}

#####################################

