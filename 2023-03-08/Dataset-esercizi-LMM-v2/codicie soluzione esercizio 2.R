
##### PACCHETTI #####
require(lme4)
require(lmerTest)
require(ggplot2)
require(lattice)

###### OPEN THE DATA #####
rm(list = ls())
db <- read.csv("esercizio2.csv",sep=";",header=T) #ci mette qualche secondo

###### CAMPIONO SCUOLE E ANNI ######
length(unique(db$idSchool)) #quante scuole ci sono
length(unique(db$idClass))  #quante classi
length(unique(db$Year))  #quanti anni
table(db$Gender)

#il dataset ? composto da 15 variabili
head(db) 

library(corrplot)
db$Male = ifelse(db$Gender=="M",1,0)
(ctab <- round(cor(db[,c("Male", "age", "ansia", "geom")]),2))

corrplot(ctab)
#Plotto
ggplot(db, aes(y = geom, x = Male, group = as.factor(Year), 
              color = as.factor(Year))) +
  geom_smooth(method = "lm", se = F) +
  stat_summary(fun=mean, geom=c("point"), 
               aes(group = Year, color = as.factor(Year)), 
               size = 3) +
  stat_summary(fun.data=mean_se, geom=c("errorbar"), 
               aes(group = Year, color = as.factor(Year)), 
               width = .05, size = 1) +
  scale_color_discrete(name = "Year") +
  scale_x_continuous(breaks = c(0,1)) + 
  xlab("Female/Male") +
  theme_bw()

ggplot(db[1:2100,], aes(y = geom, 
                        group = as.factor(idClass), 
                        fill = as.factor(idSchool))) +
  geom_boxplot() +
  labs(title = "Intercette delle classi in alcune scuole") +
  facet_wrap(~as.factor(idSchool), ncol = 5) +
  theme_bw()

ggplot(db[1:2100,], aes(y = geom, x = Male, 
                        group = as.factor(idClass), 
                        color = as.factor(idSchool))) +
  geom_smooth(method = "lm", se = F) +
  labs(title = "Slope delle classi in alcune scuole") +
  facet_wrap(~as.factor(idSchool), ncol = 5) +
  theme_bw()

#Modelli
#Fitto
fit0 <- lm(geom~Male, data = db)
fit <- lmer(geom ~ Male + (Male|Year/idSchool/idClass), data = db, 
            control=lmerControl(optimizer="bobyqa",optCtrl = list(maxfun=2e4)))

summary(fit0)
summary(fit)

#Random effects
lattice::dotplot(ranef(fit))
lattice::dotplot(coef(fit))


