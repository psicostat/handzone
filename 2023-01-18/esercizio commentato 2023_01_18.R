
###########################

# install.packages("effects") # installo il pacchetto effects se non ce l'ho già
rm(list=ls()) # svuoto il workspace prima di iniziare 
library(effects) # carico il pacchetto effects

###########################

# stabilisco la numerosità del campione
N = 10000

# simulo anxiety da una distribuzione normale
anx = rnorm(n=N, mean=24.5, sd=6.2)

# simulo gender con dati binomiali 0/1
# in alternativa: gender = sample(c(0,1),N,replace=T)
gender = rbinom(n=N, size=1, prob=.5)

# simulo la distribuzione del termine di errore (conoscendo il sigma)
Error = rnorm(n=N, mean=0, sd=10.05)

# genero l'achievement combinando parametri noti e predittori
ach = + 59.56 - 1.28*gender - 0.46*anx + 0.11*gender*anx + Error

# metto tutto in un data frame
d = data.frame(ach,gender,anx)

# ricodifico 0/1 come f/m per praticità negli step successivi
d$gender = ifelse(d$gender==0,"f","m")

# stimo modelli CON vs SENZA l'interazione di interesse e li confronto
fit0 = lm(ach ~ gender + anx, data=d)
fit1 = lm(ach ~ gender * anx, data=d)
anova(fit0, fit1)
# do un'occhiata anche ai BIC e agli AIC dei modelli (minore è meglio)
BIC(fit0, fit1)
AIC(fit0,fit1)

# il modello CON l'interazione è risultato vincente, quindi guardo i parametri
summary(fit1)

# modo semplice per visualizzare gli effetti del modello con interazione
plot( allEffects(fit1) , multiline=T, ci.style="bands")

###########################
