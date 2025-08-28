
##############################################
##############################################

# ESERCIZI

# ESERCIZIO (A): nei due casi, capire il significato dei parametri coi quali si stanno 
# simulando i dati, e ritrovare questi parametri nel summary del modello;
# plottare gli effetti fissi e random

# ESERCIZIO (B): nel Caso 2 (multilab) fare una power analysis con ciclo "for"
# e decidere qual è il miglior bilanciamento tra numero di soggetti e numero di lab
# secondo diversi scenari di effect size e di eterogeneità dell'effetto

# ESERCIZIO (C): nel Caso 1 (soggetti) risimulare i dati correlando intercetta e slope random
# con un certo valore r prefissato; è possibile usare la funzione "mvrnorm" del pacchetto MASS
# per simulare valori di intercetta e slope correlati

# ESERCIZIO (D): nel Caso 2 (multilab) simulare dati con numero variabile di soggetti
# per ciascun laboratorio (scegliendo una distribuzione di probabilità di n)

##############################################
##############################################

rm(list=ls())
library(ggplot2)
library(effects)
library(lme4)
library(lmerTest)
library(MASS)
library(lattice)

##############################################
##############################################

## ESERCIZIO (A) RISOLTO: PLOTTARE GLI EFFETTI FISSI E RANDOM
## CASO 1: Singolo studio - LMM con effetti random dei soggetti

N = 20
n_days = 8
fixInt = 0.0
fixSlope = 0.3
randInt_sd = 1.0
randSlope_sd = 0.1
residual_sd = 0.5

d = data.frame(id = rep(1:N, each = n_days),
               day = rep(1:n_days, times = N),
               rt = NA,
               fixInt = NA,
               fixSlope = NA,
               randInt = NA,
               randSlope = NA,
               residual = NA )
d$fixInt = fixInt
d$fixSlope = fixSlope
d$randInt = rep(rnorm(N, 0, randInt_sd), each = n_days)
d$randSlope = rep(rnorm(N, 0, randSlope_sd), each = n_days)
d$residual = rnorm(N*n_days, 0, residual_sd)

d$rt = d$fixInt + d$fixSlope * d$day + d$randInt + d$randSlope * d$day + d$residual

fit = lmer(rt ~ day + (day | id), data = d)

# plot effetto fisso con dati individuali
eff = data.frame(allEffects(fit,x.levels=list(day=1:n_days))$"day")
d$id = as.factor(d$id)
ggplot(data=d,aes(x=day,y=rt))+
  geom_point(aes(color=id,group=id),size=3)+
  geom_line(aes(color=id,group=id),size=1)+
  geom_line(data=eff,aes(y=fit),size=1.5)+
  geom_ribbon(data=eff,aes(y=fit,ymin=lower,ymax=upper),alpha=.3)+
  theme(text=element_text(size=20))

# plot semplice effetto fisso
plot(allEffects(fit))

# plot semplice effetti random
lattice::dotplot(ranef(fit))

##############################################
##############################################

## ESERCIZIO (B) RISOLTO: MULTILAB - POWER ANALYSIS CON CICLO FOR (per vari k ed n)
## CASO 2: Multilab - LMM con effetti random dei laboratori

k = seq(4,10,2)
n = seq(20,100,20)

tab = expand.grid(k=k, n=n)

tab$fixInt = 0.0
tab$fixSlope = -0.5
tab$randInt_sd = 0.3
tab$randSlope_sd = 0.2
tab$residual_sd = 1.0

tab$power = NA

iterations = 100

for(row in 1:nrow(tab)){
  
  pvalue = rep(NA,iterations)
  
  for(i in 1:iterations){
    
    k = tab$k[row]
    n = tab$n[row]
    fixInt = tab$fixInt[row]
    fixSlope = tab$fixSlope[row]
    randInt_sd = tab$randInt_sd[row]
    randSlope_sd = tab$randSlope_sd[row]
    residual_sd = tab$residual_sd[row]
    
    d = data.frame(lab = rep(1:k, each = n*2),
                   clinicalstatus = rep(c(0, 1), each = n, times = k),
                   y = NA,
                   fixInt = NA,
                   fixSlope = NA,
                   randInt = NA,
                   randSlope = NA,
                   residual = NA )
    d$fixInt = fixInt
    d$fixSlope = fixSlope
    d$randInt = rep(rnorm(k, 0, randInt_sd), each = n*2)
    d$randSlope = rep(rnorm(k, 0, randSlope_sd), each = n*2)
    d$residual = rnorm(n*2*k, 0, residual_sd)
    
    d$y = d$fixInt + d$fixSlope * d$clinicalstatus + d$randInt + d$randSlope * d$clinicalstatus + d$residual
    
    tryCatch({
      fit = lmer(y ~ clinicalstatus + (clinicalstatus|lab), data=d)
      pvalue[i] = anova(fit)$"Pr(>F)"
    },error=function(e){})
  }
  tab$power[row] = mean(pvalue<0.05, na.rm=T)
  
}

tab
tab$k = as.factor(tab$k)
tab$n = as.factor(tab$n)
ggplot(tab,aes(x=n,y=power,group=k,color=k))+
  geom_point(size=3)+
  geom_line(size=1)+
  theme(text=element_text(size=24))


##############################################
##############################################

## ESERCIZIO (C) RISOLTO: SIMULARE INTERCETTE E SLOPE CORRELATE
## CASO 1: Singolo studio - LMM con effetti random dei soggetti

N = 200
n_days = 10
fixInt = 0.0
fixSlope = 0.3
randInt_sd = 1.0
randSlope_sd = 0.1
residual_sd = 0.5

rIntSlope = .5

x = MASS::mvrnorm(n = N, 
                  mu = c(0,0), 
                  Sigma = matrix(c(1,rIntSlope,
                                   rIntSlope,1),nrow=2,ncol=2), 
                  empirical=T)
x = data.frame(x)
colnames(x) = c("RandInt","RandSlope")
x$RandInt = x$RandInt * randInt_sd
x$RandSlope = x$RandSlope * randSlope_sd
cor(x)
print(x)

d = data.frame(id = rep(1:N, each = n_days),
               day = rep(1:n_days, times = N),
               rt = NA,
               fixInt = NA,
               fixSlope = NA,
               randInt = NA,
               randSlope = NA,
               residual = NA )
d$fixInt = fixInt
d$fixSlope = fixSlope
d$randInt = rep(x$RandInt, each = n_days)
d$randSlope = rep(x$RandSlope, each = n_days)
d$residual = rnorm(N*n_days, 0, residual_sd)

d$rt = d$fixInt + d$fixSlope * d$day + d$randInt + d$randSlope * d$day + d$residual

fit = lmer(rt ~ day + (day | id), data = d)
summary(fit)

##############################################
##############################################

## ESERCIZIO (D) RISOLTO: MULTILAB CON n DI SOGGETTI VARIABILE PER LAB
## CASO 2: Multilab - LMM con effetti random dei laboratori

k = 10
n_base = 20
n_medio_aggiunto = 40
fixInt = 0.0
fixSlope = -0.5
randInt_sd = 0.3
randSlope_sd = 0.2
residual_sd = 1.0

simulaStudioLab = function(idLab = NA, 
                           n = NA, 
                           Intercept = NA,
                           Slope = NA,
                           residual_sd = NA){
  df = data.frame(idLab = idLab,
                 clinicalstatus = rep(c(0, 1), each = n),
                 y = NA,
                 Intercept = Intercept,
                 Slope = Slope,
                 residual = rnorm(n, 0, residual_sd) )
  df$y = df$Intercept + df$Slope * df$clinicalstatus + df$residual
  return(df[,c("idLab","clinicalstatus","y")])
}

d = data.frame(idLab=NA, clinicalstatus=NA, y=NA)
for(i in 1:k){
  df = simulaStudioLab(idLab=i, 
                       n=n_base+rpois(1,n_medio_aggiunto),
                       Intercept = fixInt + rnorm(1,0,randInt_sd),
                       Slope = fixSlope + rnorm(1,0,randSlope_sd),
                       residual_sd = residual_sd)
  d = rbind(d,df)
}

fit = lmer(y ~ clinicalstatus + (clinicalstatus|idLab), data=d)
summary(fit)

##############################################
##############################################


