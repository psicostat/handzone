
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

##############################################
##############################################

## CASO 1: Singolo studio - LMM con effetti random dei soggetti

N = 500
n_days = 10
fixInt = 0
fixSlope = -0.2
randInt_sd = 1
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
summary(fit)

##############################################
##############################################

## CASO 2: Multilab - LMM con effetti random dei laboratori

k = 100
n = 500
fixInt = 0
fixSlope = -0.5
randInt_sd = 0.3
randSlope_sd = 0.2
residual_sd = 1

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

fit = lmer(y ~ clinicalstatus + (clinicalstatus|lab), data=d)
summary(fit)

##############################################
##############################################

