
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

# stabilisco i parametri generali della simulazione
N = 500 # numero di partecipanti
n_days = 10 # numero di trial/giorni di deprivazione di sonno
fixInt = 0.0
fixSlope = 0.3
randInt_sd = 1.0
randSlope_sd = 0.1
residual_sd = 0.5

# inizializzo il dataset
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

# simulo i dati delle risposte
d$rt = d$fixInt + d$fixSlope * d$day + d$randInt + d$randSlope * d$day + d$residual

# fitto il modello e vedo il summary
fit = lmer(rt ~ day + (day | id), data = d)
summary(fit)

##############################################
##############################################

## CASO 2: Multilab - LMM con effetti random dei laboratori

# stabilisco i parametri generali della simulazione
k = 10 # numero di laboratori
n = 100 # numero di partecipanti per laboratorio
fixInt = 0.0
fixSlope = -0.5
randInt_sd = 0.3
randSlope_sd = 0.2
residual_sd = 1

# inizializzo il dataset
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

# simulo i dati delle risposte
d$y = d$fixInt + d$fixSlope * d$clinicalstatus + d$randInt + d$randSlope * d$clinicalstatus + d$residual

# fitto il modello e vedo il summary
fit = lmer(y ~ clinicalstatus + (clinicalstatus|lab), data=d)
summary(fit)

######

## Multilab ri-analizzato come meta-analisi

library(dplyr)
library(tidyr)
library(metafor)

# aggrego i dati per lab usando funzioni di tidyverse
d_agg = d |>
  group_by(lab, clinicalstatus) |>
  summarise(m = mean(y), s = sd(y), n = n()) |>
  pivot_wider(names_from = clinicalstatus,
              values_from = c(m, s, n))
d_agg = metafor::escalc(measure="SMD",
                m1i=m_1, m2i=m_0,
                sd1i=s_1, sd2i=s_0,
                n1i=n_1,  n2i=n_0,
                data = d_agg)
# OPPURE
# aggrego i dati per lab usando funzioni di base
d_agg = data.frame(lab=levels(as.factor(d$lab)), yi=NA, vi=NA)
d_agg$lab = as.factor(d_agg$lab)
for(i in levels(d_agg$lab)){
  dlab = d[d$lab==i,]
  es = metafor::escalc(measure="SMD",
                       m1i = mean(dlab$y[dlab$clinicalstatus==1]),
                       m2i = mean(dlab$y[dlab$clinicalstatus==0]),
                       sd1i = sd(dlab$y[dlab$clinicalstatus==1]),
                       sd2i = sd(dlab$y[dlab$clinicalstatus==0]),
                       n1i = length(dlab$y[dlab$clinicalstatus==1]),
                       n2i = length(dlab$y[dlab$clinicalstatus==0])
  )
  d_agg$yi[d_agg$lab==i] = es$yi
  d_agg$vi[d_agg$lab==i] = es$vi
}

# fittiamo il modello di meta-analisi e vediamo il forest plot
fitMeta = rma(yi,vi,data=d_agg)
forest(fitMeta, xlab="Standardized Mean Difference")

##############################################
##############################################

