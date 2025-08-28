
##############################
##############################

rm(list=ls())
library(brms)
library(lme4)
library(lmerTest)
library(effects)
library(ggplot2)

# simula dati per modello lineare
set.seed(212)
nSchool = 5; nSubj = 40
fixInt = 0; fixSlo = -0.40
randIntSD = 0.50
randSloSD = 0.20
sigma = 1
schoolID = rep(letters[1:nSchool],each=nSubj)
subjID = paste0(schoolID,"_",rep(1:nSubj,times=nSchool))
randInt = rep(rnorm(nSchool,0,randIntSD),each=nSubj)
randSlo = rep(rnorm(nSchool,0,randSloSD),each=nSubj)
anx = rnorm(nSchool*nSubj,0,1)
residual = rnorm(nSchool*nSubj,0,sigma)
math = fixInt + fixSlo*anx + randInt + randSlo*anx + residual
d = data.frame(schoolID,subjID,anx,math)

# visualizza dati per modello lineare
ggplot(d,aes(x=anx,y=math,group=schoolID,color=schoolID,fill=schoolID))+
  geom_smooth(method="lm",alpha=.2)+
  geom_point(size=3,alpha=.7)+
  theme(text=element_text(size=20))+
  scale_y_continuous(breaks=seq(-10,10,1))+
  scale_x_continuous(breaks=seq(-10,10,1))+
  ylab("Math (z)")+xlab("Anx (z)")

##############################
##############################

#### MODELLO LINEARE "FREQUENTISTA"

# modello con stima per maximum likelihood
fit = lmer(math ~ anx + (anx|schoolID), data=d)
summary(fit)

# visualizzazione "semplice" dell'effetto fisso
plot(allEffects(fit))

# estrazione dell'effetto fisso con "allEffects" e visualizzazione con ggplot
eff = data.frame(allEffects(fit)$"anx")
ggplot(eff,aes(x=anx,y=fit))+
  geom_smooth(data=d,aes(x=anx,y=math,group=schoolID,color=schoolID,fill=schoolID),method="lm",alpha=.2)+
  geom_line(size=1.5,linetype=2)+
  geom_ribbon(aes(ymin=lower,ymax=upper),alpha=.2,color="black",linetype=2,size=0.8)+
  geom_point(data=d,aes(x=anx,y=math,group=schoolID,color=schoolID,fill=schoolID),size=3,alpha=.7)+
  theme(text=element_text(size=20))+
  scale_y_continuous(breaks=seq(-10,10,1))+
  scale_x_continuous(breaks=seq(-10,10,1))+
  ylab("Math (z)")+xlab("Anx (z)")

##############################
##############################

#### MODELLO LINEARE "BAYESIANO"
  
# modello con stima MCMC, ma SENZA prior informative
fitB = brm(math ~ anx + (anx|schoolID), data=d)
fitB

# ripeto ma con un po' più di iterazioni, parallelizzando le catene (per velocizzare), e seguendo un suggerimento dato dal warning
fitB = brm(math ~ anx + (anx|schoolID), data=d, iter=4000, cores=4, control=list(adapt_delta=0.95))
fitB

# visualizzazione dell'effetto fisso dell'ansia
conditional_effects(fitB)

# estrazione dell'effetto da conditional effects e visualizzazione con ggplot
effB = data.frame(conditional_effects(fitB)$"anx")
ggplot(effB,aes(x=anx,y=estimate__))+
  geom_point(data=d,aes(x=anx,y=math,group=schoolID,color=schoolID,fill=schoolID),size=3,alpha=.7)+
  geom_smooth(data=d,aes(x=anx,y=math,group=schoolID,color=schoolID,fill=schoolID),method="lm",alpha=.2)+
  geom_line(size=1.5,linetype=2)+
  geom_ribbon(aes(ymin=lower__,ymax=upper__),alpha=.2,color="black",linetype=2,size=0.8)+
  theme(text=element_text(size=20))+
  scale_y_continuous(breaks=seq(-10,10,1))+
  scale_x_continuous(breaks=seq(-10,10,1))+
  ylab("Math (z)")+xlab("Anx (z)")

# controlla quanto bene le posterior dei parametri riproducono la distribuzione dei dati osservati
pp_check(fitB,ndraws=30)

# estrai e visualizza le prime righe dei campionamenti dalla posterior
# post = data.frame(as_draws_matrix(fitB)) # alternativo
post = as.data.frame(fitB)
head(post)

# plot della posterior dell'effetto fisso dell'ansia
plot(density(post$b_anx)) # plot semplice
ggplot(post,aes(x=b_anx,y=after_stat(density)))+
  geom_density(fill="blue",alpha=.3)+
  geom_vline(xintercept=0,linetype=2,size=1)+
  theme(text=element_text(size=30))
  
# test di ipotesi con evidence ratio che l'effetto dell'ansia sia negativo
hypothesis(fitB,"anx < 0")

# ricalcolo dell'evidence ratio usando la posterior
sum(post$b_anx < 0) / sum(post$b_anx >= 0)

# stima media e 95%BCI (coi quantili) della posterior dell'effetto fisso dell'ansia
mean(post$b_anx)
quantile(post$b_anx, probs=c(.025,.975))

# guardo le prior (di default) del modello
prior_summary(fitB)

# setto un paio di prior sull'eterogeneità e rifitto il modello
pr1 = set_prior("gamma(2,4)", class="sd", coef="Intercept", group="schoolID")
pr2 = set_prior("gamma(1,2)", class="sd", coef="anx", group="schoolID")
pr3 = set_prior("lkj_corr_cholesky(4)", class="L")
fitB1 = brm(math ~ anx + (anx|schoolID), data=d, prior=c(pr1,pr2,pr3), iter=4000)
fitB1
prior_summary(fitB1)

# ricalcolo il test di ipotesi sulla posterior 
hypothesis(fitB1,"anx < 0")
post1 = data.frame(as_draws_matrix(fitB1))
plot(density(post1$b_anx))

##############################
##############################

#### META-ANALISI

# simula dei dati
set.seed(900)
k = 5
dm = data.frame(study=letters[1:k],
                eff=rnorm(k,0.5,0.3),
                vi=.03+rgamma(k,.1))
dm$sei = sqrt(dm$vi)
dm

# modello meta-analitico frequentista
library(metafor)
fitMA = rma(yi=eff,vi=vi,data=dm)
summary(fitMA)
metafor::forest(fitMA)

# modello meta-analitico bayesiano
fitMA_B = brm( eff | se(sei) ~ 1 + (1|study), data=dm, iter=5000)
fitMA_B

# posterior drawing
post = as.data.frame(fitMA_B)
ggplot(post,aes(x=sd_study__Intercept,y=after_stat(density)))+
  geom_density(fill="blue",alpha=.3,size=1)+
  scale_x_continuous(breaks=seq(0,10,0.5))+
  coord_cartesian(xlim=c(0,2))+
  theme(text=element_text(size=30))+xlab("Tau (posterior)")

# forest plot bayesiano (pacchetto non più mantenuto)
remotes::install_github("mvuorre/brmstools")
library(brmstools)
brmstools::forest(fitMA_B)

# forest plot bayesiano dei random effects
effR = data.frame(ranef(fitMA_B)$"study")
ggplot(effR,aes(y=rownames(effR),x=Estimate.Intercept,xmin=Q2.5.Intercept,xmax=Q97.5.Intercept))+
  geom_point(size=3)+
  geom_errorbar(size=1,width=0)+
  theme(text=element_text(size=20))+
  ylab("Study")+xlab("Estimated effect")+
  geom_vline(xintercept=0,size=1,linetype=2)


# modello meta-analitico bayesiano con prior debolmente informative
pr1 = set_prior("normal(0, 1)",class="Intercept",group="")
pr2 = set_prior("gamma(2, 4)",class="sd",group="study")
fitMA_B_info = brm( eff | se(sei) ~ 1 + (1|study), data=dm, prior=c(pr1,pr2), iter=5000)
fitMA_B_info

# (info) posterior drawing
postinfo = as.data.frame(fitMA_B_info)
ggplot(postinfo,aes(x=b_Intercept,y=after_stat(density)))+
  geom_density(fill="blue",alpha=.3,size=1,bw=.08)+
  scale_x_continuous(limits=c(-3,3),breaks=seq(-10,10,1))+
  geom_vline(xintercept=0,size=1,linetype=2)+
  theme(text=element_text(size=30))+xlab("Intercept (posterior)")
ggplot(postinfo,aes(x=sd_study__Intercept,y=after_stat(density)))+
  geom_density(fill="blue",alpha=.3,size=1)+
  scale_x_continuous(breaks=seq(0,10,0.5))+
  coord_cartesian(xlim=c(0,2))+
  theme(text=element_text(size=30))+xlab("Tau (posterior)")

##############################
##############################

