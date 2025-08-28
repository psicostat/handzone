
#########################################

rm(list=ls())
library(lme4)
library(lmerTest)
library(truncnorm)
library(effects)
library(ggplot2)
logit2prob <- function(logit){
  odds <- exp(logit)
  prob <- odds / (1 + odds)
  return(prob)
}

#########################################

# ESERCIZIO 1A: PRE-POST RIGENERATIVO

set.seed(4481)

N = 16

id = rep(1:(N*2),times=2)
time = rep(0:1,each=N*2)
group = rep(0:1,each=N,times=2)

rInt = rep(rtruncnorm(N*2,a=-1,b=2,0,2),times=2)

digitSpan = 6 + 1*time + 0*group + 0.5*time*group + rInt + rtruncnorm(N*2*2,a=-2,b=2,mean=0,sd=.6)
digitSpan = round(digitSpan)
hist(digitSpan)

d = data.frame(id,group,time,digitSpan)
d$group = ifelse(d$group==0,"Control","Restorative")
d$time = ifelse(d$time==0,"0.Pre","1.Post")
d$id = paste0(substr(d$group,1,2),d$id)

fit = lmer(digitSpan ~ time*group+(1|id), data=d)
summary(fit)

plot(allEffects(fit))

write.table(d,"esercizio1A.csv",sep=";",row.names=F)

#########################################

# ESEMPIO 1B: TREATMENT EFFICACY IN DYSLEXIA

set.seed(3081)

N = 16
nMeasures = 6

id = rep(1:(N*2),each=nMeasures,times=2)
time = rep(0:1,each=N*nMeasures*2)
group = rep(0:1,each=N*nMeasures,times=2)

rInt = rep(rnorm(N*2,0,.3),each=nMeasures,times=2)
rSloTime = rep(rnorm(N*2,0,.1),each=nMeasures,times=2)
rSloTimeGroup = rep(rgamma(N*2,2,2)/6,each=nMeasures,times=2)
mean(rSloTimeGroup);sd(rSloTimeGroup)

read_z = -2.5 + 0.1*time + 0*group + 0*time*group + rInt + rSloTime*time + rSloTimeGroup*time*group + rnorm(N*nMeasures*2*2,0,.3)

d = data.frame(id,time,group,read_z)
d$time = ifelse(d$time==0,"0.Pre","1.Post")
d$group = ifelse(d$group==0,"Control","Treated")
d$id = paste0(substr(d$group,1,2),d$id)
d$time = as.factor(d$time)
d$group = as.factor(d$group)
fit = lmer(read_z~time*group+(time|id),data=d)
summary(fit)

plot(allEffects(fit))

# visualize ranef
re = data.frame(ranef(fit))
re$group = NA
for(i in 1:nrow(re)) re$group[i] = as.character(d$group[d$id==re$grp[i]][1])
re$group = as.factor(re$group)
ggplot(re,aes(x=condval,y=grp,color=group))+theme_bw()+
  geom_point(size=2)+
  geom_errorbar(size=0.7,aes(xmin=condval+condsd*qnorm(.025),xmax=condval+condsd*qnorm(.975)))+
  theme(text=element_text(size=16),axis.text.y=element_text(size=11))+
  facet_wrap(~term)

# visualize coef
lattice::dotplot(coef(fit))

write.table(d,"esercizio1B.csv",sep=";",row.names=F)


#########################################

# ESEMPIO 1x: TREATMENT EFFICACY WITH RT

# (seed = round(runif(1,10,10000)))
# set.seed(seed)
# 
# N = 20
# nTrials = 50
# 
# id = rep(1:(N*2),each=nTrials,times=2)
# time = rep(0:1,each=N*nTrials*2)
# group = rep(0:1,each=N*nTrials,times=2)
# 
# rInt = rep(rnorm(N*2,0,.2),each=nTrials,times=2)
# rSloTime = rep(rnorm(N*2,0,.10),each=nTrials,times=2)
# rSloTimeGroup = rep(rnorm(N*2,0,.15),each=nTrials,times=2)
# 
# RT = 2 - 0.2*time + 0*group - 0.4*time*group + rInt + rSloTime*time + rSloTimeGroup*time*group
# avg = mean(exp(RT))
# 
# for(i in 1:length(RT)){
#   RT[i] = 10*(exp(RT[i])+rgamma(1,avg,1))
# }
# hist(RT)
# 
# d = data.frame(id,time,group,RT)
# d$time = as.factor(d$time)
# d$group = as.factor(d$group)
# fitG = glmer(RT~time*group+(time|id),data=d,family=Gamma(link=log))
# summary(fitG)
# fit = lmer(RT~time*group+(time|id),data=d)
# summary(fit)
# plot(allEffects(fit))


#########################################

# ESERCIZIO 3: PIACEVOLEZZA FOTOGRAFIE NATURA

set.seed(4171)

N = 200
nPictures = 20 * 2 # 2 are conditions

idSubj = rep(1:N,each=nPictures)
age = rep(runif(N,6,16),each=nPictures)
gender = rep(rbinom(N,1,.5),each=nPictures)
condition = rep(0:1,each=nPictures/2,times=N)
idPicture = rep(1:nPictures,times=N)

rIntSubj = rep(rtruncnorm(N,a=-2,b=2,mean=0,sd=0.8),each=nPictures)
rSloSubjCond = rep(rtruncnorm(N,a=-2,b=2,mean=0,sd=.3),each=nPictures)
rIntPict = rep(rtruncnorm(nPictures,a=-2,b=2,mean=0,sd=1.1),times=N)
rSloPictAge = rep(rtruncnorm(nPictures,a=-1,b=1,mean=0,sd=.01),times=N)
rSloPictGender = rep(rtruncnorm(nPictures,a=-1,b=1,mean=0,sd=.15),times=N)

fixedIntercept = 12
fixedSlopeAge = 0
fixedSlopeGender = .0
fixedSlopeCondition = -1.5
fixedSlopeConditionxAge = .03
fixedSlopeConditionxGender = .12

rating = fixedIntercept + 
  age*fixedSlopeAge + 
  gender*fixedSlopeGender +
  condition*fixedSlopeCondition + 
  (0.004*age^2)*age*condition*fixedSlopeConditionxAge +
  gender*condition*fixedSlopeConditionxGender +
  rIntSubj + 
  condition*rSloSubjCond +
  rIntPict +
  age*rSloPictAge +
  gender*rSloPictGender
error = rtruncnorm(length(rating),a=-2,b=2,mean=0,sd=0.5)
rating = rating + error
    
rating = round(rating*6.5)
hist(rating)
 rating[rating<0] = 0
 rating[rating>100] = 100

d = data.frame(idSubj,age,gender,condition,idPicture,rating)
d$gender = ifelse(d$gender==0,"F","M")
d$condition = ifelse(d$condition==0,"animal","landscape")
d$idSubj = paste0("subj",d$idSubj)
d$idPicture = paste0(substr(d$condition,1,4),d$idPicture)

fit = lmer(rating ~ age*condition+gender*condition+(condition|idSubj)+(gender|idPicture),data=d)
summary(fit)

plot(allEffects(fit))

write.table(d,"esercizio3.csv",sep=";",row.names=F)

#########################################

# ESERCIZIO 4: LOGISTIC GLMM ON ACCURACY OF RESPONSES IN CHILDREN

set.seed(1016)

nItemxTypexCond = 12
types = c(0,1)
conds = c(0,1)
minNSubjxClass = 14
maxNSubjxClass = 25
nClasses = 11

Subjects = c(); Classes = c()
for(i in 1:nClasses){
  nSubj = round(runif(1,minNSubjxClass,maxNSubjxClass))
  Subjects = c(Subjects,length(Subjects):(length(Subjects)+nSubj-1))
  Classes = c(Classes,rep(i,nSubj))
}
idSubj = c(); idClass = c(); idItem = c(); Type = c(); Condition = c()
for(i in 1:length(Subjects)){
  if(Subjects[i] %% 2){
    idItem = c(idItem,1:(length(types)*length(conds)*nItemxTypexCond))
    Type = c(Type,rep(types,times=nItemxTypexCond*length(conds)))
    
  }else{
    idItem = c(idItem,(length(types)*length(conds)*nItemxTypexCond):1)
    Type = c(Type,rep(rev(types),times=nItemxTypexCond*length(conds)))
  }
  Condition = c(Condition,rep(conds,each=nItemxTypexCond*length(types)))
  sigla = paste0(sample(letters,1),sample(letters,1),sample(letters,1))
  idSubj = c(idSubj,rep(paste0(sigla,Subjects[i]),length(types)*length(conds)*nItemxTypexCond))
  idClass = c(idClass,rep(paste0("cl",Classes[i]),length(types)*length(conds)*nItemxTypexCond))
}
idItem = paste0("item",idItem)
dx = data.frame(idClass,idSubj,idItem,Type,Condition,MathAnx=NA,Gender=NA,Age=NA,
                fixedIntercept = +0.60,
                fixedSlopeType = -0.25,
                fixedSlopeCondition = -0.40,
                fixedSlopeTypeCond = 0.00,
                fixedSlopeMathAnx = -0.12,
                fixedSlopeMathAnxCond = -0.08,
                fixedSlopeGender = 0.00,
                fixedSlopeMathAnxGender = +0.15,
                rIntClass=NA,rSloCondClass=NA,rSloTypeClass=NA,rSloGenderClass=NA,
                rIntSubj=NA,rSloCondSubj=NA,rSloTypeSubj=NA,rSloAnxSubj=NA,rSloCondAnxSubj=NA,
                rIntItem=NA,
                logit=NA,prob=NA,Correct=NA)
dx$idClass = as.factor(dx$idClass)
dx$idSubj = as.factor(dx$idSubj)
dx$idItem = as.factor(dx$idItem)

rIntClassSD = 0.40  
rSloCondClassSD = 0.10
rSloTypeClassSD = 0.06
rSloGenderClassSD = 0.02

rIntSubjSD = 0.50
rSloCondSubjSD = 0.15
rSloTypeSubjSD = 0.10
rSloAnxSubjSD = 0.05
rSloCondAnxSubjSD = 0.08

rIntItemSD = 0.60

errorSD = 1.00

for(l in levels(dx$idClass)){
  dx$rIntClass[dx$idClass==l] = rnorm(1,0,rIntClassSD)
  dx$rSloCondClass[dx$idClass==l] = rnorm(1,0,rSloCondClassSD)
  dx$rSloTypeClass[dx$idClass==l] = rnorm(1,0,rSloTypeClassSD)
  dx$rSloGenderClass[dx$idClass==l] = rnorm(1,0,rSloGenderClassSD)
}
for(l in levels(dx$idSubj)){
  dx$rIntSubj[dx$idSubj==l] = rnorm(1,0,rIntSubjSD)
  dx$rSloCondSubj[dx$idSubj==l] = rnorm(1,0,rSloCondSubjSD)
  dx$rSloTypeSubj[dx$idSubj==l] = rnorm(1,0,rSloTypeSubjSD)
  dx$rSloAnxSubj[dx$idSubj==l] = rnorm(1,0,rSloAnxSubjSD)
  dx$rSloCondAnxSubj[dx$idSubj==l] = rnorm(1,0,rSloCondAnxSubjSD)
  dx$MathAnx[dx$idSubj==l] = rnorm(1,0,1)
  dx$Gender[dx$idSubj==l] = rbinom(1,1,.5)
  dx$Age[dx$idSubj==l] = round(runif(1,9.5,10.5),1)
}
for(l in levels(dx$idItem)){
  dx$rIntItem[dx$idItem==l] = rnorm(1,0,rIntItemSD)
}
#dx$error = rnorm(nrow(dx),0,errorSD)
dx$MathAnx = dx$MathAnx - 0.25*dx$Gender + 0.125

dx$logit = apply(dx,1,FUN=function(x){
  as.numeric(x["fixedIntercept"])+
    as.numeric(x["fixedSlopeType"])*as.numeric(x["Type"])+
    as.numeric(x["fixedSlopeCondition"])*as.numeric(x["Condition"])+
    as.numeric(x["fixedSlopeTypeCond"])*as.numeric(x["Type"])*as.numeric(x["Condition"])+
    as.numeric(x["fixedSlopeMathAnx"])*as.numeric(x["MathAnx"])+
    as.numeric(x["fixedSlopeMathAnxCond"])*as.numeric(x["MathAnx"])*as.numeric(x["Condition"])+
    as.numeric(x["fixedSlopeGender"])*as.numeric(x["Gender"])+
    as.numeric(x["fixedSlopeMathAnxGender"])*as.numeric(x["Gender"])*as.numeric(x["MathAnx"])+
    as.numeric(x["fixedSlopeGender"])*as.numeric(x["Gender"])+
    as.numeric(x["rIntClass"])+
    as.numeric(x["rSloCondClass"])*as.numeric(x["Condition"])+
    as.numeric(x["rSloTypeClass"])*as.numeric(x["Type"])+
    as.numeric(x["rSloGenderClass"])*as.numeric(x["Gender"])+
    as.numeric(x["rIntSubj"])+
    as.numeric(x["rSloCondSubj"])*as.numeric(x["Condition"])+
    as.numeric(x["rSloTypeSubj"])*as.numeric(x["Type"])+
    as.numeric(x["rSloAnxSubj"])*as.numeric(x["MathAnx"])+
    as.numeric(x["rSloCondAnxSubj"])*as.numeric(x["Condition"])*as.numeric(x["MathAnx"])+
    as.numeric(x["rIntItem"])
  })
errorTerm = rnorm(nrow(dx),0,errorSD)
dx$prob = logit2prob(dx$logit+errorTerm)
dx$Correct = round(dx$prob)

####

d = dx[,c("idClass","idSubj","Gender","Age","MathAnx","Condition","idItem","Type","Correct")]
d$Gender = ifelse(d$Gender==0,"F","M")
d$Condition = ifelse(d$Condition==0,"Baseline","TimePressure")
d$Type = ifelse(d$Type==0,"Addition","Subtraction")
head(d)

mean(d$MathAnx[d$Gender=="M"])
mean(d$MathAnx[d$Gender=="F"])

write.table(d,"esercizio4.csv",sep=";",row.names=F)

####

fit = glmer(Correct ~ MathAnx*Condition + (1|idClass/idSubj)+(1|idItem), data=d, family="binomial")
summary(fit)

plot(allEffects(fit),multiline=T)

fit1 = glm(Correct ~ Type*MathAnx, data=d, family="binomial")
summary(fit1)
plot(allEffects(fit1),multiline=T)


fit2 = glmer(Correct ~ Condition + (Condition|idSubj)+(1|idItem), data=d, family="binomial")
summary(fit2)

#########################################


