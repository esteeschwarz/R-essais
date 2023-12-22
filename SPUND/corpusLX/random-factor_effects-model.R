#20231222(08.23)
#14517.stefanowitsch.get-categories.random-effects-model
########################################################
### TODO: a routine which after fetching the eval result modifies the factor that scores the cat definition and feeds that
### into the next run. small steps of modifying will change the cat definition, if that results in more correct definitions
### the direction of modifying is considered good. the algorithm of change can be random with documenting the effect. that
### effect of change will be measured and evaluated via lmer, so that it becomes clear which changes have the greatest effect, 
### like (lmer df model)
########################
########################

effectmodel<-data.frame(errorrate=11:17,cat=c("A",LETTERS[11:16]),mod.cat=c("A",LETTERS[11:16]),mod.fac=c(0,sample(-3:3,6)),effect.cat=NA,effect=NA,run=NA)
#effectmodel$effect<-NA
###
k<-1
for(k in 1:10){
  effects.1<-data.frame(errorrate=sample(11:17),cat=c("A",LETTERS[11:16]),mod.cat=c("A",LETTERS[11:16]),mod.fac=c(0,sample(-2:3,6)),effect.cat=NA,effect=NA,run=NA)
  effects.1$run=k
  effectmodel<-rbind(effectmodel,effects.1)
  lm1<-lmer(errorrate ~ cat + (1|mod.cat),effectmodel)
  lm.1<-summary(lm1)
  lm.1
  mx<-which.max(abs(lm.1$coefficients[2:7,3]))
  mf<-max(abs(lm.1$coefficients[2:7,3]))
  catx<-stri_split_regex(names(mx),"cat",simplify = T)[,2]
  m<-grep(catx,effectmodel$mod.cat)
  r<-k==effectmodel$run
  effectmodel$effect[r]<-mf
  effectmodel$effect.cat[r]<-catx
}
effectmodel

### the mod.cat... content (which is the steps of modifying) will be randomised over the definition runs
library(lme4)
effectmodel
lm1<-lmer(errorrate ~ cat + (1|mod.cat),effectmodel)
lm.1<-summary(lm1)
lm.1


###########################
