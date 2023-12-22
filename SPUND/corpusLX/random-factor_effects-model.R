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

build.f.model<-function(f.mod,c.mod){
  effectmodel<-data.frame(errorrate=11:17,cat=c("A",LETTERS[11:16]),
                          mod.cat=c("A",LETTERS[11:16]),mod.fac=0,effect.cat="A",effect=0,run=0)
###
#k<-1
#f.mod<-0.7
#c.mod<-"M"
for(k in 1:6){
  effects.1<-data.frame(errorrate=sample(11:17),cat=c("A",LETTERS[11:16]),mod.cat=c("A",LETTERS[11:16]),mod.fac=c(0,sample(-2:3,6)),effect.cat=NA,effect=NA,run=NA)
  ### feed in factor
  mod.cat<-c(A="A",K="K",L="L",M="M",N="N",O="O",P="P")
  mod.pos<-mod.cat%in%c.mod
  mod.pos<-k+1
    #ifelse(length(f.mod)>0,)
    mod.fac<-c(0,0,0,0,0,0,0)
    mod.fac[mod.pos]<-f.mod
    mod.fac
  effects.1<-data.frame(errorrate=sample(11:17),cat=c("A",LETTERS[11:16]),
                        mod.cat=c("A",LETTERS[11:16]),
                        mod.fac=mod.fac,effect.cat=NA,effect=NA,run=NA)
  
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
return(effectmodel)
}
build.rnd.model<-function(f.mod){
  effectmodel<-data.frame(errorrate=11:17,cat=c("A",LETTERS[11:16]),
                          mod.cat=c("A",LETTERS[11:16]),mod.fac=0,effect.cat="A",effect=0,run=0)
  for(k in 1:20){
    effects.1<-data.frame(errorrate=sample(11:17),cat=c("A",LETTERS[11:16]),
                          mod.cat=c("A",LETTERS[11:16]),
                          mod.fac=eval(f.mod),effect.cat=NA,effect=NA,run=NA)
    effects.1$run=k
 #   effects.1$mod.cat[1]<-"A"
  #  effects.1$mod.fac[1]<-0
   # effects.1$effect.cat[1]<-"A"
  #  effects.1$effect[1]<-0
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
  return(effectmodel)
}
 # effectmodel
#testeffect<-build.model(1,"M")
testeval<-function(){
#  f.mod<-expression(c(0,0,0,0,0,0,0))
  f.mod<-expression(c(0,sample(-2:3,6)))
  eval(f.mod)
  #c.mod<-expression("A",)
  testeffect<-build.rnd.model(f.mod)
  effectmodel<-testeffect
  lm1<-lmer(effect ~ effect.cat + (1|mod.fac),effectmodel)
  lm.1<-summary(lm1)
  lm.1
  mx<-which.max(abs(lm.1$coefficients[2:length(lm.1$coefficients[,1]),1]))
  mx
  mx<-mx+1
  mx
  mf<-max(abs(lm.1$coefficients[2:length(lm.1$coefficients[,1]),1]))
  mf
  #  mf<-abs(lm.1$coefficients[1,1])
  cat.int<-stri_split_regex(rownames(lm.1$coefficients)[mx],"cat",simplify = T)[,2]
  cat.x<-cat.int
  returnlist<-c(mf,cat.x)
  f.mod<-mf
  c.mod<-cat.x
  #eval(f.mod)
  #c.mod<-expression("A",)
  testeffect<-build.f.model(f.mod,c.mod)
  effectmodel<-testeffect
  lm1<-lmer(effect ~ effect.cat + (1|mod.fac),effectmodel)
  lm.1<-summary(lm1)
  lm.1
  
    }


# ?rownames
# ### the mod.cat... content (which is the steps of modifying) will be randomised over the definition runs
# library(lme4)
# effectmodel
# 
# lm1<-lmer(errorrate ~ cat + (1|mod.cat),effectmodel)
# lm.1<-summary(lm1)
# lm.1
# ####
# #### wks., now evaluate random model df factor and re-run
# 
# lm2<-lmer(effect ~ effect.cat + (1|mod.fac),effectmodel)
# lm.2<-summary(lm2)
# lm.2
# ###########################
