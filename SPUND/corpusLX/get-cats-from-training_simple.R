#20231220(08.58)
#stefanowitsch.casestudy-2.badsmelling adjectives
#peterson-traba(2021).getcategories
###################################
###################################
# this script defines the categories of nouns according to below cat.array of (9) fixed noun categories.
# method:
### 1. get the categories which where user defined in a table
#lapsi
#d10.mod<-read.csv("~/boxHKW/21S/DH/local/SPUND/corpuslx/stefanowitsch/casestudy2.mod.csv")

#mini:
#d10.stef<-read.csv("/volumes/ext/boxHKW/21S/DH/local/SPUND/corpuslx/stefanowitsch/CaseStudy2_FullData.csv")
#lapsi
library(readr)
library(stringi)

box<-"https://userpage.fu-berlin.de/stschwarz/cqpdata/"
desk<-"~/boxHKW/21S/DH/local/SPUND/corpuslx/stefanowitsch/"
#d10.stef<-read.csv(paste0(box,"CaseStudy2_FullData.csv"))
#d10.gold<-read_csv(paste0(box,"casestudy2_full.gold.csv")) # manually defined gold 
d10.stef<-read.csv(paste0(desk,"CaseStudy2_FullData.csv"))
d10.gold<-read_csv(paste0(desk,"casestudy2_full.csv")) # manually defined gold 

#from saved df
#nouns.cats.known<-read.csv("fragrance_known-cats_coll.cpt.csv")
git<-"https://raw.githubusercontent.com/esteeschwarz/R-essais/main/SPUND/corpusLX/"
local<-"~/Documents/GitHub/R-essais/SPUND/corpusLX/"
nouns.cats.known<-read.csv(paste0(local,"fragrance_known-cats_coll.cpt.csv"))
nouns.cats.known.fix<-read.csv(local,"nouns.cats.known.csv") # modeled df of fixed cats, 8274 obs
nouns.cats.known.cpt<-read.csv(local,"nouns.cats.temp_918.csv")
### from git:
# nouns.cats.known<-read.csv(paste0(git,"fragrance_known-cats_coll.cpt.csv"))
# nouns.cats.known.fix<-read.csv(git,"nouns.cats.known.csv") # modeled df of fixed cats, 8274 obs
# nouns.cats.known.cpt<-read.csv(git,"nouns.cats.temp_918.csv")
lfix<-length(nouns.cats.known.fix$lfd)
nouns.cats.known.cpt$category[(lfix+1):length(nouns.cats.known.cpt$lfd)]<-NA

#########################################################
#########################################################

mplus<-d10.stef$Noun%in%nouns.cats.known$noun
m.not<-!mplus
sum(m.not)
### random noun from df which is not in the training set:
noun.q<-sample(d10.stef$Noun[m.not],1)
get.dist.df.g<-function(noun.q){
  
  noun<-noun.q
  print(noun)
  catfinal<-"n.a"
  ### > sketchengine request: fetch collocates to noun
  #d.c.u<-get.ske(noun,k)
  ###########################
  ### here test
  ### declare empty vars if 
  d.c.t.ass<-0
  cat.df<-data.frame(cat="n.a.",score=0,row.names = "#empty#")
  
  #####################
  ### start loop
  ### if response contains collocates:
  ### here new essai with collocates from DF, no new SkE request:
  # if (length(d.c.u)>4){
  #  d2u<-unlist(lapply(d.c.u$Gramrels$Words, get.words))
  ### > all collocations of q noun from df
  d2u<-nouns.cats.known.cpt$collocations[nouns.cats.known.cpt$noun==noun]
  d2u<-unique(d2u)
  #l<-length(d.c.u$Gramrels$Words)
  l<-length(d2u)
  ########
  word.no<-"#empty#"
  m<-grep(word.no,nouns.cats.known)
  sum(m)
  m<-grepl(word.no,nouns.cats.known.cpt$collocations)
  sum(m)
  nouns.cats.known.cpt<-nouns.cats.known.cpt[!m,]
  if(l>1){
    #  word.no<-d.c.u$Gramrels$Words[[l]]['word']
    #  word.no$word
    #  m.pos<-d2u%in%word.no$word
    m.pos<-d2u%in%word.no
    m.pos
    d2u<-d2u[!m.pos]
  } #discards postag cats from array
  d2u # collocations array of noun in question
  d.c.k.ar<-list(empty=NA)
  d.c.k.ar[[noun.q]]<-d2u
  ###### collocations list:
  #########################
  ### q direct match (appearance of noun.q in) noun in known.collocation
  m.both<-noun==nouns.cats.known$collocations
  nouns.cats.known$noun[m.both]
  sum(m.both)
  d.c.t.both<-0
  ### > matches of collocates (known cat) in collocates (cat unknown)
  m.coll<-nouns.cats.known$collocations%in%d2u
  #m.coll<-d2u%in%nouns.cats.known$collocations
  length(m.coll)
  sum(m.coll)
  m.coll
  #nouns.cats.known$collocations[m.both]
  table(factor(nouns.cats.known$collocations[m.both]))
  table(factor(nouns.cats.known$collocations[m.coll]))
  ### highes match in noun:
  which.max(table(factor(nouns.cats.known$noun[m.coll])))
  ### highest match in cat:
  which.max(table(factor(nouns.cats.known$category[m.coll])))
  m.coll.t<-table(factor(nouns.cats.known$noun[m.coll]))[order(table(factor(nouns.cats.known$noun[m.coll])))]
  m.coll.t
  ### discard:
  m.coll.mf<-sort(table(factor(nouns.cats.known.cpt$collocations)),decreasing = T)
  m.coll.mf<-sort(table(factor(nouns.cats.known.cpt$noun)))
  #  ?sort
  m.coll.mf
  coll.disc<-names(tail(m.coll.mf,2)) # most frequent collocates over all /thing/ + /place/
  coll.disc
  m.disc<-names(m.coll.t)%in%coll.disc
  m.disc
  sum(m.disc)
  length(m.coll.t)
  m.coll.t<-m.coll.t[!m.disc]
  m.coll.t # now (for /bone/ most frequent match is /bone/)
  #?order()
  #?sort()
  nouns.cats.known$collocations[m.coll]
  m.coll.max<-tail(m.coll.t,10)
  m.coll.max
  ### > back remove m.disc from m.coll
  # m.coll.disc.n<-nouns.cats.known$noun[m.coll]%in%coll.disc
  # m.coll.disc.c<-nouns.cats.known$collocations[m.coll]%in%coll.disc
  # m.coll.sub<-nouns.cats.known[m.coll,]%in%coll.disc
  # m.coll.disc.w<-which(nouns.cats.known$noun[m.coll]%in%coll.disc)
  # sum(m.coll.disc.n)
  # sum(m.coll.disc.c)
  # sum(m.coll.disc.n)
  # m.coll.disc.w
  cats.dist<-table(nouns.cats.known$category) # overall distribution of predefined cats
  cats.dist
  ### > make factor of that
  f<-1
  noun
  nouns.cats.known$factor<-NA
  for ( f in 1:length(cats.dist)){
    m<-nouns.cats.known$category==names(cats.dist[f])
    sum(m)
    nouns.cats.known$factor[m]<-1/cats.dist[f]
  }
  d<-1
  #  get.dist.df<-function(noun){
  cats.dist.df<-data.frame(cat=names(cats.dist),noun=noun,dist=NA,max=NA)
  for (d in 1:length(cats.dist.df$cat)){
    m.chk<-nouns.cats.known$category[m.coll]==cats.dist.df$cat[d]
    cats.dist.df$dist[d]<-sum(nouns.cats.known$factor[m.chk],na.rm = T)
  }
  dmax<-which.max(cats.dist.df$dist)
  cats.dist.df$max[dmax]<-TRUE
  cats.dist.df
  returnlist<-list(dist=cats.dist.df,nouns.df=nouns.cats.known)
  return(returnlist)
  return(cats.dist.df)
} #end get.dist.df.g() # obsolete function
#########################################################
### factor correction, cat SE
#set<-d10.gold
#coll.set<-nouns.cats.known.cpt
#ai.s<-F
#ai.f<-1:2
nouns.set<-nouns.cats.known
mod.factor<-function(ai.form){
  goldset<-d10.gold
  nouns.set<-eval(ai.form[1])
  coll.set<-eval(ai.form[2])
  ai.cat<-eval(ai.form[3])
  ai.s<-eval(ai.form[4])
  ai.f<-eval(ai.form[5])
  #ai.s<-T
  ifelse(ai.s==F,ai.mode<-F,ai.mode<-T)
  ai.nouns<-goldset$Noun[goldset$Category==ai.cat]
  length(ai.nouns)
  ai.nouns<-unique(ai.nouns)
  ai.m<-nouns.cats.known$noun%in%ai.nouns
  sum(ai.m) # not yet coded, klar
  ai.m<-coll.set$noun%in%ai.nouns
  sum(ai.m)
  if(ai.mode==T)
    {ai.ch.s<-sample(ai.nouns,ai.s)
  ai.ch.s
  }
  ai.coll.nouns<-coll.set$noun[ai.m]
  ai.coll.m<-coll.set$noun%in%ai.coll.nouns
  ai.coll.s.t<-sort(table(coll.set$noun[ai.coll.m]),decreasing = T)
  ai.coll.s.t
  ai.m.coll<-coll.set$noun%in%names(ai.coll.s.t)
  sum(ai.m.coll)
  length(ai.s)
  if(sum(ai.f>=1)>1)
    ai.ch.fix<-names(ai.coll.s.t)[ai.f]
  ###################################
  ### add collocates of chosen SE noun to known noun df
  ifelse(ai.mode==T,ai.use<-ai.ch.s,ai.use<-ai.ch.fix)
  ai.use
  coll.sub<-coll.set[coll.set$noun%in%ai.use,]
  coll.sub$category<-ai.cat
  coll.new<-rbind(nouns.set,coll.sub)
  m0<-coll.new$unique==0
  sum(m0)
  coll.new<-coll.new[!m0,]
  
  return(coll.new)


}  
#########################################################
### function from model:
#range.df<-1:10
#noun.q<-"lake"
#nouns.df.ai<-mod.factor(ai.form)
#sum(nouns.df.ai$noun=="lake")
### > feed in nounsdfai from modfactor()
get.cat.no.df<-function(noun.q,nouns.df.ai){
#  nouns.df.no<-data.frame(w.array)
  nouns.df.no<-d10.stef
  nouns.df.no$cat.ai<-NA
  nouns.cats.known<-nouns.df.ai
  
  #  k<-8
  nouns.cats.known$fac.p<-1/nouns.cats.known$unique
  noun<-noun.q
  noun
    ### > all collocations of q noun from df
    d2u<-nouns.cats.known.cpt$collocations[nouns.cats.known.cpt$noun==noun]
    d2u<-unique(d2u)
    #l<-length(d.c.u$Gramrels$Words)
    l<-length(d2u)
    ########
    word.no<-"#empty#"
    m<-grep(word.no,nouns.cats.known)
    sum(m)
    m<-grepl(word.no,nouns.cats.known.cpt$collocations)
    sum(m)
    nouns.cats.known.cpt<-nouns.cats.known.cpt[!m,]
    l<-length(d2u)
    m<-grepl(word.no,d2u)
    sum(m)
    d2u<-d2u[!m]
    if(l>1){
      m.pos<-d2u%in%word.no
      m.pos
      d2u<-d2u[!m.pos]
    } #discards postag cats from array
    d2u # collocations array of noun in question
    #word
    ############################################
    ### > matches of collocates (known cat) in collocates (cat unknown)
    getmatches<-function(coltrain,colq){
    m.coll<-nouns.cats.known$collocations%in%d2u
    m.coll<-coltrain%in%colq
    #m.coll<-d2u%in%nouns.cats.known$collocations
    length(m.coll)
    sum(m.coll)
    m.coll
    m<-m.coll
    }
    #############################################
    ### remove mfw
    #table(factor(nouns.cats.known$collocations[m.coll]))
    ### highes match in noun:
    #which.max(table(factor(nouns.cats.known$noun[m.coll])))
    ### highest match in cat:
    #which.max(table(factor(nouns.cats.known$category[m.coll])))
    #nouns.cats.known$cat[nouns.cats.known$noun=="body"]
    ### N: q/body/ == "AC", cat /body/ == "BO" !! max cat differs from /body/ concept
    ###################
    getmfw<-function(m.coll.x){
    m.coll.t<-table(factor(nouns.cats.known$noun[m.coll.x]))[order(table(factor(nouns.cats.known$noun[m.coll.x])))]
    m.coll.t
    m.coll.cat<-table(factor(nouns.cats.known$cat[m.coll.x]))[order(table(factor(nouns.cats.known$cat[m.coll.x])))]
    m.coll.cat
    ### discard:
    m.coll.mf<-sort(table(factor(nouns.cats.known.cpt$collocations)),decreasing = T)
    m.coll.mf<-sort(table(factor(nouns.cats.known.cpt$noun)))
    #  ?sort
    m.coll.mf
    coll.disc<-names(tail(m.coll.mf,2)) # most frequent collocates over all /thing/ + /place/
    coll.disc
    returnlist<-list(coll.t=m.coll.t,coll.cat=m.coll.cat,coll.disc=coll.disc)
    return(returnlist)
    }
    m.coll<-getmatches(nouns.cats.known$collocations,d2u)
    sum(m.coll)
    #########################
    coll.disc.g<-getmfw(m.coll)
    coll.disc<-coll.disc.g$coll.disc
    #########################
    coll.disc
    mdisc<-d2u%in%coll.disc
    sum(mdisc)
    length(d2u)
    d2u.disc<-d2u[!mdisc]
    length(d2u.disc)
    
    coll.a<-nouns.cats.known$collocations[m.coll]
    coll.a
    sum(m.coll)
    
    m.coll.b<-getmatches(nouns.cats.known$collocations,d2u.disc)
    sum(m.coll.b)
    ### highes match in noun:
    which.max(table(factor(nouns.cats.known$noun[m.coll.b])))
    ### highest match in cat:
    which.max(table(factor(nouns.cats.known$category[m.coll.b])))
    ### highes match in noun:
    sort(table(factor(nouns.cats.known$noun[m.coll.b])))
    ### highest match in cat:
    catfactor<-data.frame(cat=c(unique(nouns.cats.known$category),"n.a."),fac.p=NA)
    catfactor
    unique(nouns.cats.known$unique[nouns.cats.known$category=="B&UE"]) # infinite values in df
    m0<-nouns.cats.known$unique==0
    sum(m0)
    nouns.cats.known$unique[m0]<-1
    nouns.cats.known$fac.p<-1/nouns.cats.known$unique
    for(c in 1:length(catfactor$cat)){
      cat<-catfactor$cat[c]
    catfactor$fac.p[c]<-sum(nouns.cats.known$fac.p[nouns.cats.known$category==cat])
    }
    
    catfactor
    table(factor(nouns.cats.known$category[m.coll.b]))
    lt<-length(table(factor(nouns.cats.known$category[m.coll.b])))
    ifelse(lt>0,catfactor$fac.t<-as.double(table(factor(nouns.cats.known$category[m.coll.b])))/catfactor$fac.p,
           catfactor$fac.t<-NA)
    catfactor
    m.nodisc.g<-getmfw(m.coll.b)
    m.nodisc<-m.nodisc.g$coll.disc
    m.nodisc
    m.coll.t<-getmfw(m.coll.b)$coll.t
    #length(m.coll.t)
    #m.coll.t<-m.coll.t[!m.disc]
    #m.coll.t # now (for /bone/ most frequent match is /bone/)
    #?order()
    #?sort()
    
    m.coll.max<-tail(m.coll.t,10)
    m.coll.max
    ### > back remove m.disc from m.coll
    cats.dist<-table(nouns.cats.known$category) # overall distribution of predefined cats
    cats.dist
    ###########################################
    nouns.df<-nouns.cats.known
    m.coll<-nouns.df$coll
    m.coll
    ###########
    #m<-m.coll.b # new matches
    ###########
    m.coll.n<-nouns.df$cat[m.coll.b]
    m.coll.n
    m.coll.t<-table(factor(m.coll.n))
    m.coll.t
    max.coll.n<-which.max(table(factor(m.coll.n)))
    max.coll.n # this works!
    max.coll.ns<-names(max.coll.n)
    max.coll.ns
    mf<-nouns.df$cat==names(m.coll.t)
    nouns.df$cat[mf]
    noun
    names(table(nouns.df$cat))
    ############################################
    #m
    df.s<-data.frame(cat="n.a",match=NA,score=NA,row.names = "n.a.",max=T)
    max.cat<-"n.a"
    cats.dist.df<-df.s
    if(length(names(m.coll.t))>0){
      df.s<-data.frame(cat=names(m.coll.t),match=m.coll.t,score=NA,row.names = names(m.coll.t),max=F)
    c<-2
    m0<-nouns.df$unique==0
    sum(m0)
    # df.dcf$factor<-      #TODO
    for(c in 1:length(df.s$cat)){
  #    ck<-nouns.df$fac.p[nouns.df$cat==df.s$cat[c]]
      ck<-nouns.df$fac.p[nouns.df$cat==df.s$cat[c]]
      df.s$factor[c]<-sum(ck)
      m<-is.infinite(df.s$factor)
      sum(m)
      df.s$factor[m]<-NA
      df.s$score<-df.s$match.Freq/df.s$factor
      ### this should be the place to modifying after training and insert feedback of the errorrate, maybe the proportion
      ### of correct matched cats
    
    }
    maxcore<-which.max(df.s$score)
    maxcore
    df.s$max[maxcore]<-T
    cats.dist.df<-df.s
    df.s
    ##############################################
    catfinal.coll<-df.s$cat[which.max(df.s$score)]
    max.cat<-catfinal.coll
    #max.cat<-names(max.coll.cat)
    max.cat
    }
    
    ############################################
    #k
    #nouns.df$noun[k]
    length(max.cat)
    mk<-nouns.df.no$Noun==noun
    sum(mk)
    which(mk)
    ifelse(length(max.cat)>0,nouns.df.no$cat.ai[mk]<-max.cat,nouns.df.no$cat.ai[mk]<-NA)
  #}
  returnlist<-list(dist=cats.dist.df,nouns.df=nouns.df.no)
  return(returnlist)
  return(nouns.df.no)
} #end get.cat.no.df()

#########################################################
#dmax1<-get.cat.no.df("lake")
#dmax1<-get.dist.df.g("rose")
#dmax1$nouns.df$collocations[dmax1$nouns.df$Noun=="lake"]
#dmax1$nouns.df$
#returnlist$nouns.df$cat.ai[returnlist$nouns.df$Noun=="lake"]
#returnlist$nouns.df$cat.ai[mk]
#sum(returnlist$nouns.df$Noun=="lake")
#print(dmax1$dist)
#k<-1k<-1cat.ai
dist.list<-list()
range.df<-103:104
range.df
k<-103
############################
catcall<-function(range.df,ai.form){
d10.stef$cat.ai<-NA
#range.df<-1:length(d10.stef$Noun
for(k in range.df){
  q<-d10.stef$Noun[k]
  q
  #df<-get.dist.df.g(q)
  #################### model
  ### > here feed in factor modfication:
  # formula:
  df<-get.cat.no.df(q,mod.factor(ai.form))
  ####################
  df$dist
  maxcat<-df$dist$cat[which(df$dist$max==T)]
  maxcat
  mk<-d10.stef$Noun==q
  which(mk)
  d10.stef$cat.ai[mk]<-maxcat
  cat("run",k,q,maxcat,"\n")
  dist.list[['dist.df']][[q]]<-df$dist
  #  df$nouns.df$cat.ai[103:105]
}
dist.list[['nouns.df']]<-d10.stef
#cat.df<-data.frame(dist.list$dist.df)
returnlist<-list(nouns.df=d10.stef,dist.df=dist.list$dist.df,trainset=nouns.cats.known)
return(returnlist)
}
sampledist<-sample(1:length(d10.stef$Corpus),100)
sampledist
##############################
ai.form<-expression(nouns.cats.known,nouns.cats.known.cpt,'SE',F,c(1:2))
eval(ai.form[4])

distessai<-catcall(sampledist,ai.form)
distessai$nouns.df$cat.ai[distessai$nouns.df$Noun=="lake"]
############################################
### evaluate definition:
#d10.gold<-read_csv("fragrance2_ai-cats.gold.csv") # manually defined gold standard of cats
#d10.gold<-read_csv("/Volumes/EXT/boxHKW/21S/DH/local/SPUND/corpuslx/stefanowitsch/casestudy2_full.csv") # manually defined gold 
#lapsi
#d10.ai<-d10.stef$cat.ai
########################
evalcat<-function(goldset,testset,sampledist){
  df<-goldset
  d10.gs<-df[with(df,order(df[,"Token_ID"])), ]
  d10.gs<-d10.gs[sampledist,]
  df<-testset
  d10.ai.s<-df[with(df,order(df[,"Token_ID"])), ]
  d10.ai.s$cat.ai<-gsub("B&UE","B&AE",d10.ai.s$cat.ai)
  d10.ai.s<-d10.ai.s[sampledist,]
  chks<-d10.ai.s$Token_ID==d10.gs$Token_ID
  print(chks)
  print(sampledist)
  print(sum(chks,na.rm = T))
  #d10.gs$cat.ai<-cat
  p1<-d10.gs$Category==d10.ai.s$cat.ai
  p2<-p1
  p2[is.na(p1)]<-F
  print(d10.gs$Noun[p2])
  print(d10.gs$Category[p2])
  print(sum(p1,na.rm = T))
  print(sum(p1,na.rm = T)/length(p1))
  dfreturn<-data.frame(cat=d10.gs$Category,ai=d10.ai.s$cat.ai)
  d10.ai.s$Category<-d10.gs$Category
  ai.t<-table(d10.gs$Category[p2])
  ai.t
  listreturn<-list(freq=ai.t,set=d10.ai.s)
  return(listreturn)
  return(d10.ai.s)
  ############################
  ##################################
}
### > change algorithm of defining cats from df
df.known<-distessai$nouns
############################################
### > evaluate:
#distessai$df
distessai$df[!is.na(distessai$df$cat.ai),c('Noun','cat.ai')]
testset<-distessai$nouns.df
#temp
m<-is.na(testset$cat.ai)
sum(m)
mdw<-which(duplicated(testset$Noun))
mdw
md<-duplicated(testset$Noun)
#sum(md)
testset$Noun[md]
md
###
goldset<-d10.gold
#print(sum(testset$Token_ID==goldset$Token_ID))
evaldf<-evalcat(goldset,testset,sampledist)
###########


### wks.
### N: only AC and B&UE are recognized, they are predefined the most.
catsknown.t<-table(nouns.cats.known$category)
catsknown.t
############################################

### N: theres too many AC coded, this is the only cat recognized correctly
### < factorized cats
### : 8% correct recognition
### : feedback p
### : tendency: assign SE, which is the cat with highest factor because theres only 1 in training set.
### > code more SE nouns manually resp fetch them from goldset: nouns.cats.known<-mod.factor()
################
### 10%, try factor modification: cats with high frequency nouns and cats with few definitions
### should be reduced approaching an optimum


### assemble few results and relate this to overall cat distribution
evaldf$freq

evalfrequencies<-function(){
  ### process:
  sampledist<-sample(1:length(d10.stef$Corpus),100)
  distessai<-catcall(sampledist,ai.form)
  testset<-distessai$nouns.df
  goldset<-d10.gold
  evaldf<-evalcat(goldset,testset,sampledist)
  distessai$dist.df
  ###
  df<-data.frame(evaldf$freq)
  library(collostructions)
  #  df$set<-"eval"
  df
  #m1<-nouns.cats.known$category%in%df$Var1
  nouns.cats.known$category<-gsub("B&UE","B&AE",nouns.cats.known$category)
  f.2<-table(nouns.cats.known$category)
  
  f.2
  m1<-names(f.2)%in%names(evaldf$freq)
  m1
  df$train<-NA
  k<-1
  for(k in 1: length(df$Var1)){
    c<-df$Var1[k]
    c
    m<-names(f.2)[m1]==c
    df$train[k]<-f.2[m]
    
  }
  coll<-collex(df)
  returnlist<-list(freq=coll,score=distessai$dist.df)
 
  return(returnlist)
  return(coll)
}

# freq.list<-list()
# for (k in 1:10){
# freq.list[[k]]<-evalfrequencies()  
# }
# 
# freq.list[[1]][['freq']]
# 
# getwd()
# save(freq.list,file = "freqlist.RData")
# 
############################
### TODO: a routine which after fetching the eval result modifies the factor that scores the cat definition and feeds that
### into the next run. small steps of modifying will change the cat definition, if that results in more correct definitions
### the direction of modifying is considered good. the algorithm of change can be random with documenting the effect. that
### effect of change will be measured and evaluated via lmer, so that it becomes clear which changes have the greatest effect, 
### like (lmer df model)

effectmodel<-data.frame(errorrate=11:17,cat=c("A",LETTERS[11:16]),mod.cat=c("A",LETTERS[11:16]),mod.fac=c(0,sample(-3:3,6)),effect.cat=NA,effect=NA,run=NA)
#effectmodel$effect<-NA
###
k<-1
for(k in 1:10){
effects.1<-data.frame(errorrate=sample(11:17),cat=c("A",LETTERS[11:16]),mod.cat=c("A",LETTERS[11:16]),mod.fac=c(0,sample(-2:3,6)),effect.cat=NA,effect=NA,run=NA)
effectmodel<-rbind(effectmodel,effects.1)
effectmodel$run=k
lm1<-lmer(errorrate ~ cat + (1|mod.cat),effectmodel)
lm.1<-summary(lm1)
lm.1
mx<-which.max(abs(lm.1$coefficients[2:7,3]))
mf<-max(abs(lm.1$coefficients[2:7,3]))
catx<-stri_split_regex(names(mx),"cat",simplify = T)[,2]
m<-grep(catx,effectmodel$mod.cat)
r<-k==effectmodel$run
effectmodel$effect[r]<-mf
effectmodel$effect.cat<-catx
}
effectmodel

### the mod.cat... content (which is the steps of modifying) will be randomised over the definition runs
library(lme4)
effectmodel
lm1<-lmer(errorrate ~ cat + (1|mod.cat),effectmodel)
lm.1<-summary(lm1)
lm.1


###########################
distessai$cat$eval$sandwich
m<-evaldf$match<-evaldf$Category==evaldf$cat.ai
sum(m,na.rm = T)
unique(evaldf$cat.ai)
evaldf$cat.ai[evaldf$cat.ai=="n.a"]<-NA
m<-evaldf$match<-evaldf$Category==evaldf$cat.ai
sum(m,na.rm = T)
evaldist<-data.frame(cat=unique(evaldf$cat.ai),sum=NA)
#x<-"AC"
evalsum<-function(x)sum(evaldf$cat.ai==x,na.rm = T)
evaldist$sum<-lapply(evaldist$cat,evalsum)
evaldist
distessai$cat$eval$sandwich
evaldf$Category[evaldf$Noun=="sandwich"]
length(distessai$cat$eval)
nouns.cats.known.cpt$category[nouns.cats.known.cpt$noun=="lake"]
