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
library(RecordLinkage)
library(clipr)

box<-"https://userpage.fu-berlin.de/stschwarz/cqpdata/"
desk<-"~/boxHKW/21S/DH/local/SPUND/corpuslx/stefanowitsch/"
#d10.stef<-read.csv(paste0(box,"CaseStudy2_FullData.csv"))
#d10.gold<-read_csv(paste0(box,"casestudy2_full.gold.csv")) # manually defined gold 
d10.stef<-read.csv(paste0(desk,"CaseStudy2_FullData.csv"))
d10.gold<-read_csv(paste0(desk,"casestudy2_full_m.csv")) # manually defined gold

#from saved df
#nouns.cats.known<-read.csv("fragrance_known-cats_coll.cpt.csv")
git<-"https://raw.githubusercontent.com/esteeschwarz/R-essais/main/SPUND/corpusLX/"
local<-"~/Documents/GitHub/R-essais/SPUND/corpusLX/"
nouns.cats.known<-read.csv(paste0(local,"fragrance_known-cats_coll.cpt.csv"))
nouns.cats.known.fix<-read.csv(paste0(local,"nouns.cats.known.csv")) # modeled df of fixed cats, 8274 obs
nouns.cats.known.cpt<-read.csv(paste0(local,"nouns.cats.temp_918_m.csv"))
#load(paste0(local,"result_DF(sample1-918)_singled-list.RData"))

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
### clean DB:
dbx<-nouns.cats.known.cpt[!duplicated(nouns.cats.known.cpt),]
nouns.cats.known.cpt<-dbx
#dbx<-nouns.cats.known[!duplicated(nouns.cats.known),]
get.dist.df.g.obs<-function(noun.q){
  
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
#nouns.set<-nouns.cats.known
mod.factor.obs<-function(ai.form){
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
### > matches of collocates (known cat) in collocates (cat unknown)
getmatches.first<-function(coltrain,colq){
 # m.coll<-nouns.cats.known$collocations%in%d2u
  m.coll<-coltrain%in%colq
  #m.coll<-d2u%in%nouns.cats.known$collocations
  length(m.coll)
  sum(m.coll)
  m.coll
  coll.temp<-table(nouns.cats.known$collocations[m.coll])
  coll.temp
  m<-m.coll
  
}
getmfw.first<-function(m.coll.x){
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

#noun<-"pomatum"
get.coll.array<-function(testset,noun){
  nouns.cats.known.cpt<-testset  
  d2u<-nouns.cats.known.cpt$collocations[nouns.cats.known.cpt$noun==noun]
  d2u<-unique(d2u)
  
  #l<-length(d.c.u$Gramrels$Words)
  l<-length(d2u)
  ########
  word.no<-"#empty#"
 # m<-grep(word.no,nouns.cats.known)
  #sum(m)
  #m<-grepl(word.no,nouns.cats.known.cpt$collocations)
  #sum(m)
  #nouns.cats.known.cpt<-nouns.cats.known.cpt[!m,]
  l<-length(d2u)
  m<-grepl(word.no,d2u)
  sum(m)
  #d2u<-d2u[!m]
  if(l>1){
    m.pos<-d2u%in%word.no
    m.pos
    d2u<-d2u[!m.pos]
  } #discards postag cats from array
  d2u # collocations array of noun in question
return(d2u)
    d2u=data.frame(df="test",coll=d2u)
  return(d2u)
  
}
#k<-6
#tnoun<-d10.stef$Noun[k]
#tnoun
#d1u<-get.train.array(nouns.cats.known,"lake")
#d2u<-get.coll.array(nouns.cats.known.cpt,tnoun)
#length(d2u)
#a.noun
#unoun
##anoun<-"river"
#traindf<-trainset
get.train.array<-function(traindf,anoun){
 # d1u<-nouns.cats.known$collocations[nouns.cats.known$noun==anoun]
  d1u<-traindf$collocations[traindf$noun==anoun]
  a.cat<-traindf$category[traindf$noun==anoun]
  d1u.df<-data.frame(df="train",coll=d1u,cat=a.cat)
}
#trainset<-nouns.cats.known
#testset<-nouns.cats.known.cpt
#k<-10

#qnoun<-d10.stef$Noun[k]
#unoun<-"tradition"
#qnoun<-"tradition"
#########
get.compare.df<-function(trainset,testset,unoun,qnoun){
  d1u<-get.train.array(trainset,unoun)
  d2u<-get.coll.array(testset,qnoun)
  sum(d1u$coll==d2u)
  
  ### discard mfw:
  m.coll<-getmatches.first(d1u$coll,d2u)
  length(d1u$coll)
  sum(m.coll)
  m.disc<-getmfw.first(m.coll)
  ### this returns the mf 2 matches, can be adapted to return more
  #table(nouns.cats.known$collocations[m.coll])[order(table(factor(nouns.cats.known$collocations[m.coll])))]
  m.disc$coll.cat
  m.disc$coll.t
  m.disc$coll.disc
  md1<-d1u%in%m.disc$coll.disc
  sum(md1)
  ### try without >
  # d1u<-d1u[!md1]
  # md2<-d2u%in%m.disc$coll.disc
  # sum(md2)
  # d2u<-d2u[!md2]
  # ################
  d1u.df<-data.frame(df="traincoll",coll=d1u$coll,cat=d1u$cat)
  d2u.df<-data.frame(df="testcoll",coll="#empty#",cat="n.a.")
  if(length(d2u)>0)
    d2u.df<-data.frame(df="testcoll",coll=d2u,cat=0)
  dfcompare<-rbind(d1u.df,d2u.df)
returnlist<-list(train=d1u.df,test=d2u.df)
}
u<-1
#testarray<-get.coll.array(nouns.cats.known.cpt,"odor")
#sum(is.na(nouns.cats.known$category))
#trainset<-train.clean
#testset<-nouns.cats.known.cpt
u<-4
#qnoun
###################################################
get.max.compare.cats<-function(trainset,testset,qnoun){
  nouns.cats.known<-trainset
  u1<-data.frame(noun=unique(nouns.cats.known$noun[!is.na(nouns.cats.known$category)]),freq=NA,category=NA)
  u<-1
  for (u in 1:length(u1$noun)){
    unoun<-u1$noun[u]
    unoun
    cat(u,"getmatches for:",unoun)
    compareset<-get.compare.df(trainset,testset,unoun,qnoun)
    #compareset$
    l1<-length(compareset$train$coll)
    l2<-length(compareset$test$coll)
    if(l2>0)
####>      c1<-get.max.compare.cats(compareset$train$coll,compareset$test$coll)
####>      ###########################################################################
    #  c1<-compare.linkage(matrix(compareset$train$coll),matrix(compareset$test$coll))
    #  f1<-c1$frequencies # THIS 1st method, 22%
    ##################################################################################
      #c1$frequencies
          c1<-compareset$train$coll%in%compareset$test$coll
    f1<-sum(c1)/(l1+l2)
    
  #    d1u<-get.train.array(u1$noun[u])
  #d2u<-get.coll.array(nouns.cats.known.cpt,q)
  #c1<-compare.linkage(matrix(d1u),matrix(testarray))
  cat("> ",f1,"\n")
#  u1$[u]
  u1$freq[u]<-f1
  u1$q<-qnoun
  #nouns.cats.known$category[nouns.cats.known$noun==unoun]
  length(u1$category[u])
  u1$category[u]<-unique(nouns.cats.known$category[nouns.cats.known$noun==unoun])
  

  }
  u1$category[which.min(u1$freq)]
  u1$category[which.max(u1$freq)]
  
  return(u1)
}
#qnoun<-"tradition"
#test /tradition/
#c1<-get.max.compare.cats(nouns.cats.known,nouns.cats.known.cpt,qnoun)
### > no. 
#comparedf<-get.compare.df(trainset,testset,"tradition",qnoun)
#sum(grepl("place",comparedf$test))
# d2u<-get.coll.array(nouns.cats.known.cpt,"water")
# u1<-get.max.compare.cats(nouns.cats.known,d2u)
getlinks<-function(compareset){
  c1<-compare.linkage(compareset)
  
}

#################################
### test with linkage library

# get.link.freq<-function(d2u){
# #d2u<-get.coll.array(nouns.cats.known.cpt,"lake")
# #d1u<-get.train.df()
# compareset<-get.compare.df(d2u)
# c1<-getPairs(compareset)
# c1<-compare.linkage(matrix(compareset$train$coll),matrix(compareset$test$coll))
# c1$frequencies
# 
# }

get.cat.no.df.obs<-function(noun.q,nouns.df.ai){
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
    #}
 #   d2u<-get.coll.array(nouns.cats.known.cpt,"lake") #ref: /lake/=141 tokens
    d2u<-get.coll.array(nouns.cats.known.cpt,noun) #ref: /lake/=141 tokens
    
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
#dist.list<-list()
#range.df<-103:104
#range.df
#k<-103

############################
#trainset<-get.train.array(nouns.cats.known,"lake")
#testset<-get.coll.array(nouns.cats.known.cpt,"lake")
#q<-10
#qnoun<-d10.stef$Noun[q]
#qnoun
get.link.freq.obs<-function(trainset,testset,q){
  max.df<-data.frame(noun=q,freq=NA,category=NA)
 # trainset<-get.train.array(nouns.cats.known,q)
#  testset<-get.coll.array(nouns.cats.known.cpt,q)
  
#  d2u<-get.coll.array(testset,q)
  #d1u<-get.train.df()
  # compareset<-get.compare.df(trainset,testset)
  # length(compareset$train$coll)
  # if(length(compareset$test$coll)>0)
  #   c1<-get.max.compare.cats(trainset,testset,qnoun)
  #   c1<-compare.linkage(matrix(compareset$train$coll),matrix(compareset$test$coll))
  # m<-c1$pairs$V1>0
  # sum(m)
#  c1$data1$V1[c1$pairs[m,1]]
  # if(length(d2u)>0)
    c1<-get.max.compare.cats(nouns.cats.known,nouns.cats.known.cpt,qnoun)
  #   c1<-compare.linkage(matrix(compareset$train$coll),matrix(compareset$test$coll))
  max.df<-c1
  
}
#c1<-get.link.freq(trainset,testset,qnoun)
#ml<-c1$data1=="thing"
#sum(ml)
#range.df<-1:30
####################################

catcall<-function(range.df,ai.form){
  dist.df<-list()
d10.stef$cat.ai<-NA
#range.df<-1:length(d10.stef$Noun
k<-11

for(k in range.df){
  q<-d10.stef$Noun[k]
  qnoun<-q
  cat("run",k,"qnoun to match:",q,"-------------\n")
  
  #df<-get.dist.df.g(q)
  #################### model
  ### > here feed in factor modfication:
  # formula:
  ########################################
  ### new with recordlinks
  ### test with linkage library
  #c1<-get.link.freq(nouns.cats.known,nouns.cats.known.cpt,q)
  #c1$frequencies
 # max.cat<-c1$category[which.max(c1$freq)]
#  dist.df[[q]]<-c1[!is.na(c1$freq),]
  #dist.df[[q]]<-c1$frequencies
  c1<-get.max.compare.cats(nouns.cats.known,nouns.cats.known.cpt,qnoun)
  ### test:
  dist.df[[q]]<-c1
  ########################################
#   tempout.df<-function(){
#   out:  df<-get.cat.no.df(q,mod.factor(ai.form))
#   df$dist
#   maxcat<-df$dist$cat[which(df$dist$max==T)]
#   maxcat
#   mk<-d10.stef$Noun==q
#   which(mk)
#   d10.stef$cat.ai[mk]<-maxcat
#   cat("run",k,q,maxcat,"\n")
#   dist.list[['dist.df']][[q]]<-df$dist
#   #  df$nouns.df$cat.ai[103:105]
# }
# d10.stef$cat.ai[k]<-max.cat
  cat(" --- done\n")
  
  }
#dist.list[['nouns.cats.ai']]<-d10.stef
#cat.df<-data.frame(dist.list$dist.df)
#returnlist<-list(nouns.cats.ai=d10.stef,dist.list=dist.df,trainset=nouns.cats.known)
returnlist<-list(dist.list=dist.df)

#returnlist<-list(nouns.df=d10.stef,trainset=nouns.cats.known)#dist.df=dist.list$dist.df,trainset=nouns.cats.known)
return(returnlist)
} #end catcall
goldset<-d10.gold
clean.db<-function(trainset,goldset){
  trainset$category<-gsub("B&UE","B&AE",trainset$category)
  a.noun.u<-unique(trainset$noun)
    for(k in 1:length(a.noun.u)){
    anoun<-a.noun.u[k]
    gn.array<-goldset$Noun%in%anoun
    q.cat<-table(goldset$Category[gn.array])
    cat.max<-names(q.cat)[which.max(q.cat)]
    kn.array<-trainset$noun%in%anoun
    trainset$category[kn.array]<-cat.max
  }
#  m<-trainset$noun%in%goldset$Noun
 return(trainset) 

}
train.clean<-clean.db(nouns.cats.known,goldset)
##############################
nouns.cats.known<-train.clean
##############################
#sampledist<-sample(1:length(d10.stef$Corpus),100)
### > for complete corpus:
sampledist<-1:length(d10.stef$Corpus)
# make sample of unique qnouns
q.noun.d<-duplicates(d10.stef$Noun)
#sampledist<-1:10
##############################
ai.form<-expression(nouns.cats.known,nouns.cats.known.cpt,'SE',F,c(1:2))
eval(ai.form[4])
#eval(get.cat.no.df)
###################################### THIS >
distessai<-catcall(sampledist,ai.form)
#############################################
getwd()
save(distessai,file = paste0(local,"/freqlist_linkrecord(sample1-918.4).RData"))
### > get factor into df
distlist<-distessai$dist.list
c.df.from.dist<-function(distlist){
  t.df<-data.frame(distlist)
  delist<-function(x)data.frame(noun=unlist(x$noun),freq=unlist(x$freq),category=unlist(x$category))
  cdf1<-lapply(distessai$dist.list, delist)
  mna<-is.na(cdf1)
  mna
  sum(mna)
  ldf<-length(t.df)
  lc<-length(distessai$dist.list[[1]])
  cf<-ldf/lc
  
  c.sel.s<-seq(1,ldf,lc)
  c.sel.e<-c.sel.s-1
  c.sel.e<-c(c.sel.e[2:length(c.sel.e)],ldf)
  c.sel.s
  c.sel.e # col selection of df, 3 for each test noun (noun,freq,cat)
  #######
  k<-1
  s.df<-matrix("",ncol = 4)
  colnames(s.df)<-c("noun","freq","cat","q")
  
  for (k in 1:length(c.sel.s)){
    cs<-c.sel.s[k]
    ce<-c.sel.e[k]
    s.df.temp<-t.df[,cs:ce]
    colnames(s.df.temp)<-c("noun","freq","cat","q")
    s.df.temp$q<-names(distessai$dist.list)[k]
    #mna<-is.na()
        
    s.df<-rbind(s.df,s.df.temp)
    
  }
  m<-is.na(s.df$noun)
  sum(m)
  s.df<-s.df[!m,]
  m<-s.df$noun==""
  s.df<-s.df[!m,]
}
disttable<-c.df.from.dist(distessai$dist.list)

apply.factor<-function(disttable){
  a.noun.u<-unique(disttable$noun)
  q.noun.u<-unique(disttable$q)
  ccat.u<-unique(disttable$cat)
  #fac.t<-table(nouns.cats.known$category)
  fac.u<-table(disttable$cat)
  fac.u
  qnoun<-"rose"
  qnoun.array<-disttable$q==qnoun
  sum(qnoun.array) # 55 assigned cat frequencies
  q.cat.t<-table(disttable$cat[qnoun.array])
  #q.cat.t<-fac.u
  q.cat.t # this factor applies to all q.nouns
  q.cat.exp<-q.cat.t/sum(q.cat.t)*q.cat.t
  q.cat.exp
  q.cat.dif<-q.cat.exp-q.cat.t
  q.cat.dif
  q.cat.p<-q.cat.dif*q.cat.dif/q.cat.exp
  q.cat.p
  
  #write_clip(fac.u)
  #write_clip(names(fac.u))
  # for(q in 1:length(q.noun.u)){
  #   qnoun<-q.noun.u[q]
  for(k in 1:length(fac.u)){
    disttable$fac.u[disttable$cat==names(fac.u)[k]]<-fac.u[k] 
    disttable$fac.q.p[disttable$cat==names(q.cat.p)[k]]<-q.cat.p[k] 
    
      #   # qnoun.array<-disttable$q==qnoun
  #   # fac.u.q<-table(disttable$cat[qnoun.array])
  #   # disttable$fac.u.q[disttable$cat==names(fac.u)[k]<-fac.u[k]
  # }
  }
  
  fac.s<-sum(fac.u)
  fac.s #100%
  fac.u #p
  sum(q.cat.p)
  ### get p for category:
  q<-7
  fac.u.s<-fac.u/fac.s
  fac.u.s
  fac.u.ch<-fac.s
  #write_clip(fac.u)
  fac.s
  disttable$max.obs<-F
  disttable$max.qp<-F
  mode(disttable$freq)<-"double"
  disttable$max.p<-F
  k<-1
  anoun<-unique(disttable$noun)
  qnoun<-unique(disttable$q)
  ccat<-unique(disttable$cat)
  qnoun
  anoun
  for (k in 1:length(qnoun)){
    u.noun<-qnoun[k]
    n.array<-disttable$q==u.noun
    which(n.array)
    sum(n.array)
    disttable$max.obs[n.array][which.max(disttable$freq[n.array])]<-T
  }
  typeof(disttable$freq)
  for (k in 1:length(anoun)){
    cat(k,"of",length(anoun),"\n")
    a.noun<-anoun[k]
    a.noun.array<-disttable$noun==a.noun
    
    disttable$q[a.noun.array]
    for (qn in 1:length(qnoun)){
      q.noun<-qnoun[qn]
      q.noun
      q.noun.array<-disttable$q==q.noun
      sum(q.noun.array)
      disttable[a.noun.array,]
      sum(q.noun.array)
      c<-1
      for(c in 1:length(ccat)){
        a.cat<-ccat[c]
        a.cat
        cat.array<-disttable$cat==a.cat&disttable$q==q.noun
        which(cat.array)
        disttable$q[cat.array]
        sum(cat.array)
        disttable$sum.cat[cat.array]<-sum(disttable$freq[cat.array])
        disttable$sum.all.cat[cat.array]<-sum(disttable$sum.cat[cat.array])
        
      }
      disttable$noun[q.noun.array]
      disttable$q[a.noun.array]
      
      #      disttable$sum.noun[q.noun.array]<-sum(disttable$freq[a.noun.array])
      
      
    }
      
      
  
  disttable$exp[a.noun.array]<-disttable$freq[a.noun.array]/
    disttable$sum.cat[a.noun.array]*
    sum(disttable$sum.all.cat[a.noun.array])
  
  }
  disttable$ex.dif<-abs(disttable$freq-disttable$exp)
  disttable$p<-(disttable$ex.dif*disttable$ex.dif)/disttable$freq
  disttable$freq.p<-(disttable$p*disttable$freq)
  #disttable$freq.q.p<-(disttable$freq.p*disttable$fac.q.p) # this not, makes all to B&UE
  disttable$freq.q.p<-(disttable$freq.p/disttable$fac.q.p)
  
  #print(disttable[disttable$max.obs,])
  
  for (k in 1:length(qnoun)){
    u.noun<-qnoun[k]
    q.noun.array<-disttable$q==u.noun
    which(q.noun.array)
    sum(q.noun.array)
  #  disttable$max.obs[n.array][which.max(disttable$freq[n.array])]<-T
  disttable$cat.ai[q.noun.array]<-NA
  disttable$max.p[q.noun.array][which.max(disttable$freq.p[q.noun.array])]<-T
  cat.true<-disttable$cat[q.noun.array][which.max(disttable$freq.p[q.noun.array])]
  if(length(cat.true)>0)
     disttable$cat.ai[q.noun.array]<-cat.true
  disttable$max.qp[q.noun.array][which.max(disttable$freq.q.p[q.noun.array])]<-T
  }
#  cat.true<-which(disttable$max.p==T)
 # disttable
#  print(disttable[disttable$max.p,])
 # print(disttable[disttable$q=="rose",])
  
  #print(disttable[disttable$max.p,])
  return(disttable)
  }
  
resulttable<-apply.factor(disttable)
save(resulttable,file = paste0(local,"/result_DF_qp(sample1-918)_4_m-array.RData"))
#load(paste0(local,"/result_DF_qp(sample1-918)_4_m-array.RData")) # earlier dataset with c1 recordlinkage
#print(resulttable[resulttable$max.qp,c('q','cat')])
#print(resulttable[resulttable$max.0,])
#print(resulttable[resulttable$max.n,c('q','cat')])

#distessai$nouns.df$cat.ai[distessai$nouns.df$Noun=="lake"]
############################################
### evaluate definition:
#d10.gold<-read_csv("fragrance2_ai-cats.gold.csv") # manually defined gold standard of cats
#d10.gold<-read_csv("/Volumes/EXT/boxHKW/21S/DH/local/SPUND/corpuslx/stefanowitsch/casestudy2_full.csv") # manually defined gold 
#lapsi
#d10.ai<-d10.stef$cat.ai
########################

# t.g<-table(d10.eval.log$cat)
# t.g
# t.f<-table(d10.eval.log$ai.f)
# t.f
# t.c<-table(d10.eval.log$ai.c)
# t.c
# t.cq<-table(d10.eval.log$ai.cq)
# t.cq
### > change algorithm oai.c### > change algorithm of defining cats from df
#df.known<-distessai$nouns
############################################
### > evaluate:
#distessai$df
#distessai$df[!is.na(distessai$df$cat.ai),c('Noun','cat.ai')]
#testset<-distessai$nouns.df
#temp
#m<-is.na(testset$cat.ai)
#sum(m)
#mdw<-which(duplicated(testset$Noun))
#mdw
#md<-duplicated(testset$Noun)
#sum(md)
#testset$Noun[md]
#md
###
# goldset<-d10.gold
#print(sum(testset$Token_ID==goldset$Token_ID))
#evaldf<-evalcat(goldset,testset,sampledist)
###########


### wks.
### N: only AC and B&UE are recognized, they are predefined the most.
#catsknown.t<-table(nouns.cats.known$category)
#catsknown.t
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
#evaldf$freq



#################################
# get X-square matrix from results

getfreq.df<-function(resulttable){
catarray<-resulttable$cat
cat.u<-unique(catarray)
colnames(resulttable)
a.noun.u<-unique(resulttable$noun)
q.noun.u<-unique(resulttable$q)
# l1<-length(unique(resulttable$cat))
# l2<-length(q.noun.u)
### one matrix for each q noun:
resultlist<-list()
#q<-1
#k<-1
#c<-1

#resultlist<-getfreq.df()
l1<-length(unique(resulttable$noun))
l2<-length(q.noun.u)

q<-1
k<-1
catmatrix<-matrix(nrow = l1,ncol = l2)
cat(q,"of",length(q.noun.u),"\n")
catmatrix[1:length(catmatrix)]<-NA
rownames(catmatrix)<-a.noun.u
colnames(catmatrix)<-q.noun.u
cat.df<-data.frame(catmatrix)
for (q in 1:length(q.noun.u)){
  q.noun<-q.noun.u[q]
  for(k in 1:length(a.noun.u)){
    cat(k,"of",length(a.noun.u),"\n")
    a.noun<-a.noun.u[k]
  #  for(c in 1:length(cat.u)){
   #   cat(c,"of",length(cat.u),"\n")
    #  q.cat<-cat.u[c]
      # cat.df[c,k]<-0
#      m<-resulttable$freq[resulttable$noun==a.noun&resulttable$q==q.noun]
      freq<-resulttable$freq[resulttable$noun==a.noun&resulttable$q==q.noun]
      if(length(freq)>0)
        cat.df[k,q]<-freq
      
    }
 # }
  q.noun.sing<-cat.df[,q]
  names(q.noun.sing)<-rownames(cat.df)
  resultlist[[q.noun]]<-q.noun.sing
}

return(resultlist)
}
#cat.df.sf<-cat.df
#xdf<-getfreq.df()
resultlist<-getfreq.df(resulttable) #loaded from RData
#save(resultlist,file = paste0(local,"/result_DF_m-array(sample1-918)_singled-list.RData"))
#save(cat.df,file = paste0(local,"/catdf_freq(sample1-918).RData"))

# qnoun<-"crab"
# ex.df<-list()
# qrose<-resultlist[[qnoun]]
# anoun<-"lane"
# cat<-"MA"
# k<-1
# qnoun<-"rose"
a.noun.u<-unique(nouns.cats.known$noun)
cat.u<-unique(nouns.cats.known$category)
get.exp<-function(qnoun){
  t.df<-data.frame(resultlist[[qnoun]])
  #t.df<-data.frame(resultlist[qnoun])
  colnames(t.df)[1]<-"freq"
  t.df$cat<-NA
  t.df$qsum<-NA
  t.df$csum<-NA
  t.df$exp<-NA
  t.df$dif<-NA
  t.df$p<-NA
  t.df$score<-NA
  t.df$fac.c<-NA
  t.df$score.sum<-NA
  t.df$cat.p.sum<-NA
  k<-1
  #t.cat<-matrix(ncol = length(a.noun.u),nrow = length(cat.u))
  for(k in 1:length(a.noun.u)){
    cat(k,"\n")
    a.noun<-a.noun.u[k]
    m<-nouns.cats.known$noun%in%a.noun
    sum(m)
    cat<-unique(nouns.cats.known$category[m])
    m2<-rownames(t.df)%in%a.noun
    t.df$cat[m2]<-cat
    }
    q.cat.t<-table(t.df$cat)
    #q.cat.t<-fac.u
    q.cat.t # this factor applies to all q.nouns
    q.cat.exp<-q.cat.t/sum(q.cat.t)*q.cat.t
    q.cat.exp
    q.cat.dif<-q.cat.exp-q.cat.t
    q.cat.dif
    q.cat.p<-q.cat.dif*q.cat.dif/q.cat.exp
    q.cat.p
    k<-1
    for(k in 1:length(rownames(q.cat.p))){
    m3<-t.df$cat%in%rownames(q.cat.p)[k]
    m3
    cat<-rownames(q.cat.p)[k]
    sum(m3)
    if(sum(t.df$freq[m3])==0)
      t.df$freq[m3]<-0.00001 #set minimal frequency for 0 matches
    t.df$csum[m3]<-sum(t.df$freq[m3]) # cat sum per qnoun&cat
   # m4<-names(q.cat.p)%in%cat
    t.df$fac.c[m3]<-q.cat.p[k]
    }
#}
#  m4
 # qnoun<-"rose"
  #qnoun.array<-disttable$q==qnoun
  #sum(qnoun.array) # 55 assigned cat frequencies
  
### in a distribution matrix: 
### rowsums = catsum, colsum = a.noun.sum = freq, total = sum(catsum) = q.sum
### chi formula exp (1): token(freq)/total*catsum
### chi formula dif (2): exp - freq
### chi formula p:  (3): (dif*dif)/exp
### p.freq : freq*p
#  t.qsum<-sum(t.df$freq,na.rm = T)
  t.df$qsum<-sum(t.df$freq,na.rm = T)
  t.df$exp<-t.df$freq/t.df$qsum*t.df$csum
  t.df$dif<-t.df$exp-t.df$freq
  t.df$p<-(t.df$dif*t.df$dif)/t.df$exp
  k<-1
  t.df$score<-t.df$freq*t.df$p
  t.df$score.c<-t.df$csum*t.df$fac.c
  for(k in 1:length(a.noun.u)){
    cat(k,"\n")
    a.noun<-a.noun.u[k]
    m<-rownames(t.df)%in%a.noun
    #cat<-unique(resulttable$cat[m])
    m2<-rownames(t.df)%in%a.noun
    cat<-unique(t.df$cat[m])
    #t.df$cat[m2]<-cat
    #q.cat.t<-table(t.df$cat)
    #q.cat.t<-fac.u
    #q.cat.t # this factor applies to all q.nouns
    #q.cat.exp<-q.cat.t/sum(q.cat.t)*q.cat.t
    #q.cat.exp
    #q.cat.dif<-q.cat.exp-q.cat.t
    #q.cat.dif
    #q.cat.p<-q.cat.dif*q.cat.dif/q.cat.exp
    #q.cat.p
    m3<-t.df$cat%in%cat
    m3
    cat
    sum(m3)
    #    t.df$cat.qsum[m3]<-sum(t.df$freq[m3])
    #m4<-names(q.cat.p)%in%cat
    t.df$score.sum[m3]<-sum(t.df$score[m3])
    t.df$cat.p.sum[m3]<-sum(t.df$p[m3])
  }
  #t.df$score.cs<-t.df$score.sum*t.df$fac.c
  t.df$score.cs<-t.df$cat.p.sum/t.df$csum
  t.df$freq.c<-t.df$freq*t.df$fac.c
  #t.df$score.catsum<-t.df$cat.p.sum/t.df$csum
  max<-which.max(t.df$freq.c)
  freq.c<-t.df$cat[max]
  max<-which.max(t.df$score.sum)
  c.max<-t.df$cat[max]
  max<-which.max(t.df$score.c)
  c.max.c<-t.df$cat[max]
  max<-which.max(t.df$freq)
  c.max.f<-t.df$cat[max]
  max<-which.max(t.df$score.cs)
  c.max.score<-t.df$cat[max]
  max<-which.max(t.df$csum)
  f.max.catsum<-t.df$cat[max]
  return(list(df=t.df,f.max=c.max.f,f.max.catsum=f.max.catsum,freq.c=freq.c,c.max=c.max,cq.max=c.max.c,c.max.score=c.max.score))
  
    #  ex.df[[noun]]<-exp
}
qnoun<-"tradition"
#get.exp.df<-function(qnoun){

 q.cat<-get.exp(qnoun)
 qdf<-q.cat$df
 q.cat$f.max.catsum
#q.cat$
#k<-1
put.max.cat<-function(){
  #d10.stef$cat.ai<-NA
  for(k in 1:length(d10.stef$Noun)){
    qnoun<-d10.stef$Noun[k]
    cat("fetch",k,"of",length(d10.stef$Noun), "category for:",qnoun,"---> ")
    max.list<-get.exp(qnoun)
    
    max.c.cat<-max.list$c.max
    max.f.cat<-max.list$f.max
    max.cq.cat<-max.list$cq.max
    max.score.f<-max.list$c.max.score
    max.freq.c<-max.list$freq.c
    max.catsum<-max.list$f.max.catsum
        cat(max.f.cat,max.catsum,max.freq.c,max.c.cat,max.cq.cat,"\n")
    d10.stef$cat.ai.f[k]<-max.f.cat
    d10.stef$cat.ai.f.catsum[k]<-max.catsum
    d10.stef$cat.ai.fc[k]<-max.freq.c
    d10.stef$cat.ai.c[k]<-max.c.cat
    d10.stef$cat.ai.cq[k]<-max.cq.cat
    d10.stef$cat.ai.score[k]<-max.score.f
  }
  for (k in 1:length(a.noun.u)){
    a.noun<-a.noun.u[k]
    m<-d10.stef$Noun%in%a.noun
    m2<-nouns.cats.known$noun%in%a.noun
    cat<-unique(nouns.cats.known$category[m2])
    d10.stef$Category[m]<-cat
  }
  return(d10.stef)
}

d10.stef.ai<-put.max.cat()
### there is a problem with /lunchroom/ B&UE, which matches in frequency even if a.noun = q.noun (e.g. tradition)
### test:
testmatch<-function(){
  anoun<-"tradition"
  qnoun<-"lunchroom"
  d1u<-nouns.cats.known$collocations[nouns.cats.known$noun==anoun]
  d2u<-nouns.cats.known.cpt$collocations[nouns.cats.known.cpt$noun==qnoun]
  em<-d2u%in%"#empty#"
  sum(em)
  d2u<-d2u[!em]
  d2u<-c(d2u,"belong")
  c1<-compare.linkage(matrix(d1u),matrix(d2u))
  c1$frequencies
c1
  }
#################################
evalscore<-function(goldset,testset){
  
}
#################################
### evaluation:
evalcat<-function(goldset,testset){
  df<-goldset
  d10.gs<-df[with(df,order(df[,"Token_ID"])), ]
  df<-testset
  d10.ai.s<-df[with(df,order(df[,"Token_ID"])), ]
  d10.ai.s$cat.ai.f<-gsub("B&UE","B&AE",d10.ai.s$cat.ai.f)
  d10.ai.s$cat.ai.c<-gsub("B&UE","B&AE",d10.ai.s$cat.ai.c)
  d10.ai.s$cat.ai.f.catsum<-gsub("B&UE","B&AE",d10.ai.s$cat.ai.f.catsum)
  
  d10.ai.s$cat.ai.cq<-gsub("B&UE","B&AE",d10.ai.s$cat.ai.cq)
  d10.ai.s$Category<-gsub("B&UE","B&AE",d10.ai.s$Category)
  chks<-d10.ai.s$Token_ID==d10.gs$Token_ID
  sum(chks)
  ### check inconsitency training vs. gold:
  m<-!is.na(d10.ai.s$Category)
  d10.gs$cat.cons<-d10.gs$Category
  d10.gs$cat.cons[m]<-d10.ai.s$Category[m]
  p1<-d10.gs$cat.cons==d10.ai.s$cat.ai.f
  p1<-d10.gs$cat.cons==d10.ai.s$cat.ai.f
  sum(p1)
  p2<-d10.gs$cat.cons==d10.ai.s$cat.ai.c
  p3<-d10.gs$cat.cons==d10.ai.s$cat.ai.cq
  p4<-d10.gs$cat.cons==d10.ai.s$cat.ai.f.catsum
  sum(p4)
  p.array<-data.frame(match.sum=1:4,match.freq=1:4,cpt=918,pre=92,minus=918-92,match.clean=1:4)
  p.array$match.sum[1]<-print(sum(p1,na.rm = T))
  p.array$match.freq[1]<-print(sum(p1,na.rm = T)/length(p1))
  p.array$match.sum[2]<-print(sum(p2,na.rm = T))
  p.array$match.freq[2]<-print(sum(p2,na.rm = T)/length(p2))
  p.array$match.sum[3]<-print(sum(p3,na.rm = T))
  p.array$match.freq[3]<-print(sum(p3,na.rm = T)/length(p3))
  p.array$match.sum[4]<-print(sum(p4,na.rm = T))
  p.array$match.freq[4]<-print(sum(p4,na.rm = T)/length(p4))
  p.array$BUE<-sum(d10.gs$cat.cons=="B&AE")
  p.array$match.clean<-p.array$match.sum-p.array$pre
  p.array$match.clean.freq<-p.array$match.clean/p.array$minus
  p.array$match.clean.freq.BUE<-(p.array$match.clean-p.array$BUE)/p.array$minus
  rownames(p.array)<-c("meth:raw.freq","meth:cat.c","meth:cat.cq","meth:catsum")
  # print(sum(p1,na.rm = T))
  # print(sum(p1,na.rm = T)/length(p1))
  # print(sum(p2,na.rm = T))
  # print(sum(p2,na.rm = T)/length(p2))
  # print(sum(p3,na.rm = T))
  # print(sum(p3,na.rm = T)/length(p3))
  dfreturn<-data.frame(noun=d10.gs$Noun,cat.pre=d10.ai.s$Category,gold=d10.gs$Category,cat.cons=d10.gs$cat.cons,ai.f=d10.ai.s$cat.ai.f,ai.c=d10.ai.s$cat.ai.c,ai.cq=d10.ai.s$cat.ai.cq)
  #evallist<-array(cat.freq=p.array)
  returnlist<-list(df=dfreturn,eval=p.array)
  return(returnlist)
  # d10.ai.s$Category<-d10.gs$Category
  ############################
  ##################################
}
goldset<-d10.gold
testset<-d10.stef.ai
eval.set<-evalcat(goldset,testset)
evals<-eval.set$eval
evaldf<-eval.set$df
save(evals,file=paste0(local,"cats_eval-assignment_m-array.csv"))
### no. try with freq*p
evalfrequencies.obs<-function(){
  ### process:
  sampledist<-sample(1:length(d10.stef$Corpus),100)
  distessai<-catcall(sampledist,ai.form)
  testset<-distessai$nouns.df
  goldset<-d10.gold
  evaldf<-evalcat(goldset,testset,sampledist)
  distessai$dist.df
  ###
  df<-data.frame(evaldf$freq)
 # library(collostructions)
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

# 
############################
### TODO: a routine which after fetching the eval result modifies the factor that scores the cat definition and feeds that
### into the next run. small steps of modifying will change the cat definition, if that results in more correct definitions
### the direction of modifying is considered good. the algorithm of change can be random with documenting the effect. that
### effect of change will be measured and evaluated via lmer, so that it becomes clear which changes have the greatest effect, 
### like (lmer df model)





################ scribble
### theres a package to easily get frequencies of matches:
#library(RecordLinkage)






tempfun1<-function(){
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
}
