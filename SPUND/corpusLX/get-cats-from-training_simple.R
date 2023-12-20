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
d10.stef<-read.csv("~/boxHKW/21S/DH/local/SPUND/corpuslx/stefanowitsch/CaseStudy2_FullData.csv")

#from saved df
nouns.cats.known<-read.csv("fragrance_known-cats_coll.cpt.csv")
#########################################################
#########################################################
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
  d2u<-nouns.cats.known$collocations[nouns.cats.known$noun==noun]
  d2u<-unique(d2u)
  #l<-length(d.c.u$Gramrels$Words)
  l<-length(d2u)
  ########
  if(l>1){
    #  word.no<-d.c.u$Gramrels$Words[[l]]['word']
    word.no<-"#empty#"
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
  which.max(table(factor(nouns.cats.known$noun[m.coll])))
  m.coll.t<-table(factor(nouns.cats.known$noun[m.coll]))[order(table(factor(nouns.cats.known$noun[m.coll])))]
  ### discard:
  m.coll.mf<-tail(table(factor(nouns.cats.known$noun))[order(table(factor(nouns.cats.known$noun)))],10)
  m.coll.mf
  coll.disc<-names(tail(m.coll.mf,2)) # most frequent collocates over all /thing/ + /place/
  m.disc<-names(m.coll.t)%in%coll.disc
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
  m.coll.disc.n<-nouns.cats.known$noun[m.coll]%in%coll.disc
  m.coll.disc.c<-nouns.cats.known$collocations[m.coll]%in%coll.disc
  m.coll.sub<-nouns.cats.known[m.coll,]%in%coll.disc
  m.coll.disc.w<-which(nouns.cats.known$noun[m.coll]%in%coll.disc)
  sum(m.coll.disc.n)
  sum(m.coll.disc.c)
  sum(m.coll.disc.n)
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
  returnlist<-list(dist=cats.dist.df,nouns.df=nouns.cats.known)
  return(returnlist)
  return(cats.dist.df)
} #end get.dist.df.g()
#########################################################


dmax1<-get.dist.df.g("rose")
print(dmax1)
k<-1
dist.list<-list()
range.df<-1:10
############################
catcall<-function(range.df){
d10.stef$cat.ai<-NA
#range.df<-1:length(d10.stef$Noun
for(k in range.df){
  q<-d10.stef$Noun[k]
  q
  df<-get.dist.df.g(q)
  
  maxcat<-df$dist$cat[which(df$dist$max==T)]
  maxcat
  d10.stef$cat.ai[k]<-maxcat
  cat("run",k,q,maxcat,"\n")
  dist.list[['eval']][[q]]<-df$dist
  dist.list[['nouns']]<-df$nouns.df
}
cat.df<-data.frame(dist.list$nouns)
returnlist<-list(cat=dist.list,df=d10.stef,nouns=cat.df)
return(returnlist)
}
sampledist<-sample(1:length(d10.stef$Corpus),100)
sampledist
distessai<-catcall(sampledist)
distessai
############################################
### > evaluate:
#distessai$df
distessai$df[!is.na(distessai$df$cat.ai),c('Noun','cat.ai')]
testset<-distessai$df
goldset<-d10.gold
#print(sum(testset$Token_ID==goldset$Token_ID))
evaldf<-evalcat(goldset,testset,sampledist)
#evaldf
### wks.
### N: only AC and B&UE are recognized, they are predefined the most.
catsknown.t<-table(nouns.cats.known$category)
catsknown.t
############################################
### evaluate definition:
#d10.gold<-read_csv("fragrance2_ai-cats.gold.csv") # manually defined gold standard of cats
#d10.gold<-read_csv("/Volumes/EXT/boxHKW/21S/DH/local/SPUND/corpuslx/stefanowitsch/casestudy2_full.csv") # manually defined gold 
#lapsi
d10.gold<-read_csv("~/boxHKW/21S/DH/local/SPUND/corpuslx/stefanowitsch/casestudy2_full.csv") # manually defined gold 
#d10.ai<-d10.stef$cat.ai
############
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
return(d10.ai.s)
############################
##################################
}
### N: theres too many AC coded, this is the only cat recognized correctly