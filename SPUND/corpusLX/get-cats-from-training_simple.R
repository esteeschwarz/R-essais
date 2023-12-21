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
d10.stef<-read.csv("~/boxHKW/21S/DH/local/SPUND/corpuslx/stefanowitsch/CaseStudy2_FullData.csv")
d10.gold<-read_csv("~/boxHKW/21S/DH/local/SPUND/corpuslx/stefanowitsch/casestudy2_full.csv") # manually defined gold 

#from saved df
#nouns.cats.known<-read.csv("fragrance_known-cats_coll.cpt.csv")
nouns.cats.known<-read.csv("fragrance_known-cats_coll.cpt.csv")
nouns.cats.known.fix<-read.csv("nouns.cats.known.csv") # modeled df of fixed cats, 8274 obs
nouns.cats.known.cpt<-read.csv("nouns.cats.temp_918.csv")
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
} #end get.dist.df.g()
#########################################################
### function from model:
#range.df<-1:10
noun.q<-"pit-latrine"
get.cat.no.df<-function(noun.q){
#  nouns.df.no<-data.frame(w.array)
  nouns.df.no<-d10.stef
  nouns.df.no$cat.ai<-NA
  k<-8
  nouns.cats.known$fac.p<-1/nouns.cats.known$unique
  #  w.array<-data.frame()
  # for(k in 1:length(nouns.df.no$Noun[range.df])){
  #   noun<-nouns.df.no$Noun[k]
  noun<-noun.q
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
      #  word.no<-d.c.u$Gramrels$Words[[l]]['word']
      #  word.no$word
      #  m.pos<-d2u%in%word.no$word
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
    table(factor(nouns.cats.known$noun[m.coll.b]))
    ### highest match in cat:
    catfactor<-data.frame(cat=c(unique(nouns.cats.known$category),"n.a."),fac.p=NA)
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
 #   m.disc<-names(m.coll.t)%in%coll.disc
  #  m.disc
   # sum(m.disc)
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
    ###########################################
    
    
    # c.split<-stri_split_boundaries(word,type="char",simplify = T)
    # c.split<-t(c.split)
    # colls<-c.split
    # colls
    # m<-nouns.df$coll%in%colls
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
    #max.coll.p<-m.coll.t/nouns.df$fac.c[mf]
    #max.coll.p
    #max.coll.cat<-which.max(max.coll.p)
    noun
    #max.coll.cat
    # max.f<-which.max(table(nouns.df$factor[m]))*as.double(names(which.max(table(nouns.df$factor[m]))))
    # max.f
    # max.f<-max(table(nouns.df$factor[m]))*as.double(names(which.max(table(nouns.df$factor[m]))))    
    # max.t<-table(nouns.df$factor[m])*as.double(names(table(nouns.df$factor[m])))
    # max.t
    # max.cat<-names(table(nouns.df$cat[names(max.coll.n)==nouns.df$noun]))
    # max.s<-max.t*as.double(names(max.t))
    # max.s2<-which.max(max.s)
    # #max.cat<-max.f
    names(table(nouns.df$cat))
    #max.cat<-names(table(nouns.df$cat[max.s2]))
    #word
    ############################################
    #m
    df.s<-data.frame(cat="n.a",match=NA,score=NA,row.names = "n.a.",max=T)
    max.cat<-"n.a"
    cats.dist.df<-df.s
    if(length(names(m.coll.t))>0){
      df.s<-data.frame(cat=names(m.coll.t),match=m.coll.t,score=NA,row.names = names(m.coll.t),max=F)
    
    #df.s<-data.frame(cat=names(m.coll.t),match=m.coll.t,score=NA,row.names = names(m.coll.t),max=F)
    #k<-1
    #k
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
    
    }
    maxcore<-which.max(df.s$score)
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
    #max.cat<-nouns.df$cat[nouns.df$noun==names(max.coll.n)]
    #m.coll<-unique(m.coll.n)
    #max.coll<-which.max(table(factor(m.coll.n)))
    #max.cat<-names(max.coll)
    # m.coll.c<-nouns.df$cat[m]
    # m.coll.c<-unique(m.coll.c)
    # max.coll.c<-which.max(table(factor(m.coll.c)))
    # max.cat.c<-names(max.coll.c)
    #k
    #nouns.df$noun[k]
    length(max.cat)
    ifelse(length(max.cat)>0,nouns.df.no$cat.ai[k]<-max.cat,nouns.df.no$cat.ai[k]<-NA)
  #}
  returnlist<-list(dist=cats.dist.df,nouns.df=nouns.df.no)
  return(returnlist)
  return(nouns.df.no)
} #end get.cat.no.df()

#########################################################
dmax1<-get.cat.no.df("pit-latrine")
dmax1<-get.dist.df.g("rose")
print(dmax1$dist)
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
  #################### model
  df<-get.cat.no.df(q)
  df$dist$cat
  df$dist$max
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
##############################
distessai<-catcall(sampledist)
distessai
############################################
### evaluate definition:
#d10.gold<-read_csv("fragrance2_ai-cats.gold.csv") # manually defined gold standard of cats
#d10.gold<-read_csv("/Volumes/EXT/boxHKW/21S/DH/local/SPUND/corpuslx/stefanowitsch/casestudy2_full.csv") # manually defined gold 
#lapsi
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
### > change algorithm of defining cats from df
df.known<-distessai$nouns
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

### N: theres too many AC coded, this is the only cat recognized correctly