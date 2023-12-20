#20231220(12.35)
#14515.model.get cats
#####################
#####################
library(stringi)
a1<-LETTERS
a2<-letters
we1<-c("e1word","ee2word","eee3word","eeee4word","eeeee5word")
wa1<-c("a1word","aa2word","aaa3word","aaaa4word","aaaaa5word")
wu1<-c("u1word","uu2word","uuu3word","uuuu4word","uuuuu5word")

w.c<-c(we1,wa1,wu1)
w.array<-data.frame(noun=w.c,cat=NA)
w.train<-data.frame(noun=sample(w.c,7),cat=NA)
#k<-1

get.trainset<-function(){
for(k in 1:length(w.train$noun)){
  word<-w.train$noun[k]
  c.split<-stri_split_boundaries(word,type="char",simplify = T)
  c.split
  c.count<-table(factor(c.split))
  c.count
  w.split<-stri_split_regex(word,"word",simplify = T)
  w.train$cat[k]<-w.split[,1]

}
  return(w.train)
}
w.train<-get.trainset()
# coll.array<-list()
# for(k in 1:length(w.c)){
#   word<-w.c[k]
#   c.split<-stri_split_boundaries(word,type="char")
#   coll.array[[word]]<-c.split
# }

get.cat.known.df<-function(noun){
  nouns.df<-data.frame(noun=NA,coll=NA,cat=NA,factor=NA,fac.c=NA,fac.p=NA)
  k<-1
  for(k in 1:length(w.train$noun)){
    word<-w.train$noun[k]
    word
    c.split<-stri_split_boundaries(word,type="char",simplify = T)
    c.split<-t(c.split)
    #nouns.n[['coll']]<-c.split
    #nouns.n[['word']]<-word
    #nouns.n[['cat']]<-w.train$cat[k]
    nouns.df.temp<-data.frame(noun=word,coll=c.split,cat=w.train$cat[k])
    c.factor<-length(c.split)
    nouns.df.temp$factor<-1/c.factor
    d.c.f.0<-factor(nouns.df.temp$cat)
    d.c.f.0
    d.c.t.0<-table(d.c.f.0)
    d.c.t.0 # cat frequency overall set, this factor has to be taken into account 
    #  k<-1
    for (k2 in 1:length(d.c.t.0)){
      m2<-nouns.df.temp$cat==names(d.c.t.0[k2])
      sum(m2)
      nouns.df.temp$fac.c[m2]<-1/d.c.t.0[k2]
    }
    nouns.df.temp$fac.p<-nouns.df.temp$factor*nouns.df.temp$fac.c
    nouns.df<-rbind(nouns.df,nouns.df.temp)
    nouns.df<-nouns.df[!is.na(nouns.df$noun),]
    
  }
  return(nouns.df)
}

#nouns.n
nouns.df<-get.cat.known.df("xx")

get.cat.no.df<-function(noun){
  nouns.df.no<-data.frame(w.array)
  k<-8
  k
#  w.array<-data.frame()
  for(k in 1:length(nouns.df.no$noun)){
    word<-nouns.df.no$noun[k]
    word
    c.split<-stri_split_boundaries(word,type="char",simplify = T)
    c.split<-t(c.split)
    colls<-c.split
    colls
    m<-nouns.df$coll%in%colls
    m.coll<-nouns.df$coll[m]
    m.coll
    m.coll.n<-nouns.df$cat[m]
    m.coll.n
    m.coll.t<-table(factor(m.coll.n))
    m.coll.t
    max.coll.n<-which.max(table(factor(m.coll.n)))
    max.coll.n # this works!
    max.coll.ns<-names(max.coll.n)
    max.coll.ns
    mf<-nouns.df$cat==names(m.coll.t)
    nouns.df$cat[mf]
    max.coll.p<-m.coll.t/nouns.df$fac.c[mf]
    max.coll.p
    max.coll.cat<-which.max(max.coll.p)
    word
    max.coll.cat
    max.f<-which.max(table(nouns.df$factor[m]))*as.double(names(which.max(table(nouns.df$factor[m]))))
    max.f
    max.f<-max(table(nouns.df$factor[m]))*as.double(names(which.max(table(nouns.df$factor[m]))))    
    max.t<-table(nouns.df$factor[m])*as.double(names(table(nouns.df$factor[m])))
    max.t
    max.cat<-names(table(nouns.df$cat[names(max.coll.n)==nouns.df$noun]))
    max.s<-max.t*as.double(names(max.t))
    max.s2<-which.max(max.s)
    #max.cat<-max.f
    names(table(nouns.df$cat))
    #max.cat<-names(table(nouns.df$cat[max.s2]))
    word
    ############################################
    m
tempfun1<-function(){
        d.c.f<-factor(nouns.df$cat[m])
    d.c.f
    d.c.t<-table(d.c.f)
    d.c.t
    #d.c.f.0<-
    d.c.f.0<-factor(nouns.df$cat)
    d.c.f.0
    d.c.t.0<-table(d.c.f.0)
    d.c.t.0 # cat frequency overall set, this factor has to be taken into account 
  #  k<-1
    for (k2 in 1:length(d.c.t.0)){
      m2<-nouns.df$cat==names(d.c.t.0[k2])
    sum(m2)
      nouns.df$fac.c[m2]<-1/d.c.t.0[k2]
    }
    nouns.df$fac.p<-nouns.df$factor*nouns.df$fac.c
    table(nouns.df$fac.p[m])
    d.c.f.n<-factor(nouns.df$noun[m])
    d.c.f.n
    d.c.f.n.t<-table(d.c.f.n)
    d.c.f.n.t
    # d.c.f.f<-factor(nouns.cats.known$unique[m.coll])
    # d.c.f.f
    d.c.f.c<-factor(nouns.df$coll[m])
    d.c.f.c
    d.c.f.c.t<-table(d.c.f.c)
    d.c.f.c.t
    # df.dcf<-data.frame(cat=as.character(d.c.f))
    # df.dcf$noun<-as.character(d.c.f.n)
    # #df.dcf$unique<-as.double(as.character(d.c.f.f))
    # co<-1
    # for(co in 1:length(names(d.c.f.n.t))){
    #   df.dcf$matches[df.dcf$noun==names(d.c.f.n.t)[co]]<-sum(d.c.f.n==names(d.c.f.n.t)[co])
    #   #  df.dcf$matches[df.dcf$noun==names(d.c.f.n.t)[co]]<-d.c.f.n.t[co]
    # }
}
    
    df.s<-data.frame(cat=names(m.coll.t),match=m.coll.t,score=NA,row.names = names(m.coll.t))
    #k<-1
    k
    c<-2
   # df.dcf$factor<-      #TODO
    for(c in 1:length(df.s$cat)){
      ck<-nouns.df$fac.p[nouns.df$cat==df.s$cat[c]]
      ck<-nouns.df$fac.p[nouns.df$cat==df.s$cat[c]]
      df.s$factor[c]<-sum(ck)
      df.s$score<-df.s$match.Freq*df.s$factor
    }
    
    ##############################################
    catfinal.coll<-df.s$cat[which.max(df.s$score)]
    word
    max.cat<-catfinal.coll
    #max.cat<-names(max.coll.cat)
    max.cat
    ############################################
    #max.cat<-nouns.df$cat[nouns.df$noun==names(max.coll.n)]
    #m.coll<-unique(m.coll.n)
    #max.coll<-which.max(table(factor(m.coll.n)))
    #max.cat<-names(max.coll)
    # m.coll.c<-nouns.df$cat[m]
    # m.coll.c<-unique(m.coll.c)
    # max.coll.c<-which.max(table(factor(m.coll.c)))
    # max.cat.c<-names(max.coll.c)
    nouns.df$noun[k]
    ifelse(length(max.cat)>0,nouns.df.no$cat[k]<-max.cat,nouns.df.no$cat[k]<-NA)
  }
  return(nouns.df.no)
}

cats.no<-get.cat.no.df("xx")
#nouns.n
#co.list<-list()







