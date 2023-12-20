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
w.train<-data.frame(noun=sample(w.c,5),cat=NA)
k<-1
for(k in 1:length(w.train$noun)){
  word<-w.train$noun[k]
  c.split<-stri_split_boundaries(word,type="char",simplify = T)
  c.split
  c.count<-table(factor(c.split))
  c.count
  w.split<-stri_split_regex(word,"word",simplify = T)
  w.train$cat[k]<-w.split[,1]

}
w.train
coll.array<-list()
for(k in 1:length(w.c)){
  word<-w.c[k]
  c.split<-stri_split_boundaries(word,type="char")
  coll.array[[word]]<-c.split
}

get.cat.known.df<-function(noun){
  nouns.df<-data.frame(noun=NA,coll=NA,cat=NA,factor=NA)
  k<-1
  for(k in 1:length(w.train$noun)){
    word<-w.train$noun[k]
    c.split<-stri_split_boundaries(word,type="char",simplify = T)
    c.split<-t(c.split)
    #nouns.n[['coll']]<-c.split
    #nouns.n[['word']]<-word
    #nouns.n[['cat']]<-w.train$cat[k]
    nouns.df.temp<-data.frame(noun=word,coll=c.split,cat=w.train$cat[k])
    c.factor<-length(c.split)
    nouns.df.temp$factor<-1/c.factor
    nouns.df<-rbind(nouns.df,nouns.df.temp)
    nouns.df<-nouns.df[!is.na(nouns.df$noun),]
    
  }
  return(nouns.df)
}

nouns.n
nouns.df<-get.cat.known.df("xx")

get.cat.no.df<-function(noun){
  nouns.df.no<-data.frame(w.array)
  k<-7
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
    m.coll.n<-nouns.df$noun[m]
    m.coll.n
    max.coll.n<-which.max(table(factor(m.coll.n)))
    max.f<-max(table(nouns.df$factor[m]))*as.double(names(which.max(table(nouns.df$factor[m]))))
    max.f<-max(table(nouns.df$factor[m]))*as.double(names(which.max(table(nouns.df$factor[m]))))    
    max.t<-table(nouns.df$factor[m])*as.double(names(table(nouns.df$factor[m])))
    max.cat<-names(table(nouns.df$cat[names(max.coll.n)==nouns.df$noun]))
    max.s<-max.t*as.double(names(max.t))
    max.s2<-which.max(max.s)
    #max.cat<-max.f
    names(table(nouns.df$cat))
    max.cat<-names(table(nouns.df$cat[max.s2]))
    #max.cat<-nouns.df$cat[nouns.df$noun==names(max.coll.n)]
    #m.coll<-unique(m.coll.n)
    #max.coll<-which.max(table(factor(m.coll.n)))
    #max.cat<-names(max.coll)
    # m.coll.c<-nouns.df$cat[m]
    # m.coll.c<-unique(m.coll.c)
    # max.coll.c<-which.max(table(factor(m.coll.c)))
    # max.cat.c<-names(max.coll.c)
    ifelse(length(max.cat)>0,nouns.df.no$cat[k]<-max.cat,nouns.df.no$cat[k]<-NA)
  }
  return(nouns.df.no)
}
k
cats.no<-get.cat.no.df("xx")
nouns.n
co.list<-list()
for (k in 1:length(we))
co1<-stri_split_boundaries()







