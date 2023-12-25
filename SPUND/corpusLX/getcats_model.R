#20231220(12.35)
#14515.model.get cats
#####################
#####################
library(stringi)
library(rbenchmark)
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
  w.split<-stri_split_regex(word,"[0-9]",simplify = T)
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
#nounset<-w.array
get.cat.known.df<-function(nounset){
  nouns.df<-data.frame(noun=NA,coll=NA,cat=NA,factor=NA,fac.c=NA,fac.p=NA)
  k<-1
  w.train<-nounset
  for(k in 1:length(w.train$noun)){
    word<-w.train$noun[k]
    word
    c.split<-stri_split_boundaries(word,type="char",simplify = T)
    c.split<-t(c.split)
    #nouns.n[['coll']]<-c.split
    #nouns.n[['word']]<-word
    #nouns.n[['cat']]<-w.train$cat[k]
    cat.x<-w.train$cat[k]
    nouns.df.temp<-data.frame(noun=word,coll=c.split,cat=cat.x,fac.c=NA,fac.p=NA)
    c.factor<-length(c.split)
    nouns.df.temp$factor<-1/c.factor
    if(!is.na(cat.x)){
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
    }
    nouns.df.temp$fac.p<-nouns.df.temp$factor*nouns.df.temp$fac.c
    nouns.df<-rbind(nouns.df,nouns.df.temp)
    nouns.df<-nouns.df[!is.na(nouns.df$noun),]
    
  }
  return(nouns.df)
}

#nouns.n
nouns.df<-get.cat.known.df(w.train)
nouns.df.known<-get.cat.known.df(w.train)

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
    #max.coll.p<-m.coll.t/nouns.df$fac.c[mf]
    #max.coll.p
    #max.coll.cat<-which.max(max.coll.p)
    word
    #max.coll.cat
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
############################
#wks., now with record linkage:
###############################
library(RecordLinkage)
nouns.df.known<-get.cat.known.df(w.train)
nouns.df.unknown<-get.cat.known.df(w.array)
testset<-nouns.df.unknown
trainset<-nouns.df.known
varset<-c("noun","coll","cat")
length(unlist(testset[varset[1]]))

getrecords<-function(trainset,testset,varset){
  ldf<-length(unlist(testset[varset[1]]))
  ldv<-length(varset)
  ldm<-ldf*ldv
  temp.set<-matrix(1:ldm,ncol=length(varset))
  temp.set[1:ldm]<-NA
  temp.set<-data.frame(temp.set)
  colnames(temp.set)<-varset
for (k in 1:ldv){
  temp.set[varset[k]]<-testset[varset[k]]
}
### wks., df from global testset
###############################
### now record linkage:
### get train array for noun
  k<-1
  noun<-varset[1]
  coll<-varset[2]
  q.noun.u<-unique(testset[[noun]])
  q.noun.u
  a.noun.u<-unique(trainset[[noun]])
  a.noun.u
  lq<-length(q.noun.u)
  q.list<-list()
  temp.set$freq<-NA
  k<-1
  q.array<-rep(q.noun.u,length(a.noun.u))
  q.array
  a.array<-rep(a.noun.u,length(q.noun.u))
  lqa<-length(q.array)
  laa<-length(a.array)
  sum(q.array==a.array)
  #eval.a-array<-rep(a.noun.u,)
  eval.set<-data.frame(a.noun=a.array,q.noun=q.array,cat=NA,freq=NA,score=NA,max.obs=F,max.score=F)
  ################
    a<-1
    for (a in 1:length(eval.set$q.noun)){
      anoun<-eval.set$a.noun[a]
      qnoun<-eval.set$q.noun[a]
      k<-1
      cat("run -",a,"- for:",qnoun,"-----\n")
      
      #for(k in 1:lqa){
       # qnoun<-q.array[k]
        d2.sel<-temp.set[[noun]]%in%qnoun
        d2u<-testset[[coll]][d2.sel]
        #  a<-1
       # d3.sel<-eval.set$q.noun%in%qnoun
        #sum(d3.sel)
        cat("match freq for--:",qnoun,a,k,"in:",anoun," --- >")
    d1.sel<-trainset[[noun]]%in%anoun
    #d4.sel<-
    d1u<-matrix(trainset[[coll]][d1.sel])
    d2u<-matrix(d2u)
    #d4.sel<-k*a
    c1<-compare.linkage(d1u,d2u)
    q.list[[qnoun]]<-c1
    # eval.set$a.noun[k]<-anoun
    # eval.set$freq[k]<-c1$frequencies
    # d.fac<-sum(d1.sel)+sum(d2.sel) # number of collocates
    d.fac<-sum(d1.sel)+sum(d2.sel) # number of collocates
    freq.f<-c1$frequencies/d.fac
    # freq.f<-c1$frequencies/d.fac
     cat(anoun,c1$frequencies,"f:",freq.f,"\n") #wks., highest shortest match TRUE
     # eval.set$score[k]<-freq.f
   # eval.set$a.noun[d3.sel]<-anoun
     #eval.set$a.noun[k]<-anoun
     eval.set$freq[a]<-c1$frequencies
     eval.set$score[a]<-freq.f
      #}  
      
    }
  k<-2
  for(k in 1:length(q.noun.u)){
    mnoun<-q.noun.u[k]
   d3.sel<-eval.set$q.noun%in%mnoun
   sum(d3.sel)
   #7*15
   eval.set$max.obs[d3.sel][which.max(eval.set$freq[d3.sel])]<-T
   eval.set$max.score[d3.sel][which.max(eval.set$score[d3.sel])]<-T
   
}
  #eval.set$q.noun[d3.sel]
  eval.set[eval.set$max.score==T,]
  ### wks.
  ### > here factor modeling, category u with less recognitions has to be factored
  
  returnlist<-list(freq.df=q.list,qset=temp.set,eval.set=eval.set)
  return(returnlist)
  return(temp.set)
}
#test:
b<-1:5
a<-1:2
c<-a*a
d<-b*b


for(k in a){
  for(d in b){
    print((k*k)*(d*d))
  }
}
tempset<-getrecords(nouns.df.known,nouns.df.unknown,c("noun","coll","cat"))
evalset<-tempset$eval.set
tempfun2<-function(){
  
eval.set$a.noun[d3.sel]
d1u<-nouns.df.known$coll[nouns.df.known$noun=="aa2word"]
d2u<-nouns.df.unknown$coll[nouns.df.unknown$noun=="eee3word"]
d1u==d2u
c1<-compare.linkage(matrix(d1u),matrix(d2u))
c1$frequencies
c1$frequencies/(length(d1u)+length(d2u))
tempset<-getrecords(nouns.df.known,nouns.df.unknown,c("noun","coll","cat"))
tempeval<-getrecords(nouns.df.known,nouns.df.unknown,c("noun","coll","cat"))
#temp.df<-data.frame(tempeval$freq.df$e1word)


#nouns.n
#co.list<-list()
library(RecordLinkage)
benchmark(wa1,wa1)
levenshteinSim(wa1,we1)
levenshteinSim(we1,wa1[1])
levenshteinSim(wa1,wa1[1])
benchmark(levenshteinDist(wa1[1],wa1[3]))
benchmark(levenshteinSim(wa1[1],we1[4]))
levenshteinSim("dreimal schwarzer kater","kater")
levenshteinSim("dreimal schwarzer kater","kater dreimal schwarzer")
sum(levenshteinSim(c("dreimal", "schwarzer", "kater"),c("dreimal", "kater")))
sum(jarowinkler(c("dreimal schwarzer kater"),c("kater dreimal schwarzer")))
sum(jarowinkler(c("dreimal", "schwarzer","kater"),c(" dreimal"," kater")))
m1<-matrix(wa1)
m2<-matrix(we1)
m1<-matrix(c("dreimal", "schwarzer", "kater","place"))
m2<-matrix(c("dreimal", "place","kater","kater","dreimal","kater","kater"))
c1<-compare.linkage(m1,m2)
c1$pairs
c1$frequencies
m<-m2%in%m1
sum(m)
sum(m)/(length(m1)+length(m2))
#getTable(c1)
getPairs(c1)
d1<-data.frame(m=m1,cat=m1)
d2<-data.frame(m=m2,cat=m2)
d1
rpairs<-RLBigDataDedup(   rbind(d1,d2))
rpairs
rpairs
rpairs<-epiWeights(rpairs)
getPairs(rpairs)

}