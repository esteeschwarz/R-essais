#20231216(08.09)
#stefanowitsch.casestudy-2.badsmelling adjectives
#peterson-traba(2021).getcategories
###################################
###################################
# this script defines the categories of nouns according to below cat.array of (9) fixed noun categories.
# method:
### 1. get the categories which where user defined in a table
d10.stef<-read.csv("~/boxHKW/21S/DH/local/SPUND/corpuslx/stefanowitsch/casestudy2.mod.csv")
sum(d10.stef$Category!="")+sum(d10.stef$category.modified!="")
61
### 2. declare function to request sketchengine wordsketch (get collocations to a noun) 
##################
#WSKETCH, collocations, R-translation:
d<-read.csv("~/boxHKW/21S/DH/local/R/cred_gener.csv")
#12367.sketchenginge API request
library(httr)
library(jsonlite)
library(purrr)
USERNAME = d$bn[d$q=="sketch"]
API_KEY = d$key[d$q=="sketch"]
BASE_URL = 'https://api.sketchengine.eu/bonito/run.cgi'
#item<-"buzzard"
#for (item in c('make', 'ensure')){
get.ske<-function(item,run){
  cat("run",run,"\n")
  d <- GET(paste0(BASE_URL, '/wsketch'), authenticate(USERNAME, API_KEY),query=list(
  lemma=item,
  # lpos='-v',
  corpname='preloaded/bnc2',
  format= 'json')
)%>%content("text")%>%fromJSON()
}
# beware of FUP, see https://www.sketchengine.eu/service-level-agreement/
#########################################################################

### 3. create list of all collocations to nouns with known (predefined) categories 

# which(names(d$Gramrels)=='word')
# k<-1
# x<-d
#########################################
get.words<-function(x)unlist(x[['word']])
#l<-length(d.c.u$Gramrels$Words)
#word.no<-d.c.u$Gramrels$Words[[l]]['word']
#word.no$word
#d1u
#d1u%in%word.no$word
# d.c<-get.ske("eagle")
# d1u<-unlist(lapply(d$Gramrels$Words, get.words))
# d2u<-unlist(lapply(d.c$Gramrels$Words, get.words))
# d1u<-unique(d1u)
# d2u<-unique(d2u)
# ifelse(length(d1u)>length(d2u),m<-d1u%in%d2u,m<-d2u%in%d1u)
# sum(m)
#d.c.ar<-array()
# for(k in 1:length(d10.stef$category.modified)){
#   d.c<-get.ske(d10.stef$Noun[k])
# #  d1u<-unlist(lapply(d$Gramrels$Words, get.words))
#   d2u<-unlist(lapply(d.c$Gramrels$Words, get.words))
#  # d1u<-unique(d1u)
#   d2u<-unique(d2u)
#   ifelse(length(d1u)>length(d2u),m<-d1u%in%d2u,m<-d2u%in%d1u)
#   d.c.ar[k]<-sum(m)
#   for (i in 5000:1){
#     cat(k,i,sum(m),"\n")
#   }
# }
# d10.stef$category.modified[which.max(d.c.ar)]
# #d10.stef$category.modified[which.max(d.c.ar)]<-NA
# df<-d10.stef
# #df$Noun[order(d.c.ar) ]
# #df$category.modified[order(d.c.ar) ]
# 
# #which.max(d.c.ar)]
# #d.c.ar
# #df$category.modified[which(d.c.ar>=10)]
# d.c.f<-factor(df$category.modified[which(d.c.ar>=10)],exclude = c("",NA),ordered = T)
# #rank(d.c.f,ties.method = "max")
# #sum(d.c.f=="AN")
# #count
# plot(d.c.f)
# #levels(ordered(d.c.f))
# #levels(d.c.f))
# d.c.t<-table(d.c.f)
# d.c.t
# d.c.t[which.max(d.c.t)]
###wks.
################
# now empty cats
d10.stef$cat.ai<-NA # declare empty column for later input categories
#r<-1
#k<-1
d.c.ar<-array()
d.c.k.ar<-list()
m.k.pet<-d10.stef$Category!="" #original stefanowitsch (petterson) categories
m.k.mod<-d10.stef$category.modified!="" #first training manually edited cats
sum(m.k.pet)
sum(m.k.mod)
m.k.w.1<-which(m.k.pet)
m.k.w.2<-which(m.k.mod)
m.k.c<-c(m.k.w.1,m.k.w.2) # join array of all predefined categories in df
#join.freqs(m.k,m.k.pet)
length(m.k.c)
m.k<-m.k.c
length(d10.stef$Noun[m.k])==length(unique(d10.stef$Noun[m.k])) # seem to be doubled nouns in df
k<-1
### 3.2 make a sketchengine request for each noun and get the collocates
for(k in 1:length(d10.stef$Noun[m.k])){
  d.c.k<-get.ske(d10.stef$Noun[m.k][k],k)
  d1u<-unlist(lapply(d.c.k$Gramrels$Words, get.words))
  d1uu<-unique(d1u)
  l<-length(d.c.k$Gramrels$Words)
  word.no<-d.c.k$Gramrels$Words[[l]]['word']
  m.pos<-d1uu%in%word.no$word
  sum(m.pos)
  d1uu<-d1uu[!m.pos] #discards postag cats from array
  d.c.k.ar[[d10.stef$Noun[m.k][k]]]<-d1uu
  d1uu
}
d.c.k.ar$rose
d.c.k.df<-cbind(unlist(d.c.k.ar))
nouns.nm<-stri_split_regex(rownames(d.c.k.df),"[a-z]",simplify = T)
nouns.nm.df<-as.data.frame(as.double(nouns.nm))
m<-nouns.nm==""
nouns.nm[m]<-NA
mode(nouns.nm)<-"double"
nouns.nm.sum<-rowSums(nouns.nm,na.rm = T)
nouns.nm.df<-as.double(nouns.nm)
nouns.nm.sum
nouns<-stri_split_regex(rownames(d.c.k.df),"[0-9]",simplify = T)
d.c.k.df.c<-data.frame(lfd=nouns.nm.sum,noun=nouns[,1],collocations=d.c.k.df[,1])
nouns.cats.known<-d.c.k.df.c
k<-1
sum(nouns.cats.known$noun=="rose")
#nouns.cats.known$category[nouns.cats.known$noun=="rose"]
nouns.cats.known$category<-NA
nouns.cats.known$category.pet<-NA
for(k in 1:length(d10.stef$Noun[m.k.pet])){
  m.3<-d10.stef$Noun[m.k.pet][k]==nouns.cats.known$noun
 sum(m.3)
  nouns.cats.known$category[m.3]<-d10.stef$Category[m.k.pet][k]
#  nouns.cats.known$category[m.k.mod]<-d10.stef$category.modified[m.k.mod][k]
}
for(k in 1:length(d10.stef$Noun[m.k.mod])){
  m.3<-d10.stef$Noun[m.k.mod][k]==nouns.cats.known$noun
  sum(m.3)
  nouns.cats.known$category[m.3]<-d10.stef$category.modified[m.k.mod][k]
}
nouns.cats.known$category[nouns.cats.known$noun=="work"]
write.csv(nouns.cats.known,"fragrance_known-cats_coll.cpt.csv",row.names = F)
getwd()
#k<-8
#d10.stef$cat.ai<-NA
####################
### 4. get collocates for nouns of unknown category and seek most frequent matches between collocates of known and unknown nouns
### define the category (unknown noun) to that of the category with the most agreement in collocates
for(k in 1:length(d10.stef$Noun)){
  d.c.u<-1:4 # empty response simulation at the begin of the loop 
  noun<-d10.stef$Noun[k]
  d.c.u<-get.ske(noun,k)
  ### here test
  if (length(d.c.u)>4){
  d2u<-unlist(lapply(d.c.u$Gramrels$Words, get.words))
  d2u<-unique(d2u)
  l<-length(d.c.u$Gramrels$Words)
  word.no<-d.c.u$Gramrels$Words[[l]]['word']
  word.no$word
  m.pos<-d2u%in%word.no$word
  m.pos
  d2u<-d2u[!m.pos] #discards postag cats from array
  d2u
  m.both<-noun==nouns.cats.known$collocations
  sum(m.both)
  m.coll<-nouns.cats.known$collocations%in%d2u
  sum(m.coll)
  m.coll
  nouns.cats.known$collocations[m.both]
  factor(nouns.cats.known$collocations[m.both])
  nouns.cats.known$noun[m.both]
  nouns.cats.known$collocations[m.coll]
  nouns.cats.known$noun[m.coll]
  nouns.cats.known$category[m.both]
  nouns.cats.known$category[m.coll]
  u<-1
  typeof(nouns.cats.known$lfd)
  for (u in 1:length(nouns.cats.known$unique)){
    nouns.cats.known$unique[u]<-max(nouns.cats.known$lfd[nouns.cats.known$noun==nouns.cats.known$noun[u]])
  }
  nouns.cats.known$unique[nouns.cats.known$noun=="alley"]
  if(length(nouns.cats.known$category[m.both])>1){
    
    d.c.f.1<-factor(nouns.cats.known$category[m.both])
    plot(d.c.f.1)
    d.c.t.1<-table(d.c.f.1)
    d.c.t.1
    d.c.t.both<-d.c.t.1
    d.c.t.1[which.max(d.c.t.1)]  
  }
  if (sum(m.coll>0)){
  nouns.cats.known$collocations[m.coll]
  nouns.cats.known$noun[m.coll]
  nouns.cats.known$category[m.coll]
  #m.fac<-factor(nouns.cats.known$category[m.coll])
  #length(d2u)
  #sum(nouns.cats.known$noun[m.coll]=="rose")
  #nouns.cats.known$collocations[nouns.cats.known$noun[m.coll]=="rose"]
  #sum(m.coll)
  nouns.cats.known$noun[m.coll]
  #nouns.cats.known$category[m.both]
  d.c.f<-factor(nouns.cats.known$category[m.coll])
  d.c.f.n<-factor(nouns.cats.known$noun[m.coll])
  d.c.f.f<-factor(nouns.cats.known$unique[m.coll])
  length(d.c.f)
  length(d.c.f.n)
  #sum(d.c.f/d.c.f.n,na.rm = T)
  plot(d.c.f.n)
  d.c.t<-table(d.c.f)
  d.c.t
  noun
  d.c.t.n<-table(d.c.f.n)
  d.c.f.t<-table(d.c.f)
  d.c.f
  d.c.f.t
  d.c.f.n
#nouns.cats.known.p<-as.double(d.c.f.f)/nouns.cats.known$unique
#nouns.cats.known.p.2<-nouns.cats.known$unique/as.double(d.c.f.f)

 #  typeof(unique(nouns.cats.known.p))
#   m<-is.infinite(nouns.cats.known.p)
 #  nouns.cats.known.p[m]<-0
#   sum(nouns.cats.known.p,na.rm = T)
 #  nouns.cats.known$p<-nouns.cats.known.p
  # d.c.t.p<-nouns.cats.known$p[m.coll][nouns.cats.known$category=="AN"]
   #d.c.t.cat<-nouns.cats.known$category[m.coll]
   #table(d.c.t.cat)
  # factor(d.c.t.cat)
  # sum(d.c.t.p)
  # length(d.c.f.t)
   d.c.t
  #sum(d.c.t[1]/sum(nouns.cats.known$unique[m.coll[nouns.cats.known$category==names(d.c.t[1])]],na.rm = T))
  #sum(d.c.t[2]/sum(nouns.cats.known$unique[m.coll[nouns.cats.known$category==names(d.c.t[2])]],na.rm = T))
  #sum(is.na(nouns.cats.known$unique))
  #m<-nouns.cats.known$category=="AC"
  #sum(!is.na(nouns.cats.known$unique[m.coll[with(nouns.cats.known$category=="AC")]]))
  #d.c.t.n
  #length(d.c.t.n)
#  d.c.t.p<-nouns.cats.known$unique[nouns.cats.known$noun%in%names(d.c.t.n)]
  #length(unique(d.c.t.p))
  
  #length(d.c.t.n)
  #nouns.cats.known$unique
  #d10.stef$Noun==d.c.t.n[1]
  sum(d.c.t)
  sum(d.c.t.n)
  d.c.t.coll<-d.c.t
  d.c.t.ass<-c(d.c.t.coll,d.c.t.both)
  print(d.c.t.ass[which.max(d.c.t.ass)])
  d10.stef$cat.ai[k]<-names(d.c.t.ass[which.max(d.c.t.ass)])
  } #end if 1
  } #end if 2
  for (i in 5000:1){
    cat(k,i,sum(m),"\n")
  }
}
write.csv(d10.stef,"fragrance2_ai-cats.csv")
############################################
### evaluate definition:
d10.gold<-read_csv("fragrance2_ai-cats.gold.csv") # manually defined gold standard of cats
c.ai<-d10.gold$cat.ai # cats defined with script
c.gold<-d10.gold$cat.gold # cats corrected manually
p1<-c.ai==c.gold
sum(p1,na.rm = T)/length(p1)
78% # trefferquote to goldstandard
##################################
# d10.stef$category.modified[which.max(d.c.ar)]
# #d10.stef$category.modified[which.max(d.c.ar)]<-NA
# df<-d10.stef
# #df$Noun[order(d.c.ar) ]
# #df$category.modified[order(d.c.ar) ]
# 
# #which.max(d.c.ar)]
# #d.c.ar
# #df$category.modified[which(d.c.ar>=10)]
# d.c.f<-factor(df$category.modified[which(d.c.ar>=10)],exclude = c("",NA),ordered = T)
# #rank(d.c.f,ties.method = "max")
# #sum(d.c.f=="AN")
# #count
# #plot(d.c.f)
# #levels(ordered(d.c.f))
# #levels(d.c.f))
# d.c.t<-table(d.c.f)
# d.c.t
# d.c.t[which.max(d.c.t)]
# d10.stef$cat.ai[m[k]]<-names(d.c.t[which.max(d.c.t)])
#   
# }


