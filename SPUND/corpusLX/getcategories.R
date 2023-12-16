#20231216(08.09)
#stefanowitsch.casestudy-2
#peterson-traba(2021).getcategories
###################################
cat.list<-list()
cat.array<-c("FOOD & DRINK","PLANTS & FLOWERS","EARTH","BODY","MATTER","SENSATION","AESTHETICS","CLEANING","TEXTILE & CLOTHING")
cat.list<-list(cat.array)
cat.list[[cat.array[1]]]<-c("apple", "beverage", "bread", "chicken", "coffee", "cup", "drink", "food", "fruit", "liquid", "meal", "omelet", "rice", "spice", "tea", "wine")
cat.list[[cat.array[2]]]<-c("bloom", "blossom", "bower", "flower", "garden", "geranium", "grass", "herb", "lip*", "leaf", "petal", "pine", "rose", "shrub", "vine", "violet")
cat.list[[cat.array[3]]]<-c("breeze", "brook", "dew", "flood", "gale", "grove", "hill", "sea", "vale", "valley", "wind")
cat.list[[cat.array[4]]]<-c("arm", "breath*", "cheek", "face", "flesh", "hair", "hand", "head", "limb", "lip*", "lock", "mouth", "shoulder", "skin", "wrist")
cat.list[[cat.array[5]]]<-c("air", "atmosphere", "candle", "cloud*", "dust", "fume", "gas", "oil*", "night", "smoke", "steam", "vapor")
cat.list[[cat.array[6]]]<-c("aroma", "breath*", "flavor", "incense", "scent", "smell", "odor", "taste")
cat.list[[cat.array[7]]]<-c("cologne", "cosmetics", "cream", "oil*", "ointment", "powder", "talcum", "wax")
cat.list[[cat.array[8]]]<-c("deodorant", "dish-water", "disinfectant", "napkin", "soap", "soap-powder", "sponge", "spray", "suds", "tissue", "wash-ball")
cat.list[[cat.array[9]]]<-c("blanket", "cambric", "cloth", "dress", "flannel", "garment", "glove", "lace", "linen", "pillow", "robe", "sheet", "shirt", "silk")



#WSKETCH, collocations, R-translation:
d<-read.csv("~/boxHKW/21S/DH/local/R/cred_gener.csv")
#12367.sketchenginge API request
library(httr)
library(jsonlite)
library(purrr)
USERNAME = d$bn[d$q=="sketch"]
API_KEY = d$key[d$q=="sketch"]
BASE_URL = 'https://api.sketchengine.eu/bonito/run.cgi'
item<-"buzzard"
#for (item in c('make', 'ensure')){
get.ske<-function(item){
d <- GET(paste0(BASE_URL, '/wsketch'), authenticate(USERNAME, API_KEY),query=list(
  lemma=item,
  # lpos='-v',
  corpname='preloaded/bnc2',
  format= 'json')
)%>%content("text")%>%fromJSON()
}
# cat( 'Word sketch data for', item,"\n")
# g<-1
# for (g in 1:length(d[['Gramrels']][['score']])){
#   print(d[['Gramrels']][['name']][g])
# 
#   for (i in 1:length(d[['Gramrels']][['Words']][[g]])){
#   print(d[['Gramrels']][['Words']][[g]]['word'])
# }
# }
# 
# library(jsonlite)
# jsonlite::
# i<-1
# beware of FUP, see https://www.sketchengine.eu/service-level-agreement/

#d.j<-fromJSON(d)

which(names(d$Gramrels)=='word')
k<-1
x<-d
get.words<-function(x)unlist(x[['word']])
#d$Gramrels$Words[[12]]['word']
#d.1.u<-unlist('word',get.words(d))
#d.2.u<-unlist(d.c$Gramrels$Words)
#d.2.u<-
#m<-grep("bird",d$Gramrels$Words)
d.c<-get.ske("eagle")
d1u<-unlist(lapply(d$Gramrels$Words, get.words))
d2u<-unlist(lapply(d.c$Gramrels$Words, get.words))
d1u<-unique(d1u)
d2u<-unique(d2u)
ifelse(length(d1u)>length(d2u),m<-d1u%in%d2u,m<-d2u%in%d1u)
sum(m)
d.c.ar<-array()
for(k in 1:length(d10.stef$category.modified)){
  d.c<-get.ske(d10.stef$Noun[k])
#  d1u<-unlist(lapply(d$Gramrels$Words, get.words))
  d2u<-unlist(lapply(d.c$Gramrels$Words, get.words))
 # d1u<-unique(d1u)
  d2u<-unique(d2u)
  ifelse(length(d1u)>length(d2u),m<-d1u%in%d2u,m<-d2u%in%d1u)
  d.c.ar[k]<-sum(m)
  for (i in 5000:1){
    cat(k,i,sum(m),"\n")
  }
}
d10.stef$category.modified[which.max(d.c.ar)]
#d10.stef$category.modified[which.max(d.c.ar)]<-NA
df<-d10.stef
#df$Noun[order(d.c.ar) ]
#df$category.modified[order(d.c.ar) ]

#which.max(d.c.ar)]
#d.c.ar
#df$category.modified[which(d.c.ar>=10)]
d.c.f<-factor(df$category.modified[which(d.c.ar>=10)],exclude = c("",NA),ordered = T)
#rank(d.c.f,ties.method = "max")
#sum(d.c.f=="AN")
#count
plot(d.c.f)
#levels(ordered(d.c.f))
#levels(d.c.f))
d.c.t<-table(d.c.f)
d.c.t
d.c.t[which.max(d.c.t)]
###wks.
################
# now empty cats
d10.stef$cat.ai<-NA
#r<-1
k<-1
d.c.ar<-array()
d.c.k.ar<-list()
#m.k<-d10.stef$Category!="" #original stefanowitsch
m.k<-d10.stef$category.modified!="" #first training manually modified cats
length(d10.stef$Noun[m.k])==length(unique(d10.stef$Noun[m.k]))
for(k in 1:length(d10.stef$Noun[m.k])){
  d.c.k<-get.ske(d10.stef$Noun[m.k][k])
  d1u<-unlist(lapply(d.c.k$Gramrels$Words, get.words))
  d1uu<-unique(d1u)
  d.c.k.ar[[d10.stef$Noun[m.k][k]]]<-d1uu
  
}
#d.c.k.df<-data.frame(d.c.k.ar)
d.c.k.df<-cbind(unlist(d.c.k.ar))
nouns.nm<-stri_split_regex(rownames(d.c.k.df),"[a-z]",simplify = T)
nouns.nm.df<-as.data.frame(as.double(nouns.nm))
m<-nouns.nm==""
nouns.nm[m]<-NA
mode(nouns.nm)<-"double"
nouns.nm.sum<-rowSums(nouns.nm,na.rm = T)
nouns.nm.df<-as.double(nouns.nm)
nouns.nm.sum
#nm.sum<-function(x)sum(as.double(x),na.rm = T)
#library(abind)
#sum(as.double(nouns.nm[2,]),na.rm = T)
#nouns.nm.sum<-lapply(nouns.nm[,1:length(nouns.nm[2,])],nm.sum)
nouns<-stri_split_regex(rownames(d.c.k.df),"[0-9]",simplify = T)
d.c.k.df.c<-data.frame(lfd=nouns.nm.sum,noun=nouns[,1],collocations=d.c.k.df[,1])
nouns.cats.known<-d.c.k.df.c
k<-1
nouns.cats.known$category<-NA
for(k in 1:length(d10.stef$Noun[m.k])){
  m.3<-d10.stef$Noun[m.k][k]==nouns.cats.known$noun
sum(m.3)
#  nouns.cats.known$category[m.3]<-d10.stef$Category[m.k][k]
  nouns.cats.known$category[m.3]<-d10.stef$category.modified[m.k][k]
}
#colnames(d.c.k.df)<-c()
write.csv(nouns.cats.known,"fragrance_known-cats_coll.mod.csv",row.names = F)
getwd()
#m.u<-d10.stef$Category==""
#d.c.kn<-get.ske(d10.stef$Noun[m][k])
k<-44

for(k in 1:length(d10.stef$Noun)){
  d.c.u<-1:4
  noun<-d10.stef$Noun[k]
  #if(d10.stef$Category[k]!="")
  if(d10.stef$category.modified[k]!="")
      d.c.u<-get.ske(noun)
  if (length(d.c.u)>4){
  d2u<-unlist(lapply(d.c.u$Gramrels$Words, get.words))
  # d1u<-unique(d1u)
  d2u<-unique(d2u)
  m.both<-noun==nouns.cats.known$collocations
  sum(m.both)
 # m.coll<-nouns.cats.known$collocations%in%d2u
  m.coll<-d2u==nouns.cats.known$collocations # where fit the collocations of query into collocations of known categories
  sum(m.coll)
  if (sum(m.coll>0)){
  nouns.cats.known$collocations[m.coll]
  #m.fac<-factor(nouns.cats.known$category[m.coll])
  #length(d2u)
  #sum(nouns.cats.known$noun[m.coll]=="rose")
  #nouns.cats.known$collocations[nouns.cats.known$noun[m.coll]=="rose"]
  #sum(m.coll)
  nouns.cats.known$noun[m.coll]
  #nouns.cats.known$category[m.both]
  d.c.f<-factor(nouns.cats.known$category[m.coll])
  #plot(d.c.f)
  d.c.t<-table(d.c.f)
  d.c.t
  d.c.t[which.max(d.c.t)]
  d10.stef$cat.ai[k]<-names(d.c.t[which.max(d.c.t)])
  } #end if
  } #end if
  #  d1u<-unlist(lapply(d$Gramrels$Words, get.words))
  #d2u<-unlist(lapply(d.c.u$Gramrels$Words, get.words))
  # d1u<-unique(d1u)
  #d2u<-unique(d2u)
  #m<-d2u%in%d1u
  #ifelse(length(d1u)>length(d2u),m<-d1u%in%d2u,m<-d2u%in%d1u)
  #d.c.ar[k]<-sum(m)
  for (i in 5000:1){
    cat(k,i,sum(m),"\n")
  }
}
d10.stef$category.modified[which.max(d.c.ar)]
#d10.stef$category.modified[which.max(d.c.ar)]<-NA
df<-d10.stef
#df$Noun[order(d.c.ar) ]
#df$category.modified[order(d.c.ar) ]

#which.max(d.c.ar)]
#d.c.ar
#df$category.modified[which(d.c.ar>=10)]
d.c.f<-factor(df$category.modified[which(d.c.ar>=10)],exclude = c("",NA),ordered = T)
#rank(d.c.f,ties.method = "max")
#sum(d.c.f=="AN")
#count
#plot(d.c.f)
#levels(ordered(d.c.f))
#levels(d.c.f))
d.c.t<-table(d.c.f)
d.c.t
d.c.t[which.max(d.c.t)]
d10.stef$cat.ai[m[k]]<-names(d.c.t[which.max(d.c.t)])
  
}


