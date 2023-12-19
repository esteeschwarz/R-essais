#20231216(08.09)
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
#sum(d10.stef$Category!="")+sum(d10.stef$category.modified!="")
61
### 2. declare function to request sketchengine wordsketch (get collocations to a noun) 
##################
#WSKETCH, collocations, R-translation:
#mini:
#d<-read.csv("/volumes/ext/boxHKW/21S/DH/local/R/cred_gener.csv")
d<-read.csv("~/boxHKW/21S/DH/local/R/cred_gener.csv")
#12367.sketchenginge API request
library(httr)
library(jsonlite)
library(purrr)
library(stringi)
library(readr)
USERNAME = d$bn[d$q=="sketch"]
API_KEY = d$key[d$q=="sketch"]
BASE_URL = 'https://api.sketchengine.eu/bonito/run.cgi'
#item<-"buzzard"
#for (item in c('make', 'ensure')){
get.ske<-function(item,run){
  cat("run",run,"\n")
  d <- GET(paste0(BASE_URL, '/wsketch'), authenticate(USERNAME, API_KEY),query=list(
  lemma=item,
  lpos='-n',
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
###wks.
################
# now empty cats
d10.stef$cat.ai<-NA # declare empty column for later input categories
#r<-1
#k<-1
d.c.ar<-array()
d.c.k.ar<-list()
m.k.pet<-d10.stef$Category!="" #original stefanowitsch (petterson) categories
sum(m.k.pet)
m.k.mod<-d10.stef$category.modified!="" #first training manually edited cats
### obsolete, no predefined cats in df
# m.ai<-grep("cat.ai",colnames(d10.stef))
# if(length(m.ai)>0)
#   m.k.ai<-d10.stef$cat.ai!="" #first training manually edited cats
# sum(m.k.pet)
# sum(m.k.mod)
 m.k.w.1<-which(m.k.pet)
 m.k.w.2<-which(m.k.mod)
m.k.c<-c(m.k.w.1,m.k.w.2) # join array (rows in df) of all predefined categories in df
#join.freqs(m.k,m.k.pet)
length(m.k.c)
m.k<-m.k.c
#length(d10.stef$Noun[m.k])==length(unique(d10.stef$Noun[m.k])) # seem to be doubled nouns in df
#k<-1
#m.k<-92
m.cpt<-1:length(d10.stef$Category)

# new for known cats of training:
m.not<-d10.stef$Noun%in%nouns.cats.known$noun
sum(!m.not)
m.not<-m.cpt%in%m.n
which(!m.not)
#d.c.k.ar<-getknowncats(m.k)
#d.c.k.ar<-getknowncats(1:2)
#d.c.k.ar$rose


### 3.2 make a sketchengine request for each noun (category defined) and get the collocates
getknowncats<-function(m.k){
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
  return(d.c.k.ar)
  
  }

nouns.cats.known.fun<-function(d.c.k.ar){
d.c.k.df<-cbind(unlist(d.c.k.ar))
nouns.nm<-stri_split_regex(rownames(d.c.k.df),"[a-z]",simplify = T)
nouns.nm.df<-as.data.frame(as.double(nouns.nm))
m<-nouns.nm==""
sum(m)
nouns.nm[m]<-NA
mode(nouns.nm)<-"double"
nouns.nm.sum<-rowSums(nouns.nm,na.rm = T)
nouns.nm.df<-as.double(nouns.nm)
nouns.nm.sum
nouns<-stri_split_regex(rownames(d.c.k.df),"[0-9]",simplify = T)
d.c.k.df.c<-data.frame(lfd=nouns.nm.sum,noun=nouns[,1],collocations=d.c.k.df[,1])
nouns.cats.known<-d.c.k.df.c
#k<-1
sum(nouns.cats.known$noun=="rose")
#nouns.cats.known$category[nouns.cats.known$noun=="rose"]
nouns.cats.known$category<-NA
#nouns.cats.known$category.pet<-NA
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
#u<-58
nouns.cats.known$unique<-NA
for (u in 1:length(nouns.cats.known$unique)){
  nouns.cats.known$unique[u]<-max(nouns.cats.known$lfd[nouns.cats.known$noun==nouns.cats.known$noun[u]])
}
#m.k.pet
# if(length(m.ai)>0){
# for(k in 1:length(d10.stef$Noun[m.k.ai])){
#   m.3<-d10.stef$Noun[m.k.mod][k]==nouns.cats.known$noun
#   sum(m.3)
#   nouns.cats.known$category[m.3]<-d10.stef$cat.ai[m.k.mod][k]
# }
# }
return(nouns.cats.known)
}
#d.c.k.ar<-getcat(14,nouns.cats.old = nouns.cats.known)
######################################################
#noun
#catarray<-cat.test[[noun]] # noun defined in calling function
nouns.cats.known.ai<-function(catarray,m.k.ai){
  #m.k.ai<-d10.stef$cat.ai!=""
  #sum(m.k.ai,na.rm = T)
  nouns.cats.new<-data.frame(lfd=NA,noun=NA,collocations=NA,category="n.a.",unique=NA)
  catarray$coll[['empty']]<-"#empty#"
  d.c.k.df<-cbind(unlist(catarray$coll))
  l1<-length(catarray$coll)
  #length(listreturn$coll)
  if(length(catarray$coll[[l1]])>0){
  nouns.nm<-stri_split_regex(rownames(d.c.k.df),"[a-z]",simplify = T)
  nouns.nm.df<-as.data.frame(as.double(nouns.nm))
  m<-nouns.nm==""
  sum(m)
  nouns.nm[m]<-NA
  mode(nouns.nm)<-"double"
  nouns.nm.sum<-rowSums(nouns.nm,na.rm = T)
  nouns.nm.df<-as.double(nouns.nm)
  nouns.nm.sum
  nouns<-stri_split_regex(rownames(d.c.k.df),"[0-9]",simplify = T)
  d.c.k.df.c<-data.frame(lfd=nouns.nm.sum,noun=nouns[,1],collocations=d.c.k.df[,1])
  nouns.cats.new<-d.c.k.df.c
  nouns.cats.new$category<-"n.a."
  nouns.cats.new$unique<-NA
  for (u in 1:length(nouns.cats.new$unique)){
    nouns.cats.new$unique[u]<-max(nouns.cats.new$lfd[nouns.cats.new$noun==nouns.cats.new$noun[u]])
  }
  #k
  #if(length(m.k.ai)>0){
    # for(k in 1:length(d10.stef$Noun[m.k.ai])){
    #   m.3<-d10.stef$Noun[m.k.ai][k]==nouns.cats.known$noun
    #   sum(m.3)
      #nouns.cats.known$category[m.3]<-d10.stef$cat.ai[m.k.ai][k]
      nouns.cats.new$category<-names(which.max(d.c.k.ar$cat))
  #  }
  #}
  }
  
  return(nouns.cats.new)
}
######################### testing:
#rm(nouns.cats.known.temp)
#m.k
#nouns.cats.temp<-nouns.cats.known.ai(getcat(12,nouns.cats.old = nouns.cats.known),m.k.ai) #cat.test[[noun]][['coll']]
#nouns.cats.known<-nouns.cats.known.fun(getknowncats(m.k))
####################
### 4. get collocates for nouns of unknown category and seek most frequent matches between collocates of known and unknown nouns
### define the category (unknown noun) to that of the category with the most agreement in collocates
#for(k in 1:length(d10.stef$Noun)){
#k<-16
### 14514.it is to be taken total of collocations/token into account, else all will be classified AC as with the most
### collocations in that cat and therefore most matches in general

#nouns.cats.old
run<-233
d10.stef$Noun[233]
#noun.q<-"canal"
  getcat<-function(run,noun.q,nouns.cats.old){
nouns.cats.known<-nouns.cats.old
      k<-run
  d.c.u<-1:4 # empty response simulation at the begin of the loop 
  ifelse(run>0,noun.q<-d10.stef$Noun[k],noun.q<-noun.q)
  noun<-noun.q
  print(noun)
  d.c.u<-get.ske(noun,k)
  ### here test
  ### declare var if 
  d.c.t.ass<-0
  cat.df<-data.frame(cat="n.a.",score=0,row.names = "#empty#")

  #####################
  ### start loop
  ### if response contains collocates:
  if (length(d.c.u)>4){
  d2u<-unlist(lapply(d.c.u$Gramrels$Words, get.words))
  d2u<-unique(d2u)
  l<-length(d.c.u$Gramrels$Words)
  ########
  if(l>1){
  word.no<-d.c.u$Gramrels$Words[[l]]['word']
  word.no$word
  m.pos<-d2u%in%word.no$word
  m.pos
  d2u<-d2u[!m.pos]
  } #discards postag cats from array
  d2u
  d.c.k.ar<-list(empty=NA)
  d.c.k.ar[[noun.q]]<-d2u
  ###### collocations list:
  #########################
  m.both<-noun==nouns.cats.known$collocations
  nouns.cats.known$noun[m.both]
  sum(m.both)
  d.c.t.both<-0
  m.coll<-nouns.cats.known$collocations%in%d2u
  #m.coll<-d2u%in%nouns.cats.known$collocations
  sum(m.coll)
  m.coll
  #nouns.cats.known$collocations[m.both]
  #factor(nouns.cats.known$collocations[m.both])
  nouns.cats.known$noun[m.coll]
  #nouns.cats.known$collocations[m.coll]
  #nouns.cats.known$noun[m.coll]
  #nouns.cats.known$category[m.both]
  #nouns.cats.known$category[m.coll]
#  u<-1
  #typeof(nouns.cats.known$lfd)
  #nouns.cats.known$unique[nouns.cats.known$noun=="alley"]
 # if(length(nouns.cats.known$category[m.both])>1){
  if(length(nouns.cats.known$category[m.both])>0){
      
    d.c.f.1<-factor(nouns.cats.known$category[m.both])
d.c.f.1
      #  plot(d.c.f.1)
    d.c.t.1<-table(d.c.f.1)
    d.c.t.1
    d.c.t.both<-d.c.t.1
    d.c.t.1[which.max(d.c.t.1)]  
  }
  if (sum(m.coll>0)){
  #nouns.cats.known$collocations[m.coll]
  #nouns.cats.known$noun[m.coll]
  #nouns.cats.known$category[m.coll]
  #nouns.cats.known$noun[m.coll]
  #nouns.cats.known$category[m.both]
#  d.c.t
  #df.t<-table(df.dcf)
  #which.max(df.t)
  # df.t[,1,]
  # df.t[,,43] # max 126 bei unique 43, cat AC, noun: day
  # df.t[,,1] # 1, cat MA, noun: vapor
  # df.t[,,2] # 1, cat B&UE, noun: guardhouse
  # df.t[,'MA',1]
  # colnames(df.t[,,1])[which.max(df.t[,,1])]
  # which.max(df.t[,,1]) #373 in the matrix, which cat?
  # df.dcf$d.c.f[373]
  # df.t[which.max(df.t)]
  # names(max(df.t[,,1:2]))
  # noun
  #plot(df.t)
  # this the category totals of matches, it has to be cleaned by totals of noun collocates
 # d.c.f
  d.c.f<-factor(nouns.cats.known$category[m.coll])
  d.c.f.n<-factor(nouns.cats.known$noun[m.coll])
  d.c.f.f<-factor(nouns.cats.known$unique[m.coll])
  
  df.dcf<-data.frame(cat=as.character(d.c.f))
  df.dcf$noun<-as.character(d.c.f.n)
  df.dcf$unique<-as.double(as.character(d.c.f.f))
  df.dcf$matches<-as.double(d.c.f.f)
  df.dcf$factor<-1/df.dcf$unique
  
  d.c.t<-table(d.c.f)
  d.c.t.f<-table(d.c.f.f)
  #df.dcf<-df.dcf[c(3,1,4)]
  #colnames(df.dcf)[2]<-"cat"
 # df.dcf$score<-1/as.double(df.dcf$unique)
  df.s<-data.frame(cat=as.character(unique(df.dcf$cat)),score=NA,row.names = unique(df.dcf$cat))
  #k<-1
  k
  c<-1
  for(c in 1:length(df.s$cat)){
    ck<-df.dcf$factor[df.dcf$cat==df.s$cat[c]]
    
    df.s$score[c]<-sum(ck,na.rm = T)
  }
  c
  ##############################################
  catfinal.coll<-df.s$cat[which.max(df.s$score)]
  cat.df<-df.s
  df.s
  noun
  catfinal.coll
  #### THIS ####################################
  
  m.coll
  #plot(d.c.f)
  sum(d.c.f.n=="rose") # total of matches
  nouns.cats.known$unique[nouns.cats.known$noun=="rose"] #total of collocations
  # so if rose has 21 matches in 125 collocations of rose in the nouns known set, 
  # the number of matches for that noun between query and nouns.known has to be reduced as /125
  21/125
  
  # model:
  # 10 matches, 100 coll in gold, 20 in q
  # 10 - 100 - 50
  10/120 # 
  10/150 # 
  # 10 - 50 -10
  10/60
  10/30
  
  d.c.f.f.u<-unique(d.c.f.f)
  d.c.f.f.u2<-nouns.cats.known$unique[m.coll]
  #d.c.f/d.c.f.f.u2
  #d.c.f.n<-nouns.cats.known$noun[m.coll]
  #length(d.c.f)
  #noun
  #length(d.c.f.n)
  #sum(d.c.f/d.c.f.n,na.rm = T)
 # plot(d.c.f.n)
  #names(d.c.t)
  #noun
  d.c.t.n<-table(d.c.f.n)
  d.c.f.t<-table(d.c.f)
  d.c.f
  d.c.f.t
  d.c.f.n
  sum(d.c.t)
  sum(d.c.t.n)
  d.c.t.coll<-d.c.t
  #names(d.c.t.coll)<-noun.q
  d.c.t.ass<-c(d.c.t.coll,d.c.t.both)
  l.d.c.t.both<-length(d.c.t.both)
  l.d.c.t.coll<-length(d.c.t.coll)
  l.dif<-(l.d.c.t.coll-l.d.c.t.both)
  l.dif.a<-rep(0,abs(l.dif))
  if(l.dif>0){
    names(l.dif.a)<-rep("sub",l.dif)
    l.dif.a
    d.c.t.both.cor<-c(d.c.t.both,l.dif.a)
    d.c.t.com<-rbind(d.c.t.coll,d.c.t.both.cor)
    d.c.t.sum<-d.c.t.coll+d.c.t.both.cor
    catfinal<-d.c.t.sum
  }
  if(l.dif<=0&sum(m.both)>0){
    d.c.t.both.cor<-c(d.c.t.both,d.c.t.coll)
    catfinal<-d.c.t.both.cor
  }
  #sum(d.c.t.com[,1])
 # print(d.c.t.sum)
  print(noun)
#catfinal<-d.c.t.ass[which.max(d.c.t.ass)]
#d10.stef$cat.ai[k]<-catfinal
  } #end if 1
  } #end if 2
  #return(catfinal)
  listreturn<-list(catmatches=d.c.t.ass,catscore=cat.df,coll=d.c.k.ar)
  #return(d.c.t.ass,d.c.k.ar)
  listreturn
  return(listreturn)
  } # end getcat
#catfinal
cat.test<-list()
cat.test
#k<-7
k<-75
#which(!m.not)
#for(k in 1:length(d10.stef$Noun)){
range<-75:75
#nouns.cats.old<-nouns.cats.known
#rm(nouns.cats.known.temp)
cat.process<-function(range){ #,nouns.cats.old){
  nouns.cats.old<-read.csv("nouns.cats.temp.csv") # will be saved at end of loop under same name after adding new categories
  d10.stef$cat.ai<-"n.a."
  d10.stef$cat.ai[d10.stef$Noun=="rose"]<-"P&F"
  #range<-14:19
  for(k in range){
  m.ai<-grep("cat.ai",colnames(d10.stef))
  if(length(m.ai)>0)
    m.k.ai<-!is.na(d10.stef$cat.ai) #first training manually edited cats
    sum(m.k.ai)  
  noun<-d10.stef$Noun[k]  
  noun
 # noun<-"rose"
  is.cat<-nouns.cats.known$category[nouns.cats.known$noun==noun]
  if(length(is.cat)>0){
    catfinal<-nouns.cats.known$category[nouns.cats.known$noun==noun][1]
  }
  if(length(is.cat)==0){
     cat.test[[noun]]<-getcat(k,"",nouns.cats.old)
  d3u<-unlist(cat.test[[noun]])
  #empty<-c(0,"n.a.",NA)
  m<-grepl("0|n.a.",d3u)
  sum(m)
  d3u.c<-d3u[!m]
  l3<-length(d3u.c)
  #cat.test[[noun]]<-getcat(82,"",nouns.cats.old)
  listreturn<-cat.test
  cat.wmax<-which.max(listreturn[[noun]][['catscore']][['score']][listreturn[[noun]][['catscore']][['cat']]!="n.a."])
  cat.max<-max(listreturn[[noun]][['catscore']][['score']][listreturn[[noun]][['catscore']][['cat']]!="n.a."])
  catfinal<-"n.a."
  #l3>0
  #is.infinite(cat.max)
  if(l3>1&!is.infinite(cat.max))
    catfinal<-listreturn[[noun]][['catscore']][['cat']][[cat.wmax]]
  #cat.max<-which.max((listreturn[[noun]][['catscore']][['score']]))
  #catfinal<-listreturn$catscore$cat[which.max(listreturn$catscore$score)]
  #catfinal<-listreturn$catscore$cat[which.max(listreturn$catscore$score)]
   # cat.max
  #cat.test$buzzard$cat
  #catfinal<-names(cat.max)
  #print(cat.test[[noun]][['cat']])
  print(catfinal)
  }
  
if(is.null(catfinal))
     catfinal<-"n.a."
  d10.stef$cat.ai[k]<-catfinal
#d.c.k.ar
  l.nouns<-length(nouns.cats.old$noun)
  noun
  
nouns.cats.known.temp<-nouns.cats.known.ai(cat.test[[noun]],m.k.ai) #[[noun]][['coll']]
nouns.cats.known.temp$category<-catfinal
nouns.cats.known.temp$noun<-noun
#nouns.cats.known.temp<-nouns.cats.known.ai(catarray,m.k.ai) #[[noun]][['coll']]
#nouns.cats.new.a<-append(nouns.cats.known[,1:5],nouns.cats.temp[,1:5],after = length(nouns.cats.known$noun))
#nouns.cats.new.a<-append(nouns.cats.old,nouns.cats.known.temp,after = length(nouns.cats.old$noun))
  print(l.nouns)
  nouns.cats.old<-rbind(nouns.cats.old,nouns.cats.known.temp)
  write_csv(nouns.cats.old,"nouns.cats.temp.csv")
  for(w in 80000:1){
    cat("wait",w,"\n")
  }
  #write_csv(nouns.cats.known,"nouns.cats.temp.csv")
  #nouns.cats.old<-nouns.cats.new.i
  }
  listreturn<-list(df=d10.stef,nouns=nouns.cats.old) # TODO, feedback new categories here
  listreturn
  #listreturn$nouns$noun
  length(unique(listreturn$nouns$noun))
    cat("nounscats.old length:",length(nouns.cats.old$lfd),"\n")
  cat("nounscats.temp length:",length(nouns.cats.known.temp$lfd),"\n")
  #cat("nounscats.new length:",length(nouns.cats.new.i$lfd),"\n")
  
return(listreturn)  
return(d10.stef)  
}
#rm(nouns.cats.temp)
#########################################################
### process:
#m.k
m.pr<-which(!m.not)
#from scratch:
#nouns.cats.known<-getknowncats(m.k)
###############################################################
#from saved df
nouns.cats.known<-read.csv("fragrance_known-cats_coll.cpt.csv")
write_csv(nouns.cats.known,"nouns.cats.temp.csv")
ds<-read.csv("nouns.cats.temp.csv")
#write_csv(ds,"nouns.cats.temp.csv")
#d10.stef<-read.csv("~/boxHKW/21S/DH/local/SPUND/corpuslx/stefanowitsch/casestudy2.mod.csv")
###############################################################
#rm(nouns.cats.new)
fun.eval<-function(){
nouns.cats.known$category[nouns.cats.known$noun=="rose"]
length(unique(nouns.cats.known$noun))
length(unique(d10.stef$Noun))

m<-grepl("ailanthus",d10.stef$Noun)
#m.pr<-m.pr[!m]
#m.pr<-m.pr[859:length(m.pr)]
d11<-cat.process(m.pr[161:length(m.pr)])#,nouns.cats.known)
###############################################
### RUN
d11<-cat.process(m.pr) #,nouns.cats.known)
###############################################
d11.df<-data.frame(d11$df)
length(unique(d11$nouns$noun))
#d10.stef$Noun[]
nouns.cats.known$category[nouns.cats.known$noun=="job"]
m<-!is.na(nouns.cats.known$category)
nouns.cats.known<-nouns.cats.known[m,]
catfinal
# noun
# cat.test
#   factor(cat.test)
#   cat.test['AN']
#   catfinal<-names(cat.test[which.max(cat.test)])
#   unique(nouns.cats.known$category)
#     for (i in 5000:1){
#     cat(k,i,sum(m),"\n")
#   }
#   
# }
#write.csv(d10.stef,"fragrance2_ai-cats.csv")
############################################
### evaluate definition:
d10.gold<-read_csv("fragrance2_ai-cats.gold.csv") # manually defined gold standard of cats
#d10.gold<-read_csv("/Volumes/EXT/boxHKW/21S/DH/local/SPUND/corpuslx/stefanowitsch/casestudy2_full.csv") # manually defined gold 
#lapsi
d10.gold<-read_csv("~/boxHKW/21S/DH/local/SPUND/corpuslx/stefanowitsch/casestudy2_full.csv") # manually defined gold 
d10.ai<-ds
length(unique(ds$noun))
cat<-array()
k<-1
for (k in 1:length(d10.stef$Category)){
  noun<-d10.stef$Noun[k]
  cat.u<-unique(d10.ai$category[d10.ai$noun==noun])
  ifelse(length(cat.u)>0,cat[k]<-cat.u,cat[k]<-NA)
  
  
}
cat
d10.stef$category<-cat
#sort back gold df after tokenid:
df[with(df,order(df[,"a"])), ]
d10.gold<-d10.gold[order]
df<-d10.gold
d10.gs<-df[with(df,order(df[,"Token_ID"])), ]
d10.gs$cat.ai<-cat
p1<-c.ass[m.pr]==c.gold[m.pr]
sum(p1,na.rm = T)
sum(p1,na.rm = T)/length(p1)
p2<-d10.gs$Category==d10.gs$cat.ai
sum(p2,na.rm = T)
sum(p2,na.rm = T)/length(p2)

#standard of cats
#c.ai<-d10.gold$cat.ai # cats defined with script
c.gold<-d10.gs$Category # cats corrected manually
d11.df<-data.frame(d11$df)
d11.nouns<-d11$nouns
length(unique(d11.df$Noun))
length(unique(d11.nouns$noun))
c.ai<-d11.df$cat.ai[m.pr]
d11.df$cat.train[m.k.mod]<-d11.df$category.modified[m.k.mod]
d11.df$cat.train[m.k.pet]<-d11.df$Category[m.k.pet]
d11.df$cat.ass<-d11.df$cat.train
d11.df$cat.ass[m.pr]<-d11.df$cat.ai[m.pr]
d11.df$cat.gold<-d10.gold$cat.gold
c.ass<-d11.df$cat.ass # cats defined with script
c.train<-c(d11.df$cat.train)
p1<-c.ass[m.pr]==c.gold[m.pr]
sum(p1,na.rm = T)
sum(p1,na.rm = T)/length(p1)
p2<-c.ass==c.gold
sum(p2,na.rm = T)
sum(p2,na.rm = T)/length(p2)
76/100 # trefferquote to goldstandard overall
62/100 # in not defined cats
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

}
