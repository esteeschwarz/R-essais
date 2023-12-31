#20231229(15.59)
#corpuslx.notes
################
#14527.
#/foulie/ vs. /lunchroom/ f=0.5: high frequency
#find why for some qnouns such a high frequency
# > collocate unique (number of coll) was set to 0, manually refined db / all q with only 1 collocate had unique = 0, see 1st script for
# routine to build collocate df again
#/lunchroom/ has only 1 collocate: "tiny"
# /foulie/ == 0 collocates
#2nd: duplicated collocates in nouns.cats.known.cpt: pushes frequency! remove duplicate entries
# try d1u%in%d2u matches again for faster algorithm / recordlinkage needs about 2h (918qnounsx55anounsx1.5s)
# import sets via package googlesheets4
install.packages("googlesheets4")
library(googlesheets4)
?googlesheets4
dtrain<-read_sheet("https://docs.google.com/spreadsheets/d/199KLIWoE8C5vjAqQsKKcqAuzKOfZPkpwAI24jZOaZWg/edit?usp=sharing")
# NO. 403
# try:
gs4_deauth() # before request
baseurl<-"https://sheets.googleapis.com"
sheet<-"/v4/spreadsheets/{spreadsheetId}"
sheets<-"/v4/spreadsheets/199KLIWoE8C5vjAqQsKKcqAuzKOfZPkpwAI24jZOaZWg"
sheeturl<-paste0(baseurl,sheets)
sheeturl
library(httr)
x<-GET(sheeturl)
# NO. 403
# script runs through all qnouns (918), not the unique number
# check if all duplicate qnouns have same cat assigned
# anouns is also only 55 unique, 92 total: compute new cat factor among 55
# applyfactor() needs 11s for each of 55 runs!

qdf<-get.exp("tradition")
qdfx<-qdf$df

# theres inconsistency of predefined cats in the 92 training sample and the finally manually defined gold cats, some nouns were assigned
# to different cats in training and gold data, some duplicate nouns were assigned differently
# after cleaning:
280/918
188/826 # unknown nouns
### 2nd run with match d1u%in%d2u, way faster, see difference if d2u%in%d1u resp longer in shorter array
371/826
### now find pattern of different frequency evaluations and feed back results
266/826
getwd()
library(readr)
write_csv(nouns.cats.known.cpt,"nouns_collocations_cpt.csv")
nouns.cats.known.cpt<-read_csv("nouns_collocations_cpt.csv")
m<-nouns.cats.known.cpt$collocations=="#empty#"
sum(m)
nouns.cats.known.cpt<-nouns.cats.known.cpt[!m,]
m<-nouns.cats.known.cpt$noun=="--"
sum(m)
nouns.cats.known.cpt<-nouns.cats.known.cpt[!m,]
length(nouns.cats.known.cpt$lfd)
m<-grepl("[0-9]",nouns.cats.known.cpt$collocations)
sum(m)
m
nouns.cats.known.cpt<-nouns.cats.known.cpt[!m,]
length(nouns.cats.known.cpt$lfd)
### refine cats:
nouns.cats.known.cpt$category<-NA
for (k in 1:length(nouns.cats.known$noun)){
  cat(k,"\n")
  qnoun<-nouns.cats.known$noun[k]
  cat.k<-nouns.cats.known$category[k]
  m<-nouns.cats.known.cpt$noun%in%qnoun
  nouns.cats.known.cpt$category[m]<-cat.k
}
m<-!is.na(nouns.cats.known.cpt$category)
sum(m)
m<-nouns.cats.known.cpt$noun%in%nouns.cats.known$noun
sum(m)
#length(unique(nouns.cats.known.cpt))
m<-grep("odor",nouns.cats.known.cpt$noun) # double entry for 1 coll
sum(m)
nouns.cats.known.cpt$noun[m[1]]<-"odor"
nouns.cats.known.cpt<-nouns.cats.known.cpt[!m,]
###
nouns.cats.known.2<-nouns.cats.known.cpt
m<-!is.na(nouns.cats.known.2$category)
n.array<-nouns.cats.known.2$noun[m]
n.array.u<-unique(n.array)
q.array<-unique(nouns.cats.known.2$noun)
coll.array.known<-nouns.cats.known.2$collocations[m]
length(n.array)
#nouns.cats.known.2$freq<-NA
#resulttable$id<-1:length(resulttable$noun)
#nouns.cats.known.2$id<-1:length(nouns.cats.known.2$lfd)
#nouns.cats.known.2$anoun<-NA
#nouns.cats.known.2$cat.ai<-NA
# k<-100
# qnoun<-"rose"
# for (k in 1:length(q.array)){
# cat(k,"\n")
#   qnoun<-q.array[k]
#   rnoun.array<-resulttable$q%in%qnoun
#   sum(rnoun.array)
#   r.a.id<-resulttable$id[rnoun.array]
#   resulttable$freq[r.a.id]
#   r.f.array<-resulttable$freq[r.a.id]
#   r.n.array<-resulttable$noun[r.a.id]
#   r.cat.array<-resulttable$cat[r.a.id]
#   r.c.array<-nouns.cats.known.2$id[nouns.cats.known.2$noun==qnoun]
#   #  sum(r.c.array)
#   nouns.cats.known.2$freq[r.c.array]<-r.f.array
#   nouns.cats.known.2$anoun[r.c.array]<-r.n.array
#   nouns.cats.known.2$cat.ai[r.c.array]<-r.cat.array
#   u<-1
#   # for (u in 1:length(a.noun.u)){
#   #   cat(u,"\n")
#   #   m1<-resulttable$noun[cnoun.array][resulttable$noun==a.noun.u[k]]
#   #   f.res<-resulttable$freq[m1]
#   #   m2<-nouns.cats.known.2$noun[cnoun.array][nouns.cats.known.2$noun==a.noun.u[k]]
#   #   nouns.cats.known.2$freq[m2]<-f.res
#   # }
# #    cnoun.res.a<-resulttable$noun
# 
# }
# 55*length(q.array)
# 55*length(unique(nouns.cats.known.2$collocations))
library(readr)
setwd("~/Documents/GitHub/R-essais/SPUND/corpusLX")
nouns.cats.known.2<-read_csv("nouns_collocations_cpt.csv")
# nouns.cats.known.3<-data.frame(anoun=rep(n.array.u,length.out=length(unique(nouns.cats.known.2$collocations))),
#                                           qnoun=,coll=unique(nouns.cats.known.2$collocations))
# nouns.cats.known.3<-data.frame(anoun=rep(n.array.u,length(unique(nouns.cats.known.2$noun))),
#                                qnoun=rep(unique(nouns.cats.known.2$noun)),coll=NA)
q.noun.u<-unique(nouns.cats.known.2$noun)
m<-!is.na(nouns.cats.known.2$category)
sum(m)
a.noun.u<-unique(nouns.cats.known.2$noun[m])
nouns.1.matrix<-matrix(nrow = length(nouns.cats.known.2$noun),ncol = length(a.noun.u)+4)
nouns.1.matrix[,2]<-nouns.cats.known.2$noun
nouns.1.matrix[,1]<-nouns.cats.known.2$collocations
nouns1df<-data.frame(nouns.1.matrix)
#nouns.1.matrix[,1]<-unique(nouns.cats.known.2$noun)
colnames(nouns1df)<-c("collocations","noun","cat","f",a.noun.u)
k<-1
qnoun<-"river"
for(k in 1:length(q.noun.u)){
  
  qnoun<-q.noun.u[k]
  qc.array<-nouns1df$noun==qnoun
  sum(qc.array)
  d2u<-nouns1df$collocations[qc.array]
  d<-2
  for(d in 1:length(a.noun.u)){
    anoun<-a.noun.u[d]
    cat(k,qnoun,"in",d,anoun,"--->")  
    anoun.array<-nouns1df$noun==anoun
    d1u<-nouns1df$collocations[anoun.array]
    # d2u<-qc.array
    m<-d1u%in%d2u
    mf<-sum(m,na.rm = T)/(length(d1u)+length(d2u))
    cat("f=",mf,"\n")
    nouns1df[qc.array,anoun]<-mf
  }
  
}
write.csv(nouns1df,"nouns1df.csv")
library(purrr)
nouns2df<-as.matrix.data.frame(nouns1df[1:length(nouns1df$collocations),5:length(nouns1df)])
mode(nouns2df)<-"double"
nouns1df$f<-rowSums(nouns2df)
  #######
# Load the dataset
#data <- read.csv("house_prices.csv")
library(lmerTest)
library(lme4)
library(stats)
# Split the data into training and testing sets
#train <- data[1:100, ]
#test <- data[101:146, ]
train<-nouns3df[1:200,]
test<-nouns3df[101:146,]
# Train the model using linear regression
model <- lm(f ~ cat, data = train)
summary(model)
model$residuals
names(model$effects)
model$fitted.values
m<-nouns3df$cat==""
sum(m,na.rm = T)
# Make predictions on the test set
predictions <- predict(model, newdata = test)
predictions
nouns3df<-nouns1df[1:length(nouns1df$collocations),1:4]
for(c in 1:length(q.noun.u)){
  qnoun<-q.noun.u[c]
  q.in.set<-nouns.cats.known.2$noun%in%qnoun
  q.cat<-unique(nouns.cats.known.2$category[q.in.set])
  q.in.df<-nouns3df$noun%in%qnoun
  nouns3df$cat[q.in.df]<-q.cat
}
1:10
p.df<-data.frame(freq=nouns3df$f[as.double(names(model$fitted.values))],fitted=model$fitted.values)
plot(p.df$freq~p.df$fitted)
plot(1:100~sample(1:100,100))
plot(model)
length(unique(nouns3df$cat))
nouns4df<-data.frame(id=1:(length(a.noun.u)*length(nouns3df$collocations)),
                     c.id=rep(1:length(nouns3df$collocations),length(a.noun.u)),
                     coll=NA,qnoun=NA,anoun=NA,cat=NA,freq=NA)
nouns4df$coll<-rep(nouns3df$collocations,length(a.noun.u))
nouns4df$qnoun<-rep(nouns3df$qnoun,length(a.noun.u))
ln<-length(unique(nouns3df$noun))
for(k in 1:ln){
  qnoun<-q.noun.u[k]
  m<-nouns3df$noun%in%qnoun
  nouns3df$q.id[m]<-k
  
}
nouns4df$q.id<-rep(nouns3df$q.id,length(a.noun.u))
#nouns4df$c.id<-rep(1:length(nouns3df$collocations),length(a.noun.u))
# nouns4df$anoun<-rep(a.noun.u,length.out=length(nouns3df$collocations))
# sum(duplicated(nouns4df))


for (k in 1:length(q.noun.u)){
  qnoun<-q.noun.u[k]
  m<-nouns4df$q.id%in%k
  nouns4df$noun[m]<-qnoun
  
}
a.id<-1:length(a.noun.u)
a.id.f<-rep(a.id,each=length(nouns3df$collocations))
anoun<-rep(a.noun.u,each=length(nouns3df$collocations))
tail(a.id.f)
nouns4df$cat<-rep(nouns3df$cat,length(a.noun.u))
nouns4df$a.id<-a.id.f
nouns4df$anoun<-anoun
k<-7
for(k in length(nouns4df$id):1){
  cat(k,"\n")
  df2.pos<-nouns4df$c.id
  anoun<-nouns4df$anoun[k]
  a.pos<-which(anoun==colnames(nouns2df))
  a.value<-nouns2df[df2.pos,a.pos]
  nouns4df$freq[k]<-a.value
}

# Evaluate the performance of the model
mse <- mean((test$price - predictions)^2)
rmse <- sqrt(mse)
cat("Root Mean Squared Error:", rmse)
