#14433.corpusLX.sketchengine-BNC_evaluation
#20231023(18.22)
#essai: why query results differ
################################
library(readr)
library(stringi)
src1<-"~/boxHKW/21S/DH/local/SPUND/corpuslx/stefanowitsch/concordance_preloaded_bnc2_tt31_q3.V.submit.det.40c.csv"
src2<-"~/boxHKW/21S/DH/local/SPUND/corpuslx/stefanowitsch/q3.V.submit.csv"

d1<-read_csv(src1,skip = 4)
d2<-read_delim("~/boxHKW/21S/DH/local/SPUND/corpuslx/stefanowitsch/q3.V.submit.csv", 
                          delim = "\t", escape_double = FALSE, 
                          col_names = FALSE, trim_ws = TRUE)
colnames(d2)[5:7]<-c("left","kwic","right")
colnames(d1)[2:4]<-c("left","kwic","right")
###
#cont<-function(d)paste(d[,2:4],collapse = " ")
#d1$cont<-lapply(d1,cont)
# d1$cont<-NA
# for(k in 1:length(d1$Reference)){
# d1$cont[k]<-paste(d1[k,2:4],collapse = " ")
# }
d2$cont<-NA
for(k in 1:length(d2$X1)){
  
  d2$cont[k]<-paste(d2[k,5:7],collapse = " ")
d2$cont<-gsub("<<<|>>>","",d2$cont)
  }
d2$cont<-gsub("<<<|>>>","",d2$cont)
d2$cont<-gsub("  "," ",d2$cont)
### d1 get 3 wds KWIC
k<-1
for (k in 1:length(d1$Reference)){
  d1.sub<-gsub("(</s>|<s>)","",d1$left[k])

d1.left<-stri_split_regex(d1.sub," ",simplify = T)
d1.left<-d1.left[!is.na(d1.left)]
d1.ln<-length(d1.left)
d1.ln3<-d1.ln-2
d1.left.3<-d1.left[d1.ln3:d1.ln]
d1$left.3[k]<-paste(d1.left.3,collapse = " ")
}
for (k in 1:length(d1$Reference)){
  d1.sub<-gsub("(</s>|<s>)","",d1$right[k])
  d1.right<-stri_split_regex(d1.sub," ",simplify = T)
  d1.right<-d1.right[!is.na(d1.right)]
  d1.right.3<-d1.right[1:3]
  d1$right.3[k]<-paste(d1.right.3,collapse = " ")
}
d1$cont<-NA
for(k in 1:length(d1$Reference)){
  d1$cont[k]<-paste(d1$left.3[k],d1$kwic[k],d1$right.3[k],collapse = " ")
}

#d1.right<-stri_split_regex(d1$right," ",simplify = T)
m<-d2$cont%in%d1$cont
sum(m)
d1.q<-stri_split_regex(d1$Reference,",",simplify = T)
d1$q.1<-d1.q[,2]
d1$q.t<-gsub("#","",d1.q[,3])
d1$Reference<-d1.q[,1]

m<-d2$X2%in%d1$q.1
sum(m) #all
m<-d1$q.1%in%d2$X2
sum(m) #3836
d1$cont[!m]
