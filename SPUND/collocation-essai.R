#20231209(19.18)
#stefanowitsch, collostructions()
#################################

library(collostructions)
library(readr)
library(httr)
library(xml2)
library(stringi)
library(lme4)
head(beginStart,12)

#ssh
[hw="perfumed"][]{0,3}[pos="N.*"]
80
[hw="fragrant|perfumed|scented|sweet(-)smelling"][]{1,3}[pos="N.*"]
365
#set DataDirectory "./cqpdata"
#show named;
count Last by word%c on match[3]
[hw="fragrant|perfumed|(.*?)scented|(.*?)smelling"][]{1,3}[pos="N.*"];
492
fragr2 = [hw="fragrant|perfumed|(.*?)scented|(.*?)smelling"][]{1,3}[pos="N.*"] within s;
419

cat Love  > "| collocates.pl -c > love-case-insensitive.csv"
cat fragr2  > "| tidycwb.pl > public_html/cqpdata/fragrance-m.csv";

#PP2 = subset PP1 where matchend: [lemma = "time"];
define macro < 'makros.txt'
count fragr2 by word on matchend;

d1<-read_csv("https://userpage.fu-berlin.de/stschwarz/cqpdata/fragrance-coll.csv")
d2<-read_csv("https://userpage.fu-berlin.de/stschwarz/cqpdata/fragrance-m.csv",col_names = c("corpus","id","left","kwic","right"))
d3<-read_table("https://userpage.fu-berlin.de/stschwarz/cqpdata/fragrance-f1.csv")
d4<-read_table("https://userpage.fu-berlin.de/stschwarz/cqpdata/fragrance-f2.csv")
d5<-read_table("https://userpage.fu-berlin.de/stschwarz/cqpdata/fragrance-f3.csv")
d6<-read_table("https://userpage.fu-berlin.de/stschwarz/cqpdata/fragrance-NN.csv",col_names = c("count","token","ref"))
d7<-read_csv("https://userpage.fu-berlin.de/stschwarz/cqpdata/tempodor.csv",col_names = c("corpus","id","left","kwic","right"))
d8.fragr<-read_table("https://userpage.fu-berlin.de/stschwarz/cqpdata/fragrant-NN.csv",col_names = c("count","token","ref"))
d8.perf<-read_table("https://userpage.fu-berlin.de/stschwarz/cqpdata/perfumed-NN.csv",col_names = c("count","token","ref"))
d8.scent<-read_table("https://userpage.fu-berlin.de/stschwarz/cqpdata/scented-NN.csv",col_names = c("count","token","ref"))
d8.smel<-read_table("https://userpage.fu-berlin.de/stschwarz/cqpdata/smelling-NN.csv",col_names = c("count","token","ref"))

#HTOED: historical thesaurus of the oxford english dictionary, define category

word<-"sauce"
getcat<-function(word,count){
#q<-sprintf("https://www.ht.ac.uk/category-selection/?word=%s&label=&category=&year=&startf=&endf=&startl=&endl=",word)
q<-sprintf("https://www.ht.ac.uk/category-selection/?word=%s&page=1&categoryMinis=off&categorySort=length",word)
g<-GET(q)
r<-content(g,"text")
rhtm<-read_html(r)

#xpath category list:
xp<-'//*[@id="mainInner"]/h4'
h4.list<-xml_find_all(rhtm,xp)
h4.true<-length(h4.list)>0
#xp.h<-'#mainInner > h4'
#xp.div<-'.//body/div[2]' #wk
cat.array<-array()
if (h4.true){
xp.div<-'.//body/div[2]/div[1]/div[1]/*' #wk
list.div<-xml_find_all(rhtm,xp.div)
l.div<-length(list.div)
xp.div.h4<-grep(xml_text(h4.list),list.div)
xp.h4.pos<-xp.div.h4
# xml_text(list.div[xp.h4.pos:(xp.h4.pos+3)])
# listitem.1<-xml_text(list.div[xp.h4.pos+1])
# listitem.2<-xml_text(list.div[xp.h4.pos+2])
# listitem.3<-xml_text(list.div[xp.h4.pos+3])
#listitem.3<-xml_text(list.div[xp.h4.pos:(xp.h4.pos+2)])
cat.array<-array()
#k<-3
for (k in 1:3){
  listitem.1<-xml_text(list.div[xp.h4.pos+k])
  item.split.1<-stri_split_regex(listitem.1,"\\{",simplify = T)
  item.split.2<-stri_split_regex(item.split.1[,1],"::",simplify = T)
  item.split.3<-stri_split_regex(item.split.2[,1],"\\.",simplify = T)
  cat.1<-item.split.3[length(item.split.3)]
#  print(cat.1)
  cat.array[k]<-cat.1
}

# item.split.1<-stri_split_regex(listitem.1.3,"::",simplify = T)
# item.split.2<-stri_split_regex(item.split.1[,1],"\\.",simplify = T)
# cat.1<-item.split.2[length(item.split.2)]
print(cat.array)
}
#wait loop
#count<-1
for(k in 20000:1){
cat("run",count,"on (",word,")",", wait:",k,"\n")
  }
return(cat.array)
}
# catq<-getcat("sauce")
# catq<-getcat("odem")
# catq

#getcats from df
d6.s<-subset(d6,d6$count>1)
get.cat.df<-function(d6.s){
cat.list<-list()
#k<-1
for(k in 1:length(d6.s$token)){
cat.list[[d6.s$token[k]]]<-getcat(d6.s$token[k],k)  
}
cat.df<-as.data.frame(cat.list)
cat.df<-as.data.frame(t(cat.df))
cat.df<-cbind(token=rownames(cat.df),cat.df)
}
cat.df<-get.cat.df(d6.s)
write_csv(cat.df,"fragrance_HTOED-categories-s.csv")
###wks.
cat.u.1<-list()
for(k in 2:length(cat.df)){
cat.u.1[[k]]<-unique(cat.df[,k])
}
cat.u.2<-unique(unlist(cat.u.1))
m<-cat.u.2==""
cat.u.2<-cat.u.2[!m]
m<-is.na(cat.u.2)
cat.u.2<-cat.u.2[!m]
cat.u.2

###collostructions
data("beginStart")
head(beginStart,12)
x<-collex.dist(beginStart)
head(x,12)
d8.5<-join.freqs(d8.scent[,2:1],d8.fragr[,2:1])
x<-collex.dist(d8.5,threshold = 2)
tail(x,12)
x
###wks
#lme analysis:

d8.fragr.l<-d8.fragr
d8.fragr.l$ref<-"fragrant"
d8.perf.l<-d8.perf
d8.perf.l$ref<-"perfumed"
d8.scent.l<-d8.scent
d8.scent.l$ref<-"scented"
d8.smel.l<-d8.smel
d8.smel.l$ref<-"smelling"
d8.cpt<-rbind(d8.fragr.l,d8.perf.l,d8.scent.l,d8.smel.l)
colnames(d8.cpt)[1]<-"freq"
lm1<-lmer(freq ~ token + (1|ref),d8.cpt)
lm.s<-summary(lm1)
lm.s
lm2<-lmer(freq ~ ref + (1|token),d8.cpt)
lm2.s<-summary(lm2)
lm2.s
###wks.
###now all cats:
tok.u<-unique(d8.cpt$token)
tok.u.df<-data.frame(token=tok.u,freq=NA)
tok.u.cat<-get.cat.df(tok.u.df)
write_csv(tok.u.cat,"fragrance_HTOED-categories-s2.csv")
k<-1
d8.cpt$category<-NA
for(k in 1:length(d8.cpt$token)){
  tok<-d8.cpt$token[k]
  m<-tok==tok.u.cat$token
  cat<-tok.u.cat$V1[m]
  d8.cpt$category[m]<-cat
}
#m<-d8.cpt$token%in%tok.u.cat$token
lm3<-lmer(freq ~ ref + (1|token) + (0+category),d8.cpt)
lm3.s<-summary(lm3)
lm3.s$coefficients[,5]
df<-lm3.s$coefficients
colnames(df)[5]<-"p"
df[with(df,order(df[,1])), ]
lm.coef.p<-as.data.frame(df[order(df[,"p"]),])
lm.coef.p$category<-NA
#k<-1
lm.coef.p$category<-gsub("(ref|category)","",rownames(lm.coef.p))
for (k in head(lm.coef.p$category,10)){
m<-k==d8.cpt$category
m[is.na(m)]<-F
print(d8.cpt$token[m])
}     
sum(m,na.rm = T)
k
lm.coef.p$category[1:10]
