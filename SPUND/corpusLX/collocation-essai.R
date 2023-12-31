#20231209(19.18)
#stefanowitsch, collostructions()
#################################

### these libraries are necessary:
library(collostructions)
library(readr)
library(httr)
library(xml2)
library(stringi)
##################################
library(lme4)
#head(beginStart,12)

### > this is not to source, its containing just notes on the cqp queries
# tempfun.1<-function(){
# #pre:
 #install.packages(file.choose(),repos = NULL)
# #ssh
# [hw="perfumed"][]{0,3}[pos="N.*"]
# 80
# [hw="fragrant|perfumed|scented|sweet(-)smelling"][]{1,3}[pos="N.*"]
# 365
# #set DataDirectory "./cqpdata"
# #show named;
# count Last by word%c on match[3]
# [hw="fragrant|perfumed|(.*?)scented|(.*?)smelling"][]{0,3}[pos="N.*"];
# 492
# fragr2 = [hw="fragrant|perfumed|(.*?)scented|(.*?)smelling"][]{1,3}[pos="N.*"] within s;
# 419
# cat Love  > "| collocates.pl -c > love-case-insensitive.csv"
# cat fragr2  > "| tidycwb.pl > public_html/cqpdata/fragrance-m.csv";
# 
# #PP2 = subset PP1 where matchend: [lemma = "time"];
# define macro < 'makros.txt'
# count fragr2 by word on matchend;
# 
# #14503.in class
# [lemma="fragrant|perfumed|scented|sweet-smelling"]([pos="av.*"%c]? [pos="jj.*"%c]){0,3}[pos="N.*"%c]{1,};
# 
# }
#########################################################################################################

#this is just some used datasets
############
# d1<-read_csv("https://userpage.fu-berlin.de/stschwarz/cqpdata/fragrance-coll.csv")
# d2<-read_csv("https://userpage.fu-berlin.de/stschwarz/cqpdata/fragrance-m.csv",col_names = c("corpus","id","left","kwic","right"))
# d3<-read_table("https://userpage.fu-berlin.de/stschwarz/cqpdata/fragrance-f1.csv")
# d4<-read_table("https://userpage.fu-berlin.de/stschwarz/cqpdata/fragrance-f2.csv")
# d5<-read_table("https://userpage.fu-berlin.de/stschwarz/cqpdata/fragrance-f3.csv")
# d6<-read_table("https://userpage.fu-berlin.de/stschwarz/cqpdata/fragrance-NN.csv",col_names = c("count","token","ref"))
# d7<-read_csv("https://userpage.fu-berlin.de/stschwarz/cqpdata/tempodor.csv",col_names = c("corpus","id","left","kwic","right"))
# d8.fragr<-read_table("https://userpage.fu-berlin.de/stschwarz/cqpdata/fragrant-NN.csv",col_names = c("count","token","ref"))
# d8.perf<-read_table("https://userpage.fu-berlin.de/stschwarz/cqpdata/perfumed-NN.csv",col_names = c("count","token","ref"))
# d8.scent<-read_table("https://userpage.fu-berlin.de/stschwarz/cqpdata/scented-NN.csv",col_names = c("count","token","ref"))
# d8.smel<-read_table("https://userpage.fu-berlin.de/stschwarz/cqpdata/smelling-NN.csv",col_names = c("count","token","ref"))
#d9.stef<-read_table("https://userpage.fu-berlin.de/stschwarz/cqpdata/smelling-NN.csv",col_names = c("count","token","ref"))
############################################################################################################################

#these are the important datasets:
d9.stef<-read_csv("https://userpage.fu-berlin.de/anatolstef/t/FRAGRANT-NOUN-COHA.csv",col_names = c("corpus","id","year","left","kwic","right"))
d9.st<-read_csv("https://userpage.fu-berlin.de/stschwarz/cqpdata/fragrant-COHA.csv",col_names = c("corpus","id","year","left","kwic","right"))
d10.stef<-read.csv("https://userpage.fu-berlin.de/stschwarz/cqpdata/Casestudy2_AS_unpleasant.csv")
d10.stef<-read.csv("~/boxHKW/21S/DH/local/SPUND/corpuslx/stefanowitsch/casestudy2.mod.csv")
############
############
############
#HTOED: historical thesaurus of the oxford english dictionary, define category

#word<-"sauce"
#############################
### this function sends a request to below adress and fetches the category for the specified noun/word
getcat<-function(word,count){
#q<-sprintf("https://www.ht.ac.uk/category-selection/?word=%s&label=&category=&year=&startf=&endf=&startl=&endl=",word)
q<-sprintf("https://www.ht.ac.uk/category-selection/?word=%s&page=1&pos=n&categoryMinis=off&categorySort=length",word)
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
#k<-1
#waiting loop to prevent rejected server requests to HTOED...
for(k in 2000:1){
cat("run",count,"- wait:",k,"\n")
  }
return(cat.array)
}
##############################################################
# catq<-getcat("sauce")
# catq<-getcat("odem")
# catq

#getcats from df
#d6.s<-subset(d6,d6$count>1)

# dataset<-d9.df.sub
# columnname<-"noun"
######################################################################################################
### this is the important function to call with a dataset containing a column with nouns to be queried
#columnname<-"Noun"
#dataset<-d10.stef
get.cat.df<-function(dataset,columnname){
cat.list<-list()
#k<-2
#k
d6.s<-dataset
d6.s$category<-NA
for(k in 1:length(d6.s[,columnname])){
cat<-getcat(d6.s[k,columnname],k)[1]
cat<-gsub(" $","",cat)
ifelse(!is.na(cat),cat.list[[as.character(d6.s[k,columnname])]]<-cat,cat.list[[as.character(d6.s[k,columnname])]]<-"none")  
d6.s$category[k]<-cat
}
#chk.
#d9.df.sub$noun
cat.df<-as.data.frame(cat.list)
cat.df<-as.data.frame(t(cat.df))
cat.df<-cbind(token=rownames(cat.df),cat.df)
return(d6.s)
}
############################################

#cat.df<-get.cat.df(d6.s,"noun")
# d6.s.sub<-subset(d9.st,)
# write_csv(cat.df,"fragrance_HTOED-categories-s.csv")
###wks.
tempfun.2<-function(){
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
}

#14503.inclass
d9.lemma<-stri_split_regex(d9.st$kwic,"/",
                           simplify = T)

m<-grep("UNDEF",d9.lemma[,2])
m<-grep("UNDEF",d9.lemma[,4])
m.split<-stri_split_regex(d9.lemma[m,3]," ",simplify = T)
d9.lemma[m,4]<-m.split[,2]
d9.df<-as.data.frame(cbind(year=d9.st$year,adj=d9.lemma[,2],noun=d9.lemma[,4]))
#write_csv(d9.df,"fragrant_year-adj-NN.csv")


#40 year period
d9.df$period<-NA
m<-d9.df$year<=1850
d9.df$period[m]<-"A"
m<-d9.df$year>=1850&d9.df$year<=1890
d9.df$period[m]<-"B"
m<-d9.df$year>=1891&d9.df$year<=1930
d9.df$period[m]<-"C"
m<-d9.df$year>=1931&d9.df$year<=1970
d9.df$period[m]<-"D"
m<-d9.df$year>=1971&d9.df$year<=1990
d9.df$period[m]<-"E"
m<-d9.df$year>=1991&d9.df$year<=2030
d9.df$period[m]<-"F"


 #collex.covar(d9.df[c("noun","period")])

#get categories for class set:
#d9.df.sub<-subset(d9.df,year==1919)
#d9.df.sub.10<-
#d9.df.sub.cat<-get.cat.df(d9.df.sub,"noun")
#d9.df.w.cats<-get.cat.df(d9.df,"noun")
### stops in 209 because of ![a-z]
m<-grep("[^a-z]",d9.df$noun)
d9.df$noun[m]
m2<-grep("<",d9.df$noun[m])
m.split<-stri_split_regex(d9.df$noun[m][m2],"<",simplify = T)
d9.df$noun[m][m2]<-m.split[,2]
m[22]
### > missing data for that row in dataset!
m<-!is.na(d9.df$year)
sum(m)-length(d9.df$year)
-2
d9.df.cl<-subset(d9.df,!is.na(year))
m<-grep("--",d9.df.cl$noun,invert = T)
d9.df.cl<-d9.df.cl[m,]
######################################

#run the following command decommented

##########
#d9.df.w.cats<-get.cat.df(d9.df.cl,"noun")
##########################################
#write_csv(d9.df.w.cats,"fragrance_COHA_wt-categories.csv")

# read in saved dataset
d9.a<-read.csv("https://github.com/esteeschwarz/R-essais/raw/main/SPUND/corpusLX/fragrance_COHA_wt-categories.csv") #!!! not read in with read_csv(), but read.csv() !!!!!

######################
# clean up categories:
# only run if starting from basic cqp dataset
temp.clean<-function(set){
  d9.a<-as.data.frame(set)
cat.split<-stri_split_regex(d9.a$category,"  ",simplify = T)
#m<-""!=cat.split[,2]
d9.a$category<-cat.split[,1]
sum(m,na.rm = T)
set$category<-gsub(" $","",set$category)
m<-d9.a$category==")"
if(sum(m,na.rm = T)>0)
   d9.a$category[m]<-NA
#write_csv(d9.a,"fragrance_COHA_wt-categories.csv")
return(d9.a)
}
#set<-d10.df
temp.clean.2<-function(set){
  #t1<-"be auty "
  #gsub(" $","p",t1)
  set$category<-gsub(" $","",set$category)
return(set)
}
#d9.b<-temp.clean.2(d9.a)
#write_csv(d9.b,"fragrance_COHA_wt-categories.csv")
#############################
temp.eval.1<-function(){
length(unique(d9.b$category))
### stats
#library(collostructions)
#collex.covar(d9.df.cl[c("noun","period")])
collex.covar(d9.a[c("noun","period")])
#typeof(d9.a)
col.1<-collex.covar(d9.b[c("period","category")])
col.1
plot(col.1)
barplot(col.1$fS1~col.1$SLOT1+col.1$SLOT2)
barplot(col.1$fS1~col.1$SLOT2+col.1$SLOT1)
lm4<-lmer(OBS~SLOT2+(EXP|SLOT1),col.1)
summary(lm4)
sum(col.1$fS2)
m<-is.na(d9.a$noun)
sum(m)
}
###################
#14511.casestudy2: unpleasant

d10.df<-get.cat.df(d10.stef,"Noun")
#typeof(d10.stef$Noun)
typeof(d10.df)
d10.df<-temp.clean(d10.df)
#d10.df<-temp.clean.2(d10.df)
m<-""==d10.df$Category
d10.df.m<-d10.df
d10.df.m$Category[m]<-d10.df$category[m]
getwd()
#write_csv(d10.df.m,"~/boxhkw/21s/dh/local/spund/corpuslx/stefanowitsch/casestudy2.csv")
##########
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
m<-2
k<-1
d10.df.m$cat.m<-NA
for(k in 1:length(d10.df.m$Noun)){
  noun<-d10.df.m$Noun[k]
  m<-grep(noun,cat.list)
print(noun)  
print(names(cat.list[m]))
print("printed")
if(length(m)==1)
   d10.df.m$cat.m[k]<-names(cat.list[m])
  }




