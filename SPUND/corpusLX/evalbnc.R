library(readr)
d1<-read_csv("https://userpage.fu-berlin.de/stschwarz/give.csv",col_names = c("corpus","id","left","kwic","right"))
write_csv(d1,"bnc-give.csv")
d.coll<-read_csv("word_sketch_give.csv",skip = 2)
c.conc.o<-c("money","something")
m1<-grep(c.conc.o[1],d1$right,ignore.case = T)
d1[m1,]
d1$eval.c<-NA
d1$eval.c[m1]<-1
c.conc.pr<-"to"
m2<-grep(c.conc.pr[1],d1$right,ignore.case = T)
d1[m2,]
d1$eval.to<-NA
d1$eval.to[m2]<-1
m.c<-m2%in%m1
d1$eval.1<-NA
d1$eval.1[m2][m.c]<-1
d1[m2[m.c],]
#word2vec of this
library(stringi)
library(word2vec)
toks  <- strsplit(paste(d1[,3:5],collapse = " "), split = "[[:space:][:punct:]]+")
toks  <- strsplit(d1$cpt, split = "[[:space:][:punct:]]+")
model <- word2vec(x = toks, dim = 15, iter = 20)
#model <- word2vec(x = paste(d1[,3:5]), dim = 15, iter = 20)
#model <- word2vec(x = d1$cpt, dim = 1, iter = 20)
#d1$cpt<-paste(d1[,3:5],collapse  = " ")
comb<-function(x)paste(x,collapse = " ")
a<-c("dreimal schwarzer","kater hättich gern","gegessen oderso")
b<-c("dreimal schwarzer","kater hättich gern","gegessen oderso")
d<-data.frame(a,b)
lapply(d, comb)
paste(a,collapse  = " ")
d1$cpt<-NA
k<-1
for(k in 1:length(d1$corpus)){
d1$cpt[k]<-paste(d1[k,3:5],collapse  = " ")
print(k)
}
d1$cpt[1]
emb   <- as.matrix(model)
emb   <- predict(model, c("bus", "money", "something"), type = "embedding")
emb
nn    <- predict(model,c("bus", "money", "something"), type = "nearest", top_n = 5)
nn
nn    <- predict(model,c("give"), type = "nearest", top_n = 5)
nn

install.packages("openNLP")
library(openNLP)
library(NLP)

sentence <- "I have a pen. I took it from my friend. I gave it to my sister."
a2 <- annotate(sentence, list(Maxent_Sent_Token_Annotator(),Maxent_Word_Token_Annotator()))

tagged<-annotate(sentence,Maxent_POS_Tag_Annotator(),a2)
tagged
for (i in 1:length(tagged)) {
  word <- sentence[tagged[i]$start:tagged[i]$end]
  tag <- attr(tagged[[i]], "features")$POS
  if (word %in% c("give", "take", "have")) {
    cat(sprintf("%s is used as a %s\n", word, tag))
  }
}
s <- paste(c("Pierre Vinken, 61 years old, will join the board as a ",
             "nonexecutive director Nov. 29.\n",
             "Mr. Vinken is chairman of Elsevier N.V., ",
             "the Dutch publishing group."),
           collapse = "")
s <- as.String(s)
s<-paste(c("Ich habe eine schwarze Katze gegessen, bevor ich zur Arbeit ging"))
s<-as.String(s)
## Need sentence and word token annotations.
sent_token_annotator <- Maxent_Sent_Token_Annotator()
word_token_annotator <- Maxent_Word_Token_Annotator()
a2 <- annotate(s, list(sent_token_annotator, word_token_annotator))
#library(openNLPdata)
#install.packages("openNLPmodels.de", repos = "http://datacube.wu.ac.at/", type = "source")
parse_annotator <- Parse_Annotator()
## Compute the parse annotations only.
p <- parse_annotator(s, a2)
## Extract the formatted parse trees.
ptexts <- sapply(p$features, `[[`, "parse")
ptexts
## Read into NLP Tree objects.
ptrees <- lapply(ptexts, Tree_parse)
ptrees
