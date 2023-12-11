library(word2vec)
library(udpipe)
udpipe_train(
  file = file.path(getwd(), "german-gsd-ud-2.5-191206.udpipe"),
  files_conllu_training,
  files_conllu_holdout = character(),
  annotation_tokenizer = "default",
  annotation_tagger = "default",
  annotation_parser = "default"
)
library(DramaAnalysis)
data()
library(readtext)
## Take data and standardise it a bit
xdf<-readtext("mann_joseph.txt")
xdf<-readLines("mann_joseph.txt")
df<-data(brussels_reviews, package = "udpipe")
unique(brussels_reviews$language)
x <- subset(brussels_reviews, language == "nl")
x <- tolower(x$feedback)
library(readr)
xdfben<-read_csv("benjamin13.2-DF.csv")
library(xml2)
benhtm<-read_html("benjamin13.html",encoding = "UTF-16")
bentxt<-xml_text(benhtm)
library(stringi)
bentxt.s<-stri_split_regex(bentxt," ")
xmodel<-udpipe_download_model("german-gsd")
x<-udpipe_load_model(xmodel$file_model)
x<-x$model
## Build the model get word embeddings and nearest neighbours
model <- word2vec(x = xdf, dim = 15, iter = 20)
model <- word2vec(x = strsplit(xdf, split = "[[:space:][:punct:]]+"), dim = 15, iter = 20)

model <- word2vec(x = bentxt.s, dim = 15, iter = 20)

emb   <- as.matrix(model)
head(emb)
emb   <- predict(model, c("Kleid", "Brunnen", "unknownword"), type = "embedding")
emb
nn    <- predict(model, c("Kleid", "Brunnen"), type = "nearest", top_n = 5)
nn    <- predict(model, c("nehmen", "nehme","geben","gebe"), type = "nearest", top_n = 5)
nn    <- predict(model, c("zu", "auf","an","nach"), type = "nearest", top_n = 5)
nn

## Get vocabulary
vocab   <- summary(model, type = "vocabulary")
head(vocab)
# Do some calculations with the vectors and find similar terms to these
emb     <- as.matrix(model)
vector  <- emb["nehmen", ] - emb["dein", ] + emb["Kleid", ]
vector  <- emb["gib", ]  - emb["mir", ]
predict(model, vector, type = "nearest", top_n = 20)

vector  <- emb["gastvrouw", ] - emb["gastvrij", ]
predict(model, vector, type = "nearest", top_n = 5)

vectors <- emb[c("gastheer", "gastvrouw"), ]
vectors <- rbind(vectors, avg = colMeans(vectors))
predict(model, vectors, type = "nearest", top_n = 10)

## Save the model to hard disk
path <- "mymodel.bin"

write.word2vec(model, file = path)
model <- read.word2vec(path)


## 
## Example of word2vec with a list of tokens 
## 
toks  <- strsplit(bentxt, split = "[[:space:][:punct:]]+")
model <- word2vec(x = toks, dim = 15, iter = 20)
emb   <- as.matrix(model)
emb   <- predict(model, c("bus", "toilet", "unknownword"), type = "embedding")
emb
nn    <- predict(model, c("Hand", "Finger","Fuß"), type = "nearest", top_n = 5)
nn

## 
## Example getting word embeddings 
##   which are different depending on the parts of speech tag
## Look to the help of the udpipe R package 
##   to get parts of speech tags on text
## 
library(udpipe)
data(brussels_reviews_anno, package = "udpipe")
x <- subset(brussels_reviews_anno, language == "fr")
x <- subset(x, grepl(xpos, pattern = paste(LETTERS, collapse = "|")))
x$text <- sprintf("%s/%s", x$lemma, x$xpos)
x <- subset(x, !is.na(lemma))
x <- split(x$text, list(x$doc_id, x$sentence_id))

model <- word2vec(x = x, dim = 15, iter = 20)
emb   <- as.matrix(model)
nn    <- predict(model, c("cuisine/NN", "rencontrer/VB"), type = "nearest")
nn
nn    <- predict(model, c("accueillir/VBN", "accueillir/VBG"), type = "nearest")
nn

source("https://www.stgries.info/teaching/groningen/coll.analysis.r")

