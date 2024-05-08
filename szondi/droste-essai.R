# install.packages("gutenberg_works")
# library(gutenbergr)
# csvlist = "https://docs.google.com/spreadsheets/d/e/2PACX-1vT87reVxrWeaYiRL6hP-fnPB9MX6Rq4eZHdEURjbzY5MbY7Q5Y59MWZYc309CqIqLxBmdaRog5BhbWn/pub?gid=0&single=true&output=csv"
# 
# booklist <- read.csv(csvlist, header = TRUE, stringsAsFactors=FALSE)
# path2 <- "/Users/djw12/Desktop/stylo/corpus/"
# 
# for (row in 1:nrow(booklist)){
#   bookname <- booklist[row,4]
#   stylotext <- gutenberg_download(booklist[row,1])
#   stylotext <- select(stylotext, text)
#   write.table(stylotext, file = paste0(path2, bookname), sep="", row.names = FALSE)
# }
# 
# dta <- gutenberg_download(6698)
# a1<- gutenberg_authors()
# library(gutenberg_works)
# works<-gutenberg_works(languages = "de")
# m<-grep("Droste",works$author)
# works$title[m]
#####################################
### droste judenbuche fetch gutenberg
dr1<-readLines("https://www.gutenberg.org/ebooks/45798.txt.utf-8")
##################################################################
dr1
m<-grep(1738,dr1)
m2<-grep("End of Project Gutenberg's Die Judenbuche, by Annette von Droste-Hülshoff",dr1)
m
dr1[m]
dr2<-dr1[m:m2] 
#library(quanteda)
#library(collostructions)
t1<-tokenize_word1(dr2)
frf<-function(x)freq.list(x)
fr1<-lapply(t1, frf)
f1<-freq.list(unlist(t1))
#stopl<-read.csv("/Users/guhl/boxHKW/21S/DH/local/R/rmdessais/corpus/wolf_LE_stopwords.csv",sep = ";")
#stop2<-readLines("/Users/guhl/Documents/GitHub/ETCRA5_dd23/R/data/stopword_list_de.txt")
stopx<-stopl[stopl$stop==T,]
stop3<-c(stop2,stopx$word)
m<-f1$WORD%in%stop3
sum(m)
f2<-f1[!m,]
f2
m<-grep("holz",f2$WORD)
m<-grep("jud",f2$WORD)
m<-grep("jud|Jud",dr2)
#f2[m,]
dr2[m]
### topic: holz
# frame holz, word field analysis
