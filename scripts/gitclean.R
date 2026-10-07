t<-Sys.getenv("HKW_TOP")
setwd(t)
fd<-list.dirs(recursive=T,full.names=T)
g<-lapply(fd,function(x){
  g<-grepl("\\.git$",x)
})
g<-unlist(g)
fg<-fd[g]
fg

# du /Users/guhl/boxHKW/21S/DH/local > dir-du.csv
d<-read.csv(paste0(Sys.getenv("GIT_TOP"),"/temp/dir-du.csv"),sep="\t")
d<-d[order(d[,1],decreasing=T),]
d[,1]<-d[,1]/1000/1000


fl<-list.files(f,recursive=T,full.names=T)
