load(paste0(Sys.getenv("HKW_TOP"),"/sys/files_zweite-work.RData"))
m<-grep("\\.rtd",tfiles1)
rtd<-tfiles1[m]
copyto<-"/Volumes/NAS4-MAC/arkiv/assembled/"
k<-rtd[1]
copied<-list()
for (k in rtd){
  f<-basename(k)
  print(f)
  f2<-paste0(copyto,f)
  cp<-file.copy(k,f2)
  copied[[f]]<-cp
}
copied
