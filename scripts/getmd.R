#13363.md convert table
#20230904(18.21)
################
#set<-d1
getmd<-function(set){
  df.md<-set
  df.md.h<-paste0("|",paste(colnames(df.md),collapse = "|"),"|")
  df.md.1<-paste0("|",paste(rep(":--",length(df.md)),collapse = "|"),"|")
  df.md.x<-array()
  k<-1
  for(k in 1:length(df.md[,1])){
#    print(k)
    df.md.x[k]<-paste0("|",paste(df.md[k,],collapse = "|"),"|")
  }
#  df.md.x
  df.md.head<-"## csv to md table
"
  return(df.md.c<-c(df.md.head,df.md.h,df.md.1,df.md.x))
  
}
df.md.c
