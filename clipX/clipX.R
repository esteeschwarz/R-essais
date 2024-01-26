library(stringi)
library(clipr)
sep=""
rm.interpunct<-T
clipX<-function (sep = "", rm.interpunct = ",;.!?"){
x<-read_clip()
x2<-unlist(stri_split_boundaries(x,type="character"))
#x2
int.punct<-unlist(stri_split_boundaries(rm.interpunct,type="character"))
intregx<-paste0('[',rm.interpunct,']')
m5<-grepl(intregx,x2)
#x2[m5]
x4<-x
if(rm.interpunct!=F)
  x2<-x2[!m5]
  x4<-gsub(intregx,"",x)
#x4
x3<-x4
m1<-grep(" ",x2)
m2<-grep("\n",x2)
m3<-grep(",",x2)
m4<-grep(sep,x2)
if(sum(m4)>0&length(m4)!=max(m4))
  x3<-unlist(stri_split_regex(x3,sep))
if(sum(m1)>0)
  x3<-unlist(stri_split_regex(x3," "))
#x3
if(sum(m2)>0)
  x3<-unlist(stri_split_regex(x3,"\n"))
#x3
if(sum(m3)>0)
  x3<-unlist(stri_split_regex(x3,","))
#x3
x4<-paste0(x3,collapse = '","')
x4<-paste0('c("',x4,'")')
write_clip(x4)
}
clipX(rm.interpunct = '",.?!')
clipX()

c("clipX","is","a","function","to","process","clipboard","content","it","depends","on","the","clipr","library","thing","thing1","bag","lot","chairs","mother","service","period","a-other","sumobs","dif","sum")


c("clipX","is","a","function","to","process","clipboard","content","it","depends","on","the","clipr","library","thing","thing1",""bag"","lot","chairs","mother","service","period","a-other","sumobs","dif","sum")


c("clipX","is","a","function","to","process","clipboard","content","it","depends","on","the","clipr","library","thing","thing1","bag","lot","chairs","mother","service","period","a-other","sumobs","dif","sum")

c("clipX","is","a","function","to","process","clipboard","content","it","depends","on","the","clipr","library","thing","thing1","bag","lot","chairs","mother","service","period","a-other","sumobs","dif","sum")


c("clipX","is","a","function","to","process","clipboard","content","it","depends","on","the","clipr","library","thing","thing1","bag","lot","chairs","mother","service","period","a-other","sumobs","dif","sum")


c("clipX","is","a","function","to","process","clipboard","content","it","depends","on","the","clipr","library","thing","thing1","bag","lot","chairs","mother","service","period","a-other","sumobs","dif","sum")
c("clipX","is","a","function","to","process","clipboard","content.","it","depends","on","the","clipr","library.")

c("thing","thing1","bag","lot","chairs","mother","service","period","a-other","sum.obs","dif","sum")