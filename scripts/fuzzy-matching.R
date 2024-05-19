#20240519(07.01)
#14212.MFN.keyword-matching.model
#################################

a1<-data.frame(id=1:6,token=c("eins","zwei","drei","vier","fünf","sechs"))
a2<-data.frame(id=1:6,token=c("ains","zwei","drei","vier","fuenf","sieben"))
dict<-data.frame(id=1:6,token=c("sechs","sieben","acht","neun","zehn","elf"))

library(fuzzyjoin)
#jw
j1<-stringdist_join(a1,a2,by="token",max_dist = 1,mode = "left",method = "lv",distance_col = "dist")
j1[j1$dist>=1&j1$dist<=2,]
