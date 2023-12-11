library(readr)
d1<-read_csv("chitest.csv")
d2<-d1[1:5,2:4]
#d3<-d2[,2:3]
d3<-data.frame(d2[,2:3],row.names = d2$...2)
rownames(d3)
#ql.1<-sum(d3[1,])
typeof(d3[1,])
d4<-rbind(d3,colSums(d3))
rownames(d4)[length(d4[,1])]<-"sum"
d4$sum<-rowSums(d4)

###table created
x<-d4
d5<-matrix(d4/d4["sum","sum"]*d4["little","sum"])
d5<-matrix(d4["sum",]/d4["sum","sum"]*d4$sum,ncol = 3)
s1<-function(x)x["sum",]/x["sum","sum"]*x["sum"]
lapply(x, s1)
dim(x["sum"])
x["little"]
d4.p<-d4
for(k in 1:length(d4$sum)){
d5[k,"boy"]<-d4["sum","boy"]/d4["sum","sum"]*d4$sum[k]
}
for(k in 1:length(d4$sum)){
  d5[k,"girl"]<-d4["sum","girl"]/d4["sum","sum"]*d4$sum[k]
}
chisq.test(d4[1:5,1:2],d5[1:5,1:2])
# same in numbers table
#x-squared:
#sum(boy)/sum(total)*sum(item) > abs(dif)
d4["sum","boy"]/d4["sum","sum"]*d4[1,"sum"]
d6<-d5
for(k in 1:length(d4$boy)){
  d6[k,"boy"]<-abs(d4[k,"boy"]-d5[k,"boy"])
  
}
for(k in 1:length(d4$boy)){
  d6[k,"girl"]<-abs(d4[k,"girl"]-d5[k,"girl"])
  
}
ma<-c(5610,2257,168938,10233063)
d3<-matrix(ma,ncol = 2)
typeof(d3[1,])
d4<-rbind(d3,colSums(d3))
d4<-cbind(d4,rowSums(d4))
rownames(d4)[length(d4[,1])]<-"sum"
colnames(d4)[length(d4[1,])]<-"sum"
d4<-as.data.frame(d4)
#d4.rs<-rowSums(d4)
#d4$sum<-NA
typeof(d4[1,1])
#d4$sum<-d4.rs
d5<-matrix(1:4,ncol = 2)
d5[1,1]<-d4["sum",1]/d4["sum","sum"]*d4$sum[1]
d5[1,2]<-d4["sum",2]/d4["sum","sum"]*d4$sum[1]
d5[2,1]<-d4["sum",1]/d4["sum","sum"]*d4$sum[2]
d5[2,2]<-d4["sum",2]/d4["sum","sum"]*d4$sum[2]
#mm
chisq.test(d4[1:2,1:2],d5)


