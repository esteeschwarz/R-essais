# 16313.2idsR

run<-function(s){
msg<-"Hello Owl"
df<-data.frame(t=16313,msg=msg)
print(df)
if(s!="")
  source(s)
}