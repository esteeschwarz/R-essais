# 16313.2idsR

run<-function(s){
msg<-"Hello Owl"
t<-16313

if(s!="")
  source(s)
df<-data.frame(t=t,msg=msg)
print(df)
}