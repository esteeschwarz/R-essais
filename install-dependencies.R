#install dependencies, called if !cache hit
dir.create("rlibs")
cat(">>>>>> rlibs dir:\n")
libs<-list.files("rlibs")
print(libs)

dep<-read.csv("rdependencies.csv")

rlib<-"rlibs"
repos<-'https://cloud.r-project.org'
dep.array<-dep$pkg[dep$chk!=1]
m<-dep.array%in%libs
dep.to.install<-dep.array[!m]
cat(">>>>>> libs to install:\n")
print(dep.to.install)
dep.array<-dep.to.install
for(k in 1:length(dep.array)){
  li<-dep.array[k]
  cat(">>>>>>>> library to install:",li,"\n")
  install.packages(li,lib=rlib,repos=repos)
  cat(">>>>>>>> library installed\n")
  dep$chk[dep$pkg==li]<-1
}
#dep$chk[4]<-1
#write.csv(dep,"rdependencies.csv",row.names = F)
