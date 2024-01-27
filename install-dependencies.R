#install dependencies, called if !cache hit
dir.create("rlibs")
print(list.files("rlibs"))
dep<-read.csv("rdependencies.csv")
rlib<-"rlibs"
repos<-'https://cloud.r-project.org'
dep.array<-dep$pkg[dep$chk!=1]
for(k in 1:length(dep.array)){
  cat("libs to install:",dep.array[k],"\n")
  cat("suspended\n")
  install.packages(dep.array[k],lib=rlib,repos=repos)
}