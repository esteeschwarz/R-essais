git_folder <- Sys.getenv("GIT_TOP")
hkw_folder <- Sys.getenv("HKW_TOP")
arkiv1<-"/Volumes/NAS4-MAC/arkiv/work"
arkivcopy<-"/Volumes/NAS4-MAC/arkiv/assembled/"
arkivtest<-"/Volumes/NAS4-MAC/arkiv/work/Texte/Alte/Werk fev 09"
arkivtest<-"/Volumes/NAS4-MAC/arkiv/work/Texte/austausch/savechange/shaharitG3"
#r_files_git <- list.files(git_folder, pattern = "\\.R$|\\.r$", recursive = TRUE, full.names = TRUE)
#r_files_hkw <- list.files(hkw_folder, pattern = "\\.R$|\\.r$", recursive = TRUE, full.names = TRUE)
#r_files_git <- list.files(git_folder, pattern = "\\.R$|\\.r$|\\.Rmd$|\\.rmd$", recursive = TRUE, full.names = TRUE)
#r_files_hkw <- list.files(hkw_folder, pattern = "\\.R$|\\.r$|\\.Rmd$|\\.rmd$", recursive = TRUE, full.names = TRUE)
#r_files<-c(r_files_git,r_files_hkw)
tfiles1 <- list.files(arkivtest, pattern = "\\.txt$|\\.pages$|\\.cwk$|\\.doc$|\\.rtf|.+[^.]$", recursive = TRUE, full.names = TRUE)
tfiles1 <- list.files(arkiv1, recursive = TRUE, full.names = TRUE)
#tfiles1 <- list.files(arkivtest, pattern = "[^.]", recursive = TRUE, full.names = TRUE)
tfiles1
save(tfiles1,file = paste0(Sys.getenv("HKW_TOP"),"/sys/files_zweite-work.RData"))
m<-grepl("(\\.)",tfiles1)
sample(tfiles1[!m],100)
sum(!m)
sans.p<-tfiles1[!m]
m2<-grepl("jpg|jpeg",sans.p)
sans.p<-sans.p[!m2]
m3<-grepl("\\.rtf$|\\.pages$|\\.doc$|\\.txt$|\\.cwk$",tfiles1)
sum(m3)
t1<-c(tfiles1[m3],sans.p)
#head(r_files)
library(dplyr)
#r_files.s<-r_files[1:10]
n<-10
3+4
m3<-grep("\\.rtd",tfiles1)
t4<-tfiles1[m3]
#copied<-list()
copy.files<-function(t1,n,copied){
  t3<-t1[1:n]
  k<-1
  for(k in 1:length(t3)){
  print(k)
  f<-t3[k]
  f2<-basename(f)
  #??path
  f4<-paste0(arkivcopy,f2)  
  copy.t<-file.copy(f,f4,overwrite = F)
  copied[[f2]]<-copy.t
    

  }
  return(copied)
}
length(cr)
n<-length(t4)
cr<-copy.files(t1,n)
cr<-copy.files(t4,n,cr)
cr.sf<-cr
sum(unlist(cr))
n1<-names(cr)
n2<-n1[!unlist(cr)]
# lines<-1:10
# seq_along(lines)
#i<-1
get.scripts.i<-function(rfiles,n){
  r_files.s<-r_files[1:n]
  
df <- bind_rows(
  lapply(seq_along(r_files.s), function(i) {
    lines <- readLines(r_files.s[i], warn = FALSE)
    finfo<-file.info(r_files.s[i])
    data.frame(
      id = i,
      location = r_files[i],
      size = finfo$size,
      created = finfo$ctime,
      modified = finfo$mtime,
      line = seq_along(lines),
      code = lines,
      stringsAsFactors = FALSE
    )
  })
)
}
#system.time(get.scripts(top_folder,10))
#system.time(get.scripts.i(r_files,10))

save.db<-function(){
df<-get.scripts.i(r_files,length(r_files))
r.scripts.db<-df
#save(r.scripts.db,file = paste0(Sys.getenv("HKW_TOP"),"/R/R-scripts-DB.RData"))
#head(df)
}
#########
# query
q<-"dependencies"
load(paste0(Sys.getenv("HKW_TOP"),"/R/R-scripts-DB.RData"))

get.com<-function(df){
  m<-grep("^#",df$code)
  m2<-grep("^ *?#",df$code)
  #m3<-grep("^#",df$code)
  m4<-c(m,m2)
  m5<-c(m2,m)
}
# head(df$code[m2],1000)
# head(df$code[get.com(df)],1000)

# df$comment<-F
# df$comment[get.com(df)]<-T
# com<-T
# q
get.q<-function(q,df,com=F){
  ifelse(com,code<-df$code,code<-df$code[df$comment])
  m<-grep(q,code)
  print(code[m])
  return(m)

  }

#mc<-get.q("readLines",r.scripts.db,T)
#mc<-get.q("readtext",r.scripts.db,T)
#mc<-get.q("keywords",df,T)
mc<-get.q("POST",r.scripts.db,T)
r.scripts.db$location[mc]
file.edit(r.scripts.db$location[mc[1]])

#mx<-grep(".Rmd",r.scripts.db$location)
