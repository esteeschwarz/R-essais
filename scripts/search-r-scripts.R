git_folder <- Sys.getenv("GIT_TOP")
hkw_folder <- Sys.getenv("HKW_TOP")
#r_files_git <- list.files(git_folder, pattern = "\\.R$|\\.r$", recursive = TRUE, full.names = TRUE)
#r_files_hkw <- list.files(hkw_folder, pattern = "\\.R$|\\.r$", recursive = TRUE, full.names = TRUE)
r_files_git <- list.files(git_folder, pattern = "\\.R$|\\.r$|\\.Rmd$|\\.rmd$", recursive = TRUE, full.names = TRUE)
r_files_hkw <- list.files(hkw_folder, pattern = "\\.R$|\\.r$|\\.Rmd$|\\.rmd$", recursive = TRUE, full.names = TRUE)
r_files<-c(r_files_git,r_files_hkw)
#head(r_files)
library(dplyr)
#r_files.s<-r_files[1:10]
get.scripts<-function(top_folder,n){
  r_files.s<-r_files[1:n]
  df <- lapply(r_files.s, function(f) {
  lines <- readLines(f, warn = FALSE)
  data.frame(
    location = f,
    line = seq_along(lines),
    code = lines,
    stringsAsFactors = FALSE
  )
}) %>% bind_rows()
}
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
df$comment<-F
df$comment[get.com(df)]<-T
com<-T
q
get.q<-function(q,df,com=F){
  ifelse(com,code<-df$code,code<-df$code[df$comment])
  m<-grep(q,code)
  print(code[m])
  return(m)

  }

mc<-get.q("readLines",r.scripts.db,T)
mc<-get.q("readtext",r.scripts.db,T)
mc<-get.q("keywords",df,T)
df$location[mc]
file.edit(df$location[mc[1]])
