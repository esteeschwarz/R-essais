print(2/100)
print("testrun")
print(list.files())
samplepage<-'
<p>just a small html sample index...:</p><a href="pkg.csv">packages installed</a><p><a href="wp001.html>wp001</a></p>'
writeLines(samplepage,"pages/index.html")
#install.packages("rmarkdown",lib="rlibs",repos = 'https://cloud.r-project.org')
#install.packages("markdown",lib="rlibs",repos = 'https://cloud.r-project.org')
pkg<-installed.packages(lib.loc="rlibs")
write.csv(pkg,"pages/pkg.csv")
source("knitpagesrmd.R")
#dir.create("/home/rlibs")
#print("packages installed")
#print(installed.packages())
#library(jsonlite,lib.loc = "/usr/local/Cellar/r/4.3.2/lib/R/library")
#print(getOption("repos"))
#just testing cache, force yaml run
#cache: run wks.,
#json chgd, rerun install, now try if cached.