print(2/100)
print("testrun")
print(list.files())
samplepage<-'
<p>just a small html sample index...:</p><a href="pkg.csv">packages installed</a>'
writeLines(samplepage,"pages/index.html")
pkg<-installed.packages(lib.loc="rlibs")
write.csv(pkg,"pages/pkg.csv")
#dir.create("/home/rlibs")
#install.packages("rmarkdown",lib="/home/rlibs")
#Sys.setenv(R_CRAN_WEB = "https://cloud.r-project.org")
#install.packages("knitr",contriburl = "https://cloud.r-project.org/src/contrib")
#install.packages("rmarkdown",contriburl = "https://cloud.r-project.org/src/contrib")
#print("packages installed")
#print(installed.packages())
#library(jsonlite,lib.loc = "/usr/local/Cellar/r/4.3.2/lib/R/library")
#print(getOption("repos"))
#just testing cache, force yaml run
#cache: run wks.,
#json chgd, rerun install, now try if cached.