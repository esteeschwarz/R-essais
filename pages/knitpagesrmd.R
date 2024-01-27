.libPaths(new = "/home/runner/work/R-essais/R-essais/rlibs", include.site = TRUE)
cat("where we at:\n") #were in top dir
print(list.files())
#print(list.files(~))
.libPaths(new = "/usr/local/lib/R/4.3/site-library", include = T)
library(rmarkdown,lib.loc="rlibs")#,lib.loc = "/usr/local/lib/R/4.3/site-library")
library(yaml,lib.loc="/home/runner/work/R-essais/R-essais/rlibs")
library(jquerylib,lib.loc="/home/runner/work/R-essais/R-essais/rlibs")
#library(knitr,lib.loc = "/usr/local/lib/R/4.3/site-library")
#?render_markdown()
#?knit
#?output_format
#knit("pages/README.Rmd",output="pages/index.html")
setwd("pages")
library(xfun)
#render("wp001.Rmd",output_file ="wp001.html",run_pandoc=T)
print("from renderscript")