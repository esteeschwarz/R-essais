library(rmarkdown,lib.loc = "/usr/local/lib/R/4.3/site-library")
#library(knitr,lib.loc = "/usr/local/lib/R/4.3/site-library")
#?render_markdown()
#?knit
#?output_format
#knit("pages/README.Rmd",output="pages/index.html")
render("pages/README.Rmd",output_format="html",output_file ="pages/index.html")