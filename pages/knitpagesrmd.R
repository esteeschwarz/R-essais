library(rmarkdown)
library(knitr)
#?render_markdown()
#?knit
#?output_format
#knit("pages/README.Rmd",output="pages/index.html")
render("pages/README.Rmd",output_format="html",output_file ="pages/index.html")