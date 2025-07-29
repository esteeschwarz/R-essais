library(reticulate)
py_available()
py_version()
reticulate::conda_list()
reticulate::conda_install("env-01")
conda_create("env-01",python_version = "3.13")
# https://www.anaconda.com/download
py_version()
reticulate::use_python_version("3.13.5")
install_python("3.13")
use_python("/Users/guhl/.pyenv/versions/3.13.5/bin/python3.13")
use_condaenv(reticulate::conda_list()[2,1])
reticulate::conda_list()
library(reticulate)
py_list_packages()
py_install(c("flask","pytorch","transformers","scikit-learn"))



library(httr)
url<-"http://localhost:5000/resolve"
body<-list(
  text="i take the #insert appropriate anapher, reference# for the past years and still losing weight. do you think 2 is too much?"
)
body<-toJSON(body)
body
headers <- c("Content-Type" = "application/json")
#wd<-Sys.getenv("GIT_TOP")
# Read the JSON data from the file
#json_data <- readLines(paste0(wd,"/idsL/backend/R/sample_data_100.json"), warn = FALSE)
json_data<-body
json_data
# Perform the POST request
r <- POST(url, add_headers(.headers = headers), body = json_data, encode = "json")

t<-content(r,"text")
t
#headers<-list("Content-Type" = "application/json")
library(jsonlite)
.headers<-toJSON(headers)
.headers
x<-POST(url,add_headers(.headers),body)
