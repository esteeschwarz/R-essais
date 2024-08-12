# Read the temporary file created by Python
csvfile <- tempfile(fileext = ".csv")
#system(paste("python your_python_script.py", csvfile))
getwd()
#library(reticulate)
source_python("py-temp.py")
read.csv(csvfile)  # Read the CSV data in R
