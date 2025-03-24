library(jsonlite)

notebook_path <- "~/boxHKW/21S/DH/local/SPUND/intLX/HA/llama/Llama-notebook.ipynb"

notebook <- fromJSON(notebook_path)
codecells<-notebook$cells
codecells$source
m<-codecells$cell_type=="code"
code_cells <- lapply(notebook$cells[m], function(cell) {
  #if (cell$cell_type == "code") {
    return(cell$source, collapse = "\n"))
  #} else {
   # return(NULL)
  }
})
codelist<-codecells$source[m]
codelist[[1]]
codecontent<-unlist(codelist)
codecontent
codecontent<-codecontent[codecontent!=""&codecontent!="\n"]
writeLines(codecontent, "~/boxHKW/21S/DH/local/SPUND/intLX/HA/llama/Llama-notebook.py")
code.tx<-readLines("~/boxHKW/21S/DH/local/SPUND/intLX/HA/llama/Llama-notebook.py")
code.tx<-code.tx[code.tx!=""&code.tx!="\n"]
writeLines(code.tx, "~/boxHKW/21S/DH/local/SPUND/intLX/HA/llama/Llama-notebook.py")
