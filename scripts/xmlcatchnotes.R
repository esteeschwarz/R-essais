#library(httr)
library(xml2)
src<-"~/boxHKW/21S/DH/local/R/xml/nietzsche-ocr.xml"
xmldoc<-read_xml(src)
library(purrr)
install.packages("purrr")
xmldoc%>%xml_ns_strip()
allp<-xml_find_all(xmldoc,"/document/body/p")
'//*[@id="collapsible38"]/div[1]/div[1]'
allnotes<-xml_text(xmldoc)
xml_text(xml_child(xml_child(xml_child(xml_child(xmldoc, 1), 1), 6), 2)) #pos 6 to iterate