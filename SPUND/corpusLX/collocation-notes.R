#20231229(15.59)
#corpuslx.notes
################
#14527.
#/foulie/ vs. /lunchroom/ f=0.5: high frequency
#find why for some qnouns such a high frequency
# > collocate unique (number of coll) was set to 0, manually refined db / all q with only 1 collocate had unique = 0, see 1st script for
# routine to build collocate df again
#/lunchroom/ has only 1 collocate: "tiny"
# /foulie/ == 0 collocates
#2nd: duplicated collocates in nouns.cats.known.cpt: pushes frequency! remove duplicate entries
# try d1u%in%d2u matches again for faster algorithm / recordlinkage needs about 2h (918qnounsx55anounsx1.5s)
# import sets via package googlesheets4
install.packages("googlesheets4")
library(googlesheets4)
?googlesheets4
dtrain<-read_sheet("https://docs.google.com/spreadsheets/d/199KLIWoE8C5vjAqQsKKcqAuzKOfZPkpwAI24jZOaZWg/edit?usp=sharing")
# NO. 403
# try:
gs4_deauth() # before request
baseurl<-"https://sheets.googleapis.com"
sheet<-"/v4/spreadsheets/{spreadsheetId}"
sheets<-"/v4/spreadsheets/199KLIWoE8C5vjAqQsKKcqAuzKOfZPkpwAI24jZOaZWg"
sheeturl<-paste0(baseurl,sheets)
sheeturl
library(httr)
x<-GET(sheeturl)
# NO. 403
# script runs through all qnouns (918), not the unique number
# check if all duplicate qnouns have same cat assigned
# anouns is also only 55 unique, 92 total: compute new cat factor among 55
# applyfactor() needs 11s for each of 55 runs!

qdf<-get.exp("tradition")
qdfx<-qdf$df

# theres inconsistency of predefined cats in the 92 training sample and the finally manually defined gold cats, some nouns were assigned
# to different cats in training and gold data, some duplicate nouns were assigned differently
# after cleaning:
280/918
188/826 # unknown nouns
### 2nd run with match d1u%in%d2u, way faster, see difference if d2u%in%d1u resp longer in shorter array
371/826
### now find pattern of different frequency evaluations and feed back results
266/826
