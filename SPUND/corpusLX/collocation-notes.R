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
# script runs through all qnouns (918), not the unique number
# check if all duplicate qnouns have same cat assigned
# anouns is also only 55 unique, 92 total: compute new cat factor among 55
# applyfactor() needs 11s for each of 55 runs!

qdf<-get.exp("tradition")
qdfx<-qdf$df
