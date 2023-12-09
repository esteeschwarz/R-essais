#20231209(19.18)
#stefanowitsch, collostructions()
#################################

library(collostructions)
library(readr)
head(beginStart,12)

#ssh
[hw="perfumed"][]{0,3}[pos="N.*"]
80
[hw="fragrant|perfumed|scented|sweet(-)smelling"][]{1,3}[pos="N.*"]
365
#set DataDirectory "./cqpdata"
#show named;
count Last by word%c on match[3]
[hw="fragrant|perfumed|(.*?)scented|(.*?)smelling"][]{1,3}[pos="N.*"];
492
fragr2 = [hw="fragrant|perfumed|(.*?)scented|(.*?)smelling"][]{1,3}[pos="N.*"] within s;
419

cat Love  > "| collocates.pl -c > love-case-insensitive.csv"
d1<-read_csv("https://userpage.fu-berlin.de/stschwarz/fragrance-coll.csv")
d2<-read_csv("https://userpage.fu-berlin.de/stschwarz/fragrance-m.csv",col_names = c("corpus","id","left","kwic","right"))
d3<-read_table("https://userpage.fu-berlin.de/stschwarz/fragrance-f1.csv")
d4<-read_table("https://userpage.fu-berlin.de/stschwarz/fragrance-f2.csv")
d5<-read_table("https://userpage.fu-berlin.de/stschwarz/fragrance-f3.csv")
d6<-read_table("https://userpage.fu-berlin.de/stschwarz/fragrance-NN.csv")

#PP2 = subset PP1 where matchend: [lemma = "time"];

count fragr2 by word on matchend;

