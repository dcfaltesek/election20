#load vectors with file locations
#this code will use RTFs of 100 results from lexis nexis each for democratic and presidential and candidate from dates april 2-9
#this is then further filtered for newspapers. All global newspapers are included.

#file location vectors 
A<-"~/Dropbox/Advanced Collaborative Experience/1.RTF"
B<-"~/Dropbox/Advanced Collaborative Experience/2.RTF"
C<-"~/Dropbox/Advanced Collaborative Experience/3.RTF"
D<-"~/Dropbox/Advanced Collaborative Experience/4.RTF"
E<-"~/Dropbox/Advanced Collaborative Experience/5.RTF"

#read-in the rtfs
A1<-read_rtf(A)
B1<-read_rtf(B)
C1<-read_rtf(C)
D1<-read_rtf(D)
E1<-read_rtf(E)

#make dataframes
A2<-data.frame(A1)
B2<-data.frame(B1)
C2<-data.frame(C1)
D2<-data.frame(D1)
E2<-data.frame(E1)

#rename the column with all the data
colnames(A2)[1]<-"text"
colnames(B2)[1]<-"text"
colnames(C2)[1]<-"text"
colnames(D2)[1]<-"text"
colnames(E2)[1]<-"text"

#make them into a single frame
corpus<-bind_rows(A2$A1,B2$B1,C2,D2,E2)
View(corpus)

#at this point we attach the basic sentiment script
#we will leave the text relatively uncleaned, for the most part words like body will have little imapact
#in this structure, every paragraph is a separate cell, there are:
dim(corpus)

#the current number of paragraphs
I<-1:9336
#and attach that new paragraph refernece number, it will be important later
corpusB<-data.frame(corpus, I)

#for a number of important reasons: secure text features of the sentence corpus
library(textfeatures)
corpusFeatures<-textfeatures(corpusB$text)

#deploy tidyverse
library(tidytext)

#corpus broken as sentences
corpusWords<-corpusB%>%
  unnest_tokens(word, text)

#now we can attach our sentiment 
sentimentsA<-get_sentiments("afinn")
sentimentsA<-data.frame(sentimentsA)

#we now have a fully joined sentiment set
cor2<-inner_join(corpusWords, sentimentsA, by = "word")

#at this point we can begin to reassemble the data
corpusSummaries<-cor2%>%
  group_by(I)%>%
  summarize(mean(score), sum(score), sd(score))

#and finally, we can join our entire dataset together. 
corE<-bind_cols(corpusB, corpusFeatures)
inner_join(corE, corpusSummaries, by = as.character(I))

#full dataset
Z<-full_join(corE, corpusSummaries)
#rename rows for ease 
colnames(Z)[20]<-"mean"
colnames(Z)[21]<-"sum"
colnames(Z)[22]<-"sd"

library(ggplot2)
ggplot(Z, aes(I, sum, colour=sd, size=mean))+geom_jitter()

TY<-str_detect(Z$text, "Warren")

ggplot(Z, aes(I, mean, colour=TY))+geom_jitter()+ scale_fill_manual(values = c("FALSE", "black", "TRUE", "red"),






#segment by candidate
library(stringr)
#orourke
beto<-Z %>% 
  filter(str_detect(text, "Beto") | str_detect(text, "O'Rourke"))
ggplot(beto, aes(I, sum, colour=sd, size=mean))+geom_jitter()
mean(beto$sum, na.rm = TRUE)
dim(beto)

#klobuchar
klobuchar<-Z %>% 
  filter(str_detect(text, "Amy") | str_detect(text, "Klobuchar"))
ggplot(klobuchar, aes(I, sum, colour=sd, size=mean))+geom_jitter()
mean(klobuchar$sum, na.rm = TRUE)
dim(klobuchar)

#biden
biden<-Z %>% 
  filter(str_detect(text, "Joe") | str_detect(text, "Biden"))
ggplot(biden, aes(I, sum, colour=sd, size=mean))+geom_jitter()
mean(biden$sum, na.rm = TRUE)
dim(biden)

#gillibrand
gillibrand<-Z %>% 
  filter(str_detect(text, "Kirsten") | str_detect(text, "Gillibrand"))
ggplot(gillibrand, aes(I, sum, colour=sd, size=mean))+geom_jitter()
mean(gillibrand$sum, na.rm = TRUE)
dim(gillibrand)

#bernie sanders
Sanders<-Z %>% 
  filter(str_detect(text, "Bernie") | str_detect(text, "Sanders"))
ggplot(Sanders, aes(I, sum, colour=sd, size=mean))+geom_jitter()
mean(Sanders$sum, na.rm = TRUE)
dim(Sanders)

#warren
#harris cluster
Warren<-Z %>% 
  filter(str_detect(text, "Elizabeth") | str_detect(text, "Warren"))
ggplot(Warren, aes(I, sum, colour=sd, size=mean))+geom_jitter()
mean(Warren$sum, na.rm = TRUE)
dim(Warren)

Warren<-Z %>% 
  filter(str_detect(text, "Elizabeth") | str_detect(text, "Warren"))

WarrenB<-slice(Warren, -4)
WarrenC<-filter(WarrenB, sum>= -7)
ggplot(WarrenC, aes(I, sum, colour=sd, size=mean))+geom_jitter()
mean(WarrenC$sum, na.rm = TRUE)

#kamalaharris
Harris<-Z %>% 
  filter(str_detect(text, "Kamala") | str_detect(text, "Harris"))
ggplot(Harris, aes(I, sum, colour=sd, size=mean))+geom_jitter()
mean(Harris$sum, na.rm = TRUE)
dim(Harris)

#mayorpete
Buttigieg<-Z %>% 
  filter(str_detect(text, "Pete") | str_detect(text, "Buttigieg"))
ggplot(Buttigieg, aes(I, sum, colour=sd, size=mean))+geom_jitter()
mean(Buttigieg$sum, na.rm = TRUE)
dim(Buttigieg)

#booker
filter(str_detect(text, "Corey") | str_detect(text, "Booker"))
ggplot(Booker, aes(I, sum, colour=sd, size=mean))+geom_jitter()
mean(Booker$sum, na.rm = TRUE)
dim(Booker)


