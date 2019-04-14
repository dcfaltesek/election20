#calcuate and produce the leaderboard
#this file depends on outcomes from candidate analysis

beto<-Z %>% 
  filter(str_detect(text, "Beto") | str_detect(text, "O'Rourke"))
betoB<-beto%>%summarize("index" = mean(sum, na.rm=TRUE), "var"=sd(mean, na.rm=TRUE), "name"="O'Rourke")
betoB<-mutate(betoB, "volume"=dim(beto)[1])


klobuchar<-Z %>% 
  filter(str_detect(text, "Amy") | str_detect(text, "Klobuchar"))
klobucharB<-klobuchar%>%summarize("index" = mean(sum, na.rm=TRUE), "var"=sd(mean, na.rm=TRUE), "name"="Klobuchar")
klobucharB<-mutate(klobucharB, "volume"=dim(klobuchar)[1])

biden<-Z %>% 
  filter(str_detect(text, "Joe") | str_detect(text, "Biden"))
bidenB<-biden%>%summarize("index" = mean(sum, na.rm=TRUE), "var"=sd(mean, na.rm=TRUE), "name"="Biden")
bidenB<-mutate(bidenB, "volume"=dim(biden)[1])

gillibrand<-Z %>% 
  filter(str_detect(text, "Kirsten") | str_detect(text, "Gillibrand"))
gillibrandB<-gillibrand%>%summarize("index" = mean(sum, na.rm=TRUE), "var"=sd(mean, na.rm=TRUE), "name"="Gillibrand")
gillibrandB<-mutate(gillibrandB, "volume"=dim(gillibrand)[1])

Sanders<-Z %>% 
  filter(str_detect(text, "Bernie") | str_detect(text, "Sanders"))
SandersB<-Sanders%>%summarize("index" = mean(sum, na.rm=TRUE), "var"=sd(mean, na.rm=TRUE), "name"="Sanders")
SandersB<-mutate(SandersB, "volume"=dim(Sanders)[1])

Warren<-Z %>% 
  filter(str_detect(text, "Elizabeth") | str_detect(text, "Warren"))
WarrenB<-Warren%>%summarize("index" = mean(sum, na.rm=TRUE), "var"=sd(mean, na.rm=TRUE), "name"="Warren")
WarrenB<-mutate(WarrenB, "volume"=dim(Warren)[1])

Harris<-Z %>% 
  filter(str_detect(text, "Kamala") | str_detect(text, "Harris"))
HarrisB<-Harris%>%summarize("index" = mean(sum, na.rm=TRUE), "var"=sd(mean, na.rm=TRUE), "name"="Harris")
HarrisB<-mutate(HarrisB, "volume"=dim(Harris)[1])

Buttigieg<-Z %>% 
  filter(str_detect(text, "Pete") | str_detect(text, "Buttigieg"))
ButtigiegB<-Buttigieg%>%summarize("index" = mean(sum, na.rm=TRUE), "var"=sd(mean, na.rm=TRUE), "name"="Buttigieg")
ButtigiegB<-mutate(ButtigiegB, "volume"=dim(Buttigieg)[1])

Booker<-Z%>%filter(str_detect(text, "Corey") | str_detect(text, "Booker"))
BookerB<-Booker%>%summarize("index" = mean(sum, na.rm=TRUE), "var"=sd(mean, na.rm=TRUE), "name"="Booker")
BookerB<-mutate(BookerB, "volume"=dim(Booker)[1])

#produce a dataframe with the relevant candidate data
result<-bind_rows(klobucharB, betoB, bidenB, gillibrandB, SandersB, WarrenB, ButtigiegB, BookerB, HarrisB)
#add this weeks date
result<-result%>%mutate("date"="04-09-2019")
resultB<-result%>%mutate("date"="04-02-2019")

#produce leaderboard graphic
ggplot(result, aes(date, index, size=volume, colour=name))+geom_point()

resultB<-result%>%mutate("date"="04-02-2019")
resultC<-bind_rows(result,resultB)

library(ggrepel)
ggplot(resultC, aes(date, index, size=volume, colour=name))+geom_label_repel(aes(label=name))

write.csv(resultC, "result1.csv", row.names = FALSE)
