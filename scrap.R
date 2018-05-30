library(rvest)
library(dplyr)
setwd("~/Desktop/refutbol")


codes=read.csv("countryCodes.csv",stringsAsFactors = FALSE)
matchs=read.csv("odds.csv",stringsAsFactors = FALSE)%>%
  select(HomeTeam, AwayTeam)%>%
  left_join(codes,by=(c("HomeTeam"="Country")))%>%
  left_join(codes,by=(c("AwayTeam"="Country")))%>%
  mutate(link=paste0("http://www.soccerbase.com/teams/head_to_head.sd?team_id=",Code.x,"&team2_id=",Code.y))

matchs$p1=0
matchs$p2=0
matchs$p3=0
for (i in 29:nrow(matchs)){
  link=matchs$link[i]
  print(link)
text=read_html(link) %>% 
  html_nodes(".total")%>%
  html_text()%>% 
  gsub("^\\s+|\\s+$", "", .)

if(length(text)>0){
  table=read.table(text=text, sep = "\n",header = T)
  
  matchs$p1[i]=table[1,1]
  matchs$p2[i]=table[2,1]
  matchs$p3[i]=table[3,1]
}

}
matchs=matchs%>%
  select(Home,Away,p1,p2,p3)
colnames(matchs)[1:2]=c("Home","Away")
write.csv(matchs,"ScrapHistoric.csv")
