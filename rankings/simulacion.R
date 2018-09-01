####### Ranking for tipsters #######
library(dplyr)
library(lubridate)
### Simulate tipsters election ###
# I need to know, the match,  final result, odds, dummy bet/no bet,stack, tip
# dummy win lose, % profit/loss

setwd("~/Desktop/rankings")

input=read.csv('input_tipsters.csv',stringsAsFactors = FALSE)%>%
  mutate(Date=as.Date(Date,format="%d/%m/%y"))%>%
  filter(!is.na(Date))


### Simulate for one tipster

simulate_rankings=function(input,name){
  df=input%>%
    mutate(bet=as.numeric( runif(nrow(input))>0.5) ,Tip=NA, stack=runif(nrow(input),0.001,0.1),win.lose=0,p.odd=NA,profit.loss=0,Name=name)%>%
    filter(bet==1)
  for (r in 1:nrow(df)){
    probs=df[r,5:7]/sum(df[r,5:7])
    p1=probs[1]
    p2=p1+probs[2]
    alea=runif(1)
    if(alea<=p1){
      tip=1
    }else if(alea<=p2){
      tip=2
    }else{
      tip=3
    }
    df$Tip[r]=tip
    df$profit.loss[r]=-df$stack[r]
    df$p.odd[r]=df[r,(4+tip)]
    if (df$Resutado[r]==tip){
      df$win.lose[r]=1
      df$profit.loss[r]=df$stack[r]*(df$p.odd[r]-1)
    }
  }
  return(df)
}


names=c("Mateo","Andres","Alejandro","Andrea","Manuela","Juan","Sebastian","Fernando","Mr")
l.names=c("Graciano","Londono","Ramirez","Hassan","Molina","Posada","Jaramillo","Perez","Uribe","Chip")
concat=1:5
DF=data.frame()
for(n in names){
  for (a in l.names){
    for (c in concat){
      DF=rbind(DF,simulate_rankings(input,paste(n,a,c)))
    }
  }
}

DF$N.purch=rpois(nrow(DF), 10)
write.csv(select(DF,Date,Resutado,Tip,stack,p.odd,Name,N.purch),'info_necesaria.csv')
