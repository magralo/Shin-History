library(rgenoud)
library(DirichletReg)
library(abind)
library(dplyr)

rm(list=ls())
setwd("~/Desktop/refutbol")
### SHIN

SHIN <- function(PI){
  PESOS <- function(z){
    if(z>0&z<1)res<-(sum( (sqrt(z^2 + 4*(1-z)*PI^2/sum(PI))-z) /(2*(1-z)) )-1)^2 else res<-10000
    return(res)
  }
  z1<- genoud(fn=PESOS,nvars=1,Domains=c(0,1))
  res<- (sqrt(z1$par^2 + 4*(1-z1$par)*PI^2/sum(PI))-z1$par)/(2*(1-z1$par))
  return(c(res,z1$par))
}

normalize=function(prob){
  return(prob/sum(prob))
}

## Read data
odds=read.csv("odds.csv",stringsAsFactors = FALSE)
hst=read.csv("historic.csv",stringsAsFactors = FALSE)%>%
  mutate(key=paste0(Home,Away))



dir.params=data.frame()
for(i in 1:nrow(odds)){
  home=odds[i,1]
  away=odds[i,2]
  match.odds.prob=1/as.numeric(odds[i,-(1:2)])
  match.odds.prob=matrix(match.odds.prob,ncol=3,byrow=TRUE)
  shin.prob=t(apply(match.odds.prob,1,SHIN))
  shin.prob=t(apply(shin.prob[,-4],1,normalize))#sum up to almost 1 (0.99999)
  DATA <-DR_data(shin.prob)
  REG <-DirichReg(DATA~1)
  ALPHA <-REG$fitted.values$alpha[1,]
  match.dir.params=data.frame(home,away,a1=ALPHA[1],a2=ALPHA[2],a3=ALPHA[3])
  dir.params=rbind(dir.params,match.dir.params)
}

output=dir.params%>%
  mutate(key=paste0(home,away))%>%
  left_join(hst)%>%
  select(Home,Away,a1,a2,a3,p1,p2,p3)%>%
  mutate(prob.home=(a1+p1)/(a1+a2+a3+p1+p2+p3),
         prob.draw=(a2+p2)/(a1+a2+a3+p1+p2+p3),
         prob.away=(a3+p3)/(a1+a2+a3+p1+p2+p3))
  
write.csv(output,"output.csv")


