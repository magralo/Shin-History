#####################################################
####Model to predict Colombian football outcomes#####
#####################################################
####################Version 1.0######################
##################August 27, 2017####################
###############Andr?s Ram?rez Hassan#################
#####################################################

rm(list = ls())
#To install the library from a local folder
library(devtools)
load_all('~/Google Drive/futbol/bivpois')
library(bivpois)
library(dplyr)
setwd("~/Google Drive/futbol")

Data<-read.table("Torneo2017IFaseRegular.csv",header=TRUE,sep=",",stringsAsFactors = FALSE)
fecha=read.csv("fechaColombia.csv")%>%
  mutate(key=paste(Team1,Team2))
fechaR=read.csv("fechaColombia.csv")%>%
  mutate(key=paste(Team2,Team1))


form1 <- ~c(team1,team2)+c(team2,team1)

# Model 1: Double Poisson
ex4.m1<-lm.bp( g1~1, g2~1, l1l2=form1, zeroL3=TRUE, data=Data)
#
# Models 2-5: bivariate Poisson models
ex4.m2<-lm.bp(g1~1,g2~1, l1l2=form1, data=Data)
ex4.m3<-lm.bp(g1~1,g2~1, l1l2=form1, l3=~team1, data=Data)
ex4.m4<-lm.bp(g1~1,g2~1, l1l2=form1, l3=~team2, data=Data)
ex4.m5<-lm.bp(g1~1,g2~1, l1l2=form1, l3=~team1+team2, data=Data)
#
# Model 6: Zero Inflated Model
ex4.m6 <-lm.dibp(g1~1,g2~1, l1l2=form1, data=Data, jmax=0)
#
# Models 7-13: Diagonal Inflated Bivariate Poisson Models
ex4.m7 <-lm.dibp(g1~1,g2~1, l1l2=form1, data=Data, jmax=1)
ex4.m8 <-lm.dibp(g1~1,g2~1, l1l2=form1, data=Data, jmax=2)
ex4.m9<-lm.dibp(g1~1,g2~1, l1l2=form1, data=Data, jmax=3)
ex4.m10 <-lm.dibp(g1~1,g2~1, l1l2=form1, data=Data, jmax=4)
ex4.m11<-lm.dibp(g1~1,g2~1, l1l2=form1, data=Data, jmax=5)
ex4.m12 <-lm.dibp(g1~1,g2~1, l1l2=form1, data=Data, distribution='geometric' )
ex4.m13<-lm.dibp(g1~1,g2~1, l1l2=form1, data=Data, distribution='poisson' )
#
# Models 14: Diagonal Inflated Double Poisson Model
ex4.m14 <- lm.dibp( g1~1,g2~1, l1l2=form1, data=Data, distribution='poisson', zeroL3=TRUE )
# --------------------------------------------------------------------------


#LogLikelihood
LogLikelihood<-c(ex4.m1$loglikelihood[length(ex4.m1$loglikelihood)],ex4.m2$loglikelihood[length(ex4.m2$loglikelihood)],
ex4.m3$loglikelihood[length(ex4.m3$loglikelihood)],ex4.m4$loglikelihood[length(ex4.m4$loglikelihood)],
ex4.m5$loglikelihood[length(ex4.m5$loglikelihood)],ex4.m6$loglikelihood[length(ex4.m6$loglikelihood)],
ex4.m7$loglikelihood[length(ex4.m7$loglikelihood)],ex4.m8$loglikelihood[length(ex4.m8$loglikelihood)],
ex4.m9$loglikelihood[length(ex4.m9$loglikelihood)],ex4.m10$loglikelihood[length(ex4.m10$loglikelihood)],
ex4.m11$loglikelihood[length(ex4.m11$loglikelihood)],ex4.m12$loglikelihood[length(ex4.m12$loglikelihood)],
ex4.m13$loglikelihood[length(ex4.m13$loglikelihood)],ex4.m14$loglikelihood[length(ex4.m14$loglikelihood)])

AIC<-c(ex4.m1$AIC[length(ex4.m1$AIC)],ex4.m2$AIC[length(ex4.m2$AIC)],
       ex4.m3$AIC[length(ex4.m3$AIC)],ex4.m4$AIC[length(ex4.m4$AIC)],
       ex4.m5$AIC[length(ex4.m5$AIC)],ex4.m6$AIC[length(ex4.m6$AIC)],
       ex4.m7$AIC[length(ex4.m7$AIC)],ex4.m8$AIC[length(ex4.m8$AIC)],
       ex4.m9$AIC[length(ex4.m9$AIC)],ex4.m10$AIC[length(ex4.m10$AIC)],
       ex4.m11$AIC[length(ex4.m11$AIC)],ex4.m12$AIC[length(ex4.m12$AIC)],
       ex4.m13$AIC[length(ex4.m13$AIC)],ex4.m14$AIC[length(ex4.m14$AIC)])

BIC<-c(ex4.m1$BIC[length(ex4.m1$BIC)],ex4.m2$BIC[length(ex4.m2$BIC)],
       ex4.m3$BIC[length(ex4.m3$BIC)],ex4.m4$BIC[length(ex4.m4$BIC)],
       ex4.m5$BIC[length(ex4.m5$BIC)],ex4.m6$BIC[length(ex4.m6$BIC)],
       ex4.m7$BIC[length(ex4.m7$BIC)],ex4.m8$BIC[length(ex4.m8$BIC)],
       ex4.m9$BIC[length(ex4.m9$BIC)],ex4.m10$BIC[length(ex4.m10$BIC)],
       ex4.m11$BIC[length(ex4.m11$BIC)],ex4.m12$BIC[length(ex4.m12$BIC)],
       ex4.m13$BIC[length(ex4.m13$BIC)],ex4.m14$BIC[length(ex4.m14$BIC)])

CRITERIA<-cbind(LogLikelihood,AIC,BIC) 
#CRITERIA
# It seems that BivPois (model 2) is the best model for 2016-I based on AIC(First place) and BIC(First place)
# It seems that BivPois (model 9) is the best model for 2016-II based on AIC(First place) and BIC(Second place)
# It seems that DblPois (model 12) is the best model for 2017-I based on AIC(First place) and BIC(Second place)
# ex4.m14$lambda3
# ex4.m12$lambda2 
# ex4.m1$p
# ex4.m6$theta
# ex4.m8$theta
# ex4.m9$theta
# ex4.m10$theta

#Probabilities

InfBivPoisson<-function(x1,x2,l1,l2,l3,zeroL3=FALSE,prob=0,model=0,theta=NULL,J=NULL){
  # prob=> Probability in mixture models
  # model=> 1=poisson, 2=geometric, 3=discrete
  # theta=> Parameters in model
  # J=> Additional parameter in discrete distribution
  # x1 and x2=> goals!!!
  # l1, l2 and l3=> parameters bivariate Poisson distribution
  if (model==0){
    if (zeroL3==TRUE) {P<-dpois(x1,l1)*dpois(x2,l2)} #Model=m1
    else {P<-pbivpois(x1,x2,c(l1,l2,l3))}} #Models=m2 to m5
  else {
    ens<-function(model,x1,theta,J){
      if (model==1) {dist<-dpois(x1,theta)} #Model=m13
      else if (model==2) {dist<-dgeom(x1,theta)} #Model=m12 
      else if (model==3 & J==0 & x1==0) {dist<-1} #Model=m7
      else if (model==3 & J==0 & x1!=0) {dist<-0} #Model=m7
      else if (model==3 & J!=0 & x1<=J) #Models=m8 to m11
        {theta<-c(1-sum(theta),theta)
        dist<-theta[x1+1]}
      else dist<-0
  return(dist)
  }
  if (x1!=x2 & zeroL3==FALSE) {P<-(1-prob)*pbivpois(x1,x2,c(l1,l2,l3))}
  else if (x1!=x2 & zeroL3==TRUE) {P<-(1-prob)*dpois(x1,l1)*dpois(x2,l2)}
  else if (x1==x2 & zeroL3==FALSE) {P<-(1-prob)*pbivpois(x1,x2,c(l1,l2,l3))+prob*ens(model,x1,theta,J)}
  else P<-(1-prob)*dpois(x1,l1)*dpois(x2,l2)+prob*ens(model,x1,theta,J) 
    }
  return(P)
}

# ex4.m1$p
# ex4.m6$theta 
InfBivPoisson(x1=0,x2=0,l1=ex4.m1$lambda1[l],l2=ex4.m1$lambda2[l],l3=0,zeroL3=TRUE,prob=0,model=0,theta=NULL,J=NULL)

Result<-cbind(rep(c(0:5),6),rep(c(0:5),each=6))
PROBSm1<-NULL
Modelo1=data.frame()
for(l in 1:nrow(Data)){
  Probs<-apply(Result,1,function(x){InfBivPoisson(x1=x[1],x2=x[2],l1=ex4.m1$lambda1[l],l2=ex4.m1$lambda2[l],l3=0,zeroL3=TRUE,prob=0,model=0,theta=NULL,J=NULL)})
  Probs<-Probs/sum(Probs)
  ALL<-cbind(Result,Probs)
  add=data.frame(ALL)
  colnames(add)=c("Goals H","Goals A","Prob")
  add$Home.Team=toString(Data$team1[l])
  add$Away.Team=toString(Data$team2[l])
  add$Game=l
  Modelo1=rbind(Modelo1,add)
  Score<-ALL[which.max(ALL[,3]),]
  PROB.HOME<-sum(ifelse(Result[,1]>Result[,2],1,0)*Probs)
  PROB.AWAY<-sum(ifelse(Result[,1]<Result[,2],1,0)*Probs)
  PROB.DRAW<-sum(ifelse(Result[,1]==Result[,2],1,0)*Probs)
  PROB<-c(PROB.HOME,PROB.AWAY,PROB.DRAW,Score)
  PROBSm1<-rbind(PROBSm1,PROB)
}
colnames(PROBSm1)<-c("Prob HOME","Prob AWAY","Prob DRAW","G1","G2","PROB.SCORE")

PROBSm2<-NULL
Modelo2=data.frame()
for(l in 1:nrow(Data)){
  Probs<-apply(Result,1,function(x){InfBivPoisson(x1=x[1],x2=x[2],l1=ex4.m2$lambda1[l],l2=ex4.m2$lambda2[l],l3=ex4.m2$lambda3[l],zeroL3=FALSE,prob=0,model=0,theta=NULL,J=NULL)})
  Probs<-Probs/sum(Probs)
  ALL<-cbind(Result,Probs)
  add=data.frame(ALL)
  colnames(add)=c("Goals H","Goals A","Prob")
  add$Home.Team=toString(Data$team1[l])
  add$Away.Team=toString(Data$team2[l])
  add$Game=l
  Modelo2=rbind(Modelo2,add)
  
  Score<-ALL[which.max(ALL[,3]),]
  PROB.HOME<-sum(ifelse(Result[,1]>Result[,2],1,0)*Probs)
  PROB.AWAY<-sum(ifelse(Result[,1]<Result[,2],1,0)*Probs)
  PROB.DRAW<-sum(ifelse(Result[,1]==Result[,2],1,0)*Probs)
  PROB<-c(PROB.HOME,PROB.AWAY,PROB.DRAW,Score)
  PROBSm2<-rbind(PROBSm2,PROB)
}
colnames(PROBSm2)<-c("Prob HOME","Prob AWAY","Prob DRAW","G1","G2","PROB.SCORE")

PROBSm3<-NULL
Modelo3=data.frame()
for(l in 1:nrow(Data)){
  Probs<-apply(Result,1,function(x){InfBivPoisson(x1=x[1],x2=x[2],l1=ex4.m3$lambda1[l],l2=ex4.m3$lambda2[l],l3=ex4.m3$lambda3[l],zeroL3=FALSE,prob=0,model=0,theta=NULL,J=NULL)})
  Probs<-Probs/sum(Probs)
  ALL<-cbind(Result,Probs)
  
  add=data.frame(ALL)
  colnames(add)=c("Goals H","Goals A","Prob")
  add$Home.Team=toString(Data$team1[l])
  add$Away.Team=toString(Data$team2[l])
  add$Game=l
  Modelo3=rbind(Modelo3,add)
  Score<-ALL[which.max(ALL[,3]),]
  PROB.HOME<-sum(ifelse(Result[,1]>Result[,2],1,0)*Probs)
  PROB.AWAY<-sum(ifelse(Result[,1]<Result[,2],1,0)*Probs)
  PROB.DRAW<-sum(ifelse(Result[,1]==Result[,2],1,0)*Probs)
  PROB<-c(PROB.HOME,PROB.AWAY,PROB.DRAW,Score)
  PROBSm3<-rbind(PROBSm3,PROB)
}
colnames(PROBSm3)<-c("Prob HOME","Prob AWAY","Prob DRAW","G1","G2","PROB.SCORE")

PROBSm4<-NULL
Modelo4=data.frame()
for(l in 1:nrow(Data)){
  Probs<-apply(Result,1,function(x){InfBivPoisson(x1=x[1],x2=x[2],l1=ex4.m4$lambda1[l],l2=ex4.m4$lambda2[l],l3=ex4.m4$lambda3[l],zeroL3=FALSE,prob=0,model=0,theta=NULL,J=NULL)})
  Probs<-Probs/sum(Probs)
  ALL<-cbind(Result,Probs)
  Score<-ALL[which.max(ALL[,3]),]
  
  add=data.frame(ALL)
  colnames(add)=c("Goals H","Goals A","Prob")
  add$Home.Team=toString(Data$team1[l])
  add$Away.Team=toString(Data$team2[l])
  add$Game=l
  Modelo4=rbind(Modelo4,add)
  PROB.HOME<-sum(ifelse(Result[,1]>Result[,2],1,0)*Probs)
  PROB.AWAY<-sum(ifelse(Result[,1]<Result[,2],1,0)*Probs)
  PROB.DRAW<-sum(ifelse(Result[,1]==Result[,2],1,0)*Probs)
  PROB<-c(PROB.HOME,PROB.AWAY,PROB.DRAW,Score)
  PROBSm4<-rbind(PROBSm4,PROB)
}
colnames(PROBSm4)<-c("Prob HOME","Prob AWAY","Prob DRAW","G1","G2","PROB.SCORE")

PROBSm5<-NULL
Modelo5=data.frame()
for(l in 1:nrow(Data)){
  Probs<-apply(Result,1,function(x){InfBivPoisson(x1=x[1],x2=x[2],l1=ex4.m5$lambda1[l],l2=ex4.m5$lambda2[l],l3=ex4.m5$lambda3[l],zeroL3=FALSE,prob=0,model=0,theta=NULL,J=NULL)})
  Probs<-Probs/sum(Probs)
  ALL<-cbind(Result,Probs)
  Score<-ALL[which.max(ALL[,3]),]
  
  add=data.frame(ALL)
  colnames(add)=c("Goals H","Goals A","Prob")
  add$Home.Team=toString(Data$team1[l])
  add$Away.Team=toString(Data$team2[l])
  add$Game=l
  Modelo5=rbind(Modelo5,add)
  PROB.HOME<-sum(ifelse(Result[,1]>Result[,2],1,0)*Probs)
  PROB.AWAY<-sum(ifelse(Result[,1]<Result[,2],1,0)*Probs)
  PROB.DRAW<-sum(ifelse(Result[,1]==Result[,2],1,0)*Probs)
  PROB<-c(PROB.HOME,PROB.AWAY,PROB.DRAW,Score)
  PROBSm5<-rbind(PROBSm5,PROB)
}
colnames(PROBSm5)<-c("Prob HOME","Prob AWAY","Prob DRAW","G1","G2","PROB.SCORE")

PROBSm6<-NULL
Modelo6=data.frame()
for(l in 1:nrow(Data)){
  Probs<-apply(Result,1,function(x){InfBivPoisson(x1=x[1],x2=x[2],l1=ex4.m6$lambda1[l],l2=ex4.m6$lambda2[l],l3=ex4.m6$lambda3[l],zeroL3=FALSE,prob=ex4.m6$p,model=3,theta=ex4.m6$theta,J=0)})
  Probs<-Probs/sum(Probs)
  ALL<-cbind(Result,Probs)
  Score<-ALL[which.max(ALL[,3]),]
  
  add=data.frame(ALL)
  colnames(add)=c("Goals H","Goals A","Prob")
  add$Home.Team=toString(Data$team1[l])
  add$Away.Team=toString(Data$team2[l])
  add$Game=l
  Modelo6=rbind(Modelo6,add)
  PROB.HOME<-sum(ifelse(Result[,1]>Result[,2],1,0)*Probs)
  PROB.AWAY<-sum(ifelse(Result[,1]<Result[,2],1,0)*Probs)
  PROB.DRAW<-sum(ifelse(Result[,1]==Result[,2],1,0)*Probs)
  PROB<-c(PROB.HOME,PROB.AWAY,PROB.DRAW,Score)
  PROBSm6<-rbind(PROBSm6,PROB)
}
colnames(PROBSm6)<-c("Prob HOME","Prob AWAY","Prob DRAW","G1","G2","PROB.SCORE")

PROBSm7<-NULL
Modelo7=data.frame()
for(l in 1:nrow(Data)){
  Probs<-apply(Result,1,function(x){InfBivPoisson(x1=x[1],x2=x[2],l1=ex4.m7$lambda1[l],l2=ex4.m7$lambda2[l],l3=ex4.m7$lambda3[l],zeroL3=FALSE,prob=ex4.m7$p,model=3,theta=ex4.m7$theta,J=1)})
  Probs<-Probs/sum(Probs)
  ALL<-cbind(Result,Probs)
  Score<-ALL[which.max(ALL[,3]),]
  
  add=data.frame(ALL)
  colnames(add)=c("Goals H","Goals A","Prob")
  add$Home.Team=toString(Data$team1[l])
  add$Away.Team=toString(Data$team2[l])
  add$Game=l
  Modelo7=rbind(Modelo7,add)
  PROB.HOME<-sum(ifelse(Result[,1]>Result[,2],1,0)*Probs)
  PROB.AWAY<-sum(ifelse(Result[,1]<Result[,2],1,0)*Probs)
  PROB.DRAW<-sum(ifelse(Result[,1]==Result[,2],1,0)*Probs)
  PROB<-c(PROB.HOME,PROB.AWAY,PROB.DRAW,Score)
  PROBSm7<-rbind(PROBSm7,PROB)
}
colnames(PROBSm7)<-c("Prob HOME","Prob AWAY","Prob DRAW","G1","G2","PROB.SCORE")

PROBSm8<-NULL
Modelo8=data.frame()
for(l in 1:nrow(Data)){
  Probs<-apply(Result,1,function(x){InfBivPoisson(x1=x[1],x2=x[2],l1=ex4.m8$lambda1[l],l2=ex4.m8$lambda2[l],l3=ex4.m8$lambda3[l],zeroL3=FALSE,prob=ex4.m8$p,model=3,theta=ex4.m8$theta,J=2)})
  Probs<-Probs/sum(Probs)
  ALL<-cbind(Result,Probs)
  Score<-ALL[which.max(ALL[,3]),]
  
  add=data.frame(ALL)
  colnames(add)=c("Goals H","Goals A","Prob")
  add$Home.Team=toString(Data$team1[l])
  add$Away.Team=toString(Data$team2[l])
  add$Game=l
  Modelo8=rbind(Modelo8,add)
  
  PROB.HOME<-sum(ifelse(Result[,1]>Result[,2],1,0)*Probs)
  PROB.AWAY<-sum(ifelse(Result[,1]<Result[,2],1,0)*Probs)
  PROB.DRAW<-sum(ifelse(Result[,1]==Result[,2],1,0)*Probs)
  PROB<-c(PROB.HOME,PROB.AWAY,PROB.DRAW,Score)
  PROBSm8<-rbind(PROBSm8,PROB)
}
colnames(PROBSm8)<-c("Prob HOME","Prob AWAY","Prob DRAW","G1","G2","PROB.SCORE")

PROBSm9<-NULL
Modelo9=data.frame()
for(l in 1:nrow(Data)){
  Probs<-apply(Result,1,function(x){InfBivPoisson(x1=x[1],x2=x[2],l1=ex4.m9$lambda1[l],l2=ex4.m9$lambda2[l],l3=ex4.m9$lambda3[l],zeroL3=FALSE,prob=ex4.m9$p,model=3,theta=ex4.m9$theta,J=3)})
  Probs<-Probs/sum(Probs)
  ALL<-cbind(Result,Probs)
  Score<-ALL[which.max(ALL[,3]),]
  
  add=data.frame(ALL)
  colnames(add)=c("Goals H","Goals A","Prob")
  add$Home.Team=toString(Data$team1[l])
  add$Away.Team=toString(Data$team2[l])
  add$Game=l
  Modelo9=rbind(Modelo9,add)
  
  PROB.HOME<-sum(ifelse(Result[,1]>Result[,2],1,0)*Probs)
  PROB.AWAY<-sum(ifelse(Result[,1]<Result[,2],1,0)*Probs)
  PROB.DRAW<-sum(ifelse(Result[,1]==Result[,2],1,0)*Probs)
  PROB<-c(PROB.HOME,PROB.AWAY,PROB.DRAW,Score)
  PROBSm9<-rbind(PROBSm9,PROB)
}
colnames(PROBSm9)<-c("Prob HOME","Prob AWAY","Prob DRAW","G1","G2","PROB.SCORE")

PROBSm10<-NULL
Modelo10=data.frame()
for(l in 1:nrow(Data)){
  Probs<-apply(Result,1,function(x){InfBivPoisson(x1=x[1],x2=x[2],l1=ex4.m10$lambda1[l],l2=ex4.m10$lambda2[l],l3=ex4.m10$lambda3[l],zeroL3=FALSE,prob=ex4.m10$p,model=3,theta=ex4.m10$theta,J=4)})
  Probs<-Probs/sum(Probs)
  ALL<-cbind(Result,Probs)
  Score<-ALL[which.max(ALL[,3]),]
  
  add=data.frame(ALL)
  colnames(add)=c("Goals H","Goals A","Prob")
  add$Home.Team=toString(Data$team1[l])
  add$Away.Team=toString(Data$team2[l])
  add$Game=l
  Modelo10=rbind(Modelo10,add)
  
  PROB.HOME<-sum(ifelse(Result[,1]>Result[,2],1,0)*Probs)
  PROB.AWAY<-sum(ifelse(Result[,1]<Result[,2],1,0)*Probs)
  PROB.DRAW<-sum(ifelse(Result[,1]==Result[,2],1,0)*Probs)
  PROB<-c(PROB.HOME,PROB.AWAY,PROB.DRAW,Score)
  PROBSm10<-rbind(PROBSm10,PROB)
}
colnames(PROBSm10)<-c("Prob HOME","Prob AWAY","Prob DRAW","G1","G2","PROB.SCORE")

PROBSm11<-NULL
Modelo11=data.frame()
for(l in 1:nrow(Data)){
  Probs<-apply(Result,1,function(x){InfBivPoisson(x1=x[1],x2=x[2],l1=ex4.m11$lambda1[l],l2=ex4.m11$lambda2[l],l3=ex4.m11$lambda3[l],zeroL3=FALSE,prob=ex4.m11$p,model=3,theta=ex4.m11$theta,J=5)})
  Probs<-Probs/sum(Probs)
  ALL<-cbind(Result,Probs)
  Score<-ALL[which.max(ALL[,3]),]
  
  add=data.frame(ALL)
  colnames(add)=c("Goals H","Goals A","Prob")
  add$Home.Team=toString(Data$team1[l])
  add$Away.Team=toString(Data$team2[l])
  add$Game=l
  Modelo11=rbind(Modelo11,add)
  
  PROB.HOME<-sum(ifelse(Result[,1]>Result[,2],1,0)*Probs)
  PROB.AWAY<-sum(ifelse(Result[,1]<Result[,2],1,0)*Probs)
  PROB.DRAW<-sum(ifelse(Result[,1]==Result[,2],1,0)*Probs)
  PROB<-c(PROB.HOME,PROB.AWAY,PROB.DRAW,Score)
  PROBSm11<-rbind(PROBSm11,PROB)
}
colnames(PROBSm11)<-c("Prob HOME","Prob AWAY","Prob DRAW","G1","G2","PROB.SCORE")

PROBSm12<-NULL
Modelo12=data.frame()
for(l in 1:nrow(Data)){
  Probs<-apply(Result,1,function(x){InfBivPoisson(x1=x[1],x2=x[2],l1=ex4.m12$lambda1[l],l2=ex4.m12$lambda2[l],l3=ex4.m12$lambda3[l],zeroL3=FALSE,prob=ex4.m12$p,model=2,theta=ex4.m12$theta,J=NULL)})
  Probs<-Probs/sum(Probs)
  ALL<-cbind(Result,Probs)
  Score<-ALL[which.max(ALL[,3]),]
  
  add=data.frame(ALL)
  colnames(add)=c("Goals H","Goals A","Prob")
  add$Home.Team=toString(Data$team1[l])
  add$Away.Team=toString(Data$team2[l])
  add$Game=l
  Modelo12=rbind(Modelo12,add)
  PROB.HOME<-sum(ifelse(Result[,1]>Result[,2],1,0)*Probs)
  PROB.AWAY<-sum(ifelse(Result[,1]<Result[,2],1,0)*Probs)
  PROB.DRAW<-sum(ifelse(Result[,1]==Result[,2],1,0)*Probs)
  PROB<-c(PROB.HOME,PROB.AWAY,PROB.DRAW,Score)
  PROBSm12<-rbind(PROBSm12,PROB)
}
colnames(PROBSm12)<-c("Prob HOME","Prob AWAY","Prob DRAW","G1","G2","PROB.SCORE")

PROBSm13<-NULL
Modelo13=data.frame()
for(l in 1:nrow(Data)){
  Probs<-apply(Result,1,function(x){InfBivPoisson(x1=x[1],x2=x[2],l1=ex4.m13$lambda1[l],l2=ex4.m13$lambda2[l],l3=ex4.m13$lambda3[l],zeroL3=FALSE,prob=ex4.m13$p,model=1,theta=ex4.m13$theta,J=NULL)})
  Probs<-Probs/sum(Probs)
  ALL<-cbind(Result,Probs)
  Score<-ALL[which.max(ALL[,3]),]
  
  add=data.frame(ALL)
  colnames(add)=c("Goals H","Goals A","Prob")
  add$Home.Team=toString(Data$team1[l])
  add$Away.Team=toString(Data$team2[l])
  add$Game=l
  Modelo13=rbind(Modelo13,add)
  PROB.HOME<-sum(ifelse(Result[,1]>Result[,2],1,0)*Probs)
  PROB.AWAY<-sum(ifelse(Result[,1]<Result[,2],1,0)*Probs)
  PROB.DRAW<-sum(ifelse(Result[,1]==Result[,2],1,0)*Probs)
  PROB<-c(PROB.HOME,PROB.AWAY,PROB.DRAW,Score)
  PROBSm13<-rbind(PROBSm13,PROB)
}
colnames(PROBSm13)<-c("Prob HOME","Prob AWAY","Prob DRAW","G1","G2","PROB.SCORE")

PROBSm14<-NULL
Modelo14=data.frame()
for(l in 1:nrow(Data)){
  Probs<-apply(Result,1,function(x){InfBivPoisson(x1=x[1],x2=x[2],l1=ex4.m14$lambda1[l],l2=ex4.m14$lambda2[l],l3=ex4.m14$lambda3[l],zeroL3=TRUE,prob=ex4.m14$p,model=1,theta=ex4.m13$theta,J=NULL)})
  Probs<-Probs/sum(Probs)
  ALL<-cbind(Result,Probs)
  Score<-ALL[which.max(ALL[,3]),]
  
  add=data.frame(ALL)
  colnames(add)=c("Goals H","Goals A","Prob")
  add$Home.Team=toString(Data$team1[l])
  add$Away.Team=toString(Data$team2[l])
  add$Game=l
  Modelo14=rbind(Modelo14,add)
  
  PROB.HOME<-sum(ifelse(Result[,1]>Result[,2],1,0)*Probs)
  PROB.AWAY<-sum(ifelse(Result[,1]<Result[,2],1,0)*Probs)
  PROB.DRAW<-sum(ifelse(Result[,1]==Result[,2],1,0)*Probs)
  PROB<-c(PROB.HOME,PROB.AWAY,PROB.DRAW,Score)
  PROBSm14<-rbind(PROBSm14,PROB)
}
colnames(PROBSm14)<-c("Prob HOME","Prob AWAY","Prob DRAW","G1","G2","PROB.SCORE")

Marg<-exp(-BIC/2)
PMP<-Marg/sum(Marg)

AVER.HOME<-PMP[1]*PROBSm1[,1]+PMP[2]*PROBSm2[,1]+PMP[3]*PROBSm3[,1]+
           PMP[4]*PROBSm4[,1]+PMP[5]*PROBSm5[,1]+PMP[6]*PROBSm6[,1]+
           PMP[7]*PROBSm7[,1]+PMP[8]*PROBSm8[,1]+PMP[9]*PROBSm9[,1]+
           PMP[10]*PROBSm10[,1]+PMP[11]*PROBSm11[,1]+PMP[12]*PROBSm12[,1]+
           PMP[13]*PROBSm13[,1]+PMP[14]*PROBSm14[,1]

AVER.AWAY<-PMP[1]*PROBSm1[,2]+PMP[2]*PROBSm2[,2]+PMP[3]*PROBSm3[,2]+
           PMP[4]*PROBSm4[,2]+PMP[5]*PROBSm5[,2]+PMP[6]*PROBSm6[,2]+
           PMP[7]*PROBSm7[,2]+PMP[8]*PROBSm8[,2]+PMP[9]*PROBSm9[,2]+
           PMP[10]*PROBSm10[,2]+PMP[11]*PROBSm11[,2]+PMP[12]*PROBSm12[,2]+
           PMP[13]*PROBSm13[,2]+PMP[14]*PROBSm14[,2]

AVER.DRAW<-PMP[1]*PROBSm1[,3]+PMP[2]*PROBSm2[,3]+PMP[3]*PROBSm3[,3]+
           PMP[4]*PROBSm4[,3]+PMP[5]*PROBSm5[,3]+PMP[6]*PROBSm6[,3]+
           PMP[7]*PROBSm7[,3]+PMP[8]*PROBSm8[,3]+PMP[9]*PROBSm9[,3]+
           PMP[10]*PROBSm10[,3]+PMP[11]*PROBSm11[,3]+PMP[12]*PROBSm12[,3]+
           PMP[13]*PROBSm13[,3]+PMP[14]*PROBSm14[,3]

Modelo1$ProbBMA=Modelo1$Prob*PMP[1]
Modelo1$key=paste(Modelo1$Home.Team,Modelo1$Away.Team,Modelo1$`Goals H`,Modelo1$`Goals A`)

Modelo2$ProbBMA=Modelo2$Prob*PMP[2]
Modelo2$key=paste(Modelo2$Home.Team,Modelo2$Away.Team,Modelo2$`Goals H`,Modelo2$`Goals A`)

Modelo3$ProbBMA=Modelo3$Prob*PMP[3]
Modelo3$key=paste(Modelo3$Home.Team,Modelo3$Away.Team,Modelo3$`Goals H`,Modelo3$`Goals A`)

Modelo4$ProbBMA=Modelo4$Prob*PMP[4]
Modelo4$key=paste(Modelo4$Home.Team,Modelo4$Away.Team,Modelo4$`Goals H`,Modelo4$`Goals A`)

Modelo5$ProbBMA=Modelo5$Prob*PMP[5]
Modelo5$key=paste(Modelo5$Home.Team,Modelo5$Away.Team,Modelo5$`Goals H`,Modelo5$`Goals A`)

Modelo6$ProbBMA=Modelo6$Prob*PMP[6]
Modelo6$key=paste(Modelo6$Home.Team,Modelo6$Away.Team,Modelo6$`Goals H`,Modelo6$`Goals A`)

Modelo7$ProbBMA=Modelo7$Prob*PMP[7]
Modelo7$key=paste(Modelo7$Home.Team,Modelo7$Away.Team,Modelo7$`Goals H`,Modelo7$`Goals A`)

Modelo8$ProbBMA=Modelo8$Prob*PMP[8]
Modelo8$key=paste(Modelo8$Home.Team,Modelo8$Away.Team,Modelo8$`Goals H`,Modelo8$`Goals A`)

Modelo9$ProbBMA=Modelo9$Prob*PMP[9]
Modelo9$key=paste(Modelo9$Home.Team,Modelo9$Away.Team,Modelo9$`Goals H`,Modelo9$`Goals A`)

Modelo10$ProbBMA=Modelo10$Prob*PMP[10]
Modelo10$key=paste(Modelo10$Home.Team,Modelo10$Away.Team,Modelo10$`Goals H`,Modelo10$`Goals A`)

Modelo11$ProbBMA=Modelo11$Prob*PMP[11]
Modelo11$key=paste(Modelo11$Home.Team,Modelo11$Away.Team,Modelo11$`Goals H`,Modelo11$`Goals A`)

Modelo12$ProbBMA=Modelo12$Prob*PMP[12]
Modelo12$key=paste(Modelo12$Home.Team,Modelo12$Away.Team,Modelo12$`Goals H`,Modelo12$`Goals A`)

Modelo13$ProbBMA=Modelo13$Prob*PMP[13]
Modelo13$key=paste(Modelo13$Home.Team,Modelo13$Away.Team,Modelo13$`Goals H`,Modelo13$`Goals A`)

Modelo14$ProbBMA=Modelo14$Prob*PMP[14]
Modelo14$key=paste(Modelo14$Home.Team,Modelo14$Away.Team,Modelo14$`Goals H`,Modelo14$`Goals A`)


Modelos=rbind(Modelo1,Modelo2,Modelo3,Modelo4,Modelo5,Modelo6,Modelo7,Modelo8,Modelo9,Modelo10,Modelo11,Modelo12,Modelo13,Modelo14)

library(dplyr)

ModelosBMA=data.frame(Modelos%>%
  group_by(key)%>%
  summarise(Probability=sum(ProbBMA)))
 

ModelosBMA= left_join(ModelosBMA,Modelo1)
ModelosBMA=arrange(ModelosBMA, (Game))
ModelosBMAS=ModelosBMA[c( "Home.Team" ,"Away.Team","Goals H" ,"Goals A", "Probability" ,"Game")]


ModelosBMA$HomeWin=0
ModelosBMA$HomeWin[ModelosBMA$`Goals H`>ModelosBMA$`Goals A`]=1
ModelosBMA$HomeWin=ModelosBMA$HomeWin*ModelosBMA$Probability

ModelosBMA$AwayWin=0
ModelosBMA$AwayWin[ModelosBMA$`Goals H`<ModelosBMA$`Goals A`]=1
ModelosBMA$AwayWin=ModelosBMA$AwayWin*ModelosBMA$Probability

ModelosBMA$Draw=0
ModelosBMA$Draw[ModelosBMA$`Goals H`==ModelosBMA$`Goals A`]=1
ModelosBMA$Draw=ModelosBMA$Draw*ModelosBMA$Probability

ModelosBMAW=arrange(data.frame(ModelosBMA%>%
                        group_by(Home.Team,Away.Team)%>%
                        summarise(Home=sum(HomeWin),Draw=sum(Draw),Away=sum(AwayWin),Game=mean(Game))  ),Game)

ModelosBMAW=ModelosBMAW[c( "Home.Team" ,"Away.Team","Home" ,"Draw", "Away" )]


Scores=mutate(ModelosBMAS,key=paste(Home.Team,Away.Team))%>%
  inner_join(fecha)%>%
  select(Home.Team,Away.Team,"Goals H","Goals A",Probability)
ScoresR=mutate(ModelosBMAS,key=paste(Home.Team,Away.Team))%>%
  inner_join(fechaR)%>%
  select(Home.Team,Away.Team,"Goals H","Goals A",Probability)
Scores=rbind(Scores,ScoresR)

RedRes=mutate(ModelosBMAW,key=paste(Home.Team,Away.Team))%>%
  inner_join(fecha)%>%
  select(Home.Team,Away.Team,Home,Draw,Away )
RedResR=mutate(ModelosBMAW,key=paste(Home.Team,Away.Team))%>%
  inner_join(fechaR)%>%
  select(Home.Team,Away.Team,Home,Draw,Away )
RedRes=rbind(RedRes,RedResR)

write.csv(ModelosBMAW,"AllResults.csv")
write.csv(Scores,"longResults.csv")
write.csv(RedRes,"tidyResults.csv")




