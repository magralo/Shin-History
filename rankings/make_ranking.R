
DF=read.csv('info_necesaria.csv')%>%
  select(Date,Resutado,Tip,stack,p.odd,Name,N.purch)

DF=DF%>%
  mutate(Date=as.Date(Date),win.lose=as.numeric(Resutado==Tip),profit.loss=-stack+win.lose*p.odd*stack)

ecdf_fun <- function(x,perc) ecdf(x)(perc)
get.perc=function(vec){
  #get a vector and return a vector of the same length where each value is the percentile where
  # the value is inside the original vector v[i]= percentile(vec[i],vec)
  ans=vec
  for(i in 1:length(ans)){
    ans[i]=ecdf_fun(vec,vec[i])
  }
  return(ans)
}

tres.meses=DF%>%
  filter(Date>max(Date)%m+%months(-3))%>%
  group_by(Name)%>%
  summarise(number.bets=length(win.lose),percentage.win=mean(win.lose),profit.loss=prod(profit.loss+1)-1,N.purch=sum(N.purch))%>%
  ungroup()%>%
  mutate(rank.profit=get.perc(profit.loss),rank.npurc=get.perc(N.purch),puntaje_total=rank.profit+rank.npurc,rank_total=round(get.perc(puntaje_total)*100))%>%
  arrange(desc(rank_total))
write.csv(tres.meses,'trimestral.csv')



seis.meses=DF%>%
  filter(Date>max(Date)%m+%months(-6))%>%
  group_by(Name)%>%
  summarise(number.bets=length(win.lose),percentage.win=mean(win.lose),profit.loss=prod(profit.loss+1)-1,N.purch=sum(N.purch))%>%
  ungroup()%>%
  mutate(rank.profit=get.perc(profit.loss),rank.npurc=get.perc(N.purch),puntaje_total=rank.profit+rank.npurc,rank_total=round(get.perc(puntaje_total)*100))%>%
  arrange(desc(rank_total))

write.csv(seis.meses,'semestral.csv')

doce.meses=DF%>%
  filter(Date>max(Date)%m+%months(-12))%>%
  group_by(Name)%>%
  summarise(number.bets=length(win.lose),percentage.win=mean(win.lose),profit.loss=prod(profit.loss+1)-1,N.purch=sum(N.purch))%>%
  ungroup()%>%
  mutate(rank.profit=get.perc(profit.loss),rank.npurc=get.perc(N.purch),puntaje_total=rank.profit+rank.npurc,rank_total=round(get.perc(puntaje_total)*100))%>%
  arrange(desc(rank_total))

write.csv(doce.meses,'anual.csv')



all=DF%>%
  group_by(Name)%>%
  summarise(number.bets=length(win.lose),percentage.win=mean(win.lose),profit.loss=prod(profit.loss+1)-1,N.purch=sum(N.purch))%>%
  ungroup()%>%
  mutate(rank.profit=get.perc(profit.loss),rank.npurc=get.perc(N.purch),puntaje_total=rank.profit+rank.npurc,rank_total=round(get.perc(puntaje_total)*100))%>%
  arrange(desc(rank_total))

write.csv(all,'allhist.csv')



