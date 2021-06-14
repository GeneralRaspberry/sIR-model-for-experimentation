library("dplyr")
library("ggplot2")
library("reshape2")


infected<-1
hosts<-1000
t<-0
I<-infected/hosts
S<-1-I
R<-1-I-S
beta<-1
gammy<-.5


simodeldf<-data.frame(Susceptible=S,Infected=I,Removed=R, Time=t)

while(t<1000){
  
  ds<- -beta*I*S
  di<- beta*I*S - I*gammy
  dr<- I * gammy
  
  
  I<-I + di
  S <-S + ds
  R <-R + dr
  
  
  t<-t+1
  
  simodeldf<-rbind(simodeldf,data.frame(Susceptible=S,Infected=I,Removed=R, Time=t))
} 


tablemelt<-melt(simodeldf,id.vars="Time")

ggplot(tablemelt)+geom_line(aes(x=Time,y=value,group=variable, colour=variable))