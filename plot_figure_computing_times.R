source("set_the_packages.R")
library(latex2exp)
library(ggplot2)
library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)
source("utils.R") 
filename=file.path("external_data","times_in_computing_born.dat")
dat=read.csv2(filename,header = F,sep=" ")%>%as.data.frame()
names(dat)<-c("time","N")
dat$time=as.numeric(dat$time)
fact=6
file_output=file.path("Figs",
                      "times_in_computing_born.pdf")

pdf(file=file_output,width =2.5*fact ,height =fact)
ggplot(dat,aes(x=N,y=time))+geom_line(lwd=1)+
  geom_point(size=6)+scale_x_continuous(breaks = seq(0,1250000,by=250000))+
  theme(text=element_text(size=30))+ylab("time (s)")+xlab("Number of tetrahedra")
dev.off()


