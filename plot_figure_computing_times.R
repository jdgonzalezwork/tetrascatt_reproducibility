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
ggplot(dat,aes(x=N,y=time))+geom_line(lwd=1)+
  geom_point(size=6)+scale_x_log10(labels = label_log(digits = 2))+
  theme(text=element_text(size=30))+ylab("time (s)")+xlab("Number of tetrahedra")




