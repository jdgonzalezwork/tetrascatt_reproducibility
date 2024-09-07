source("set_the_packages.R")
library(latex2exp)
library(ggplot2)
library(dplyr)
library(tidyr)
library(ggplot2)
library(tetrascatt)
source("utils.R")

datApath=file.path("external_data","curvaA.dat")
datA=read.csv2(datApath,header = F,sep=" ")%>%as.data.frame()
datBpath=file.path("external_data","curvaB.dat")
datB=read.csv2(datBpath,header = F,sep=" ")%>%as.data.frame()
datCpath=file.path("external_data","curvaC.dat")
datC=read.csv2(datCpath,header = F,sep=" ")%>%as.data.frame()
names(datA)<-c("Frequency (kHz)","TS (dB)")
names(datB)<-c("Frequency (kHz)","TS (dB)")
names(datC)<-c("Frequency (kHz)","TS (dB)")
datA=datA%>%mutate(case="A",`TS (dB)`=as.numeric(`TS (dB)`))
datB=datB%>%mutate(case="B",`TS (dB)`=as.numeric(`TS (dB)`))
datC=datC%>%mutate(case="C",`TS (dB)`=as.numeric(`TS (dB)`))
dat=rbind(datA,datB,datC)


mip=ggplot(dat,aes(x=`Frequency (kHz)`,
               y=`TS (dB)`,
               colour=case,lty=case,size=case))+geom_line()+
  scale_colour_manual("type", values = c("black", "red","green"))+ 
  scale_size_manual("type", values = c(4.2,1,2.2), guide = "none")+
    scale_linetype_manual("type", values = c("solid", "solid","dashed")) +
    scale_size_manual("type", values = c(4.2,1,2.2), guide = "none")+
    scale_y_continuous(limits=c(-120,-87))+ 
    theme(legend.key = element_blank(),legend.title=element_blank(),text=element_text(size=41),
          legend.key.width = unit(4,"line"),legend.key.height = unit(4,"line"),
          legend.position="top",axis.title.y = element_text(angle=90,vjust=0.5))


fact=10
file_output=file.path("Figs","fig_truncated_sphere.pdf" )
pdf(file=file_output,width =2.5*fact ,height =fact)
print(mip)
dev.off()

