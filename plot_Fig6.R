source("set_the_packages.R")
library(latex2exp)
library(ggplot2)
library(dplyr)
library(tidyr)
library(ggplot2)
library(tetrascatt)
source("utils.R")
mesh2110=file.path("Mesh","Copepod_2110","Copepod_2110.mesh")
mesh2110=read_mesh(mesh2110)

mesh95291=file.path("Mesh","Copepod_95291","Copepod_95291.mesh")
mesh95291=read_mesh(mesh95291)

c0=1477.4
rho0=1026.8
c1=1480.3
rho1=1028.9
h=c1/c0
g=rho1/rho0
a=0.01; 
Frequency_kHz= 1:400 #(kHz)
Frequency_Hz= 1000*Frequency_kHz #(Hz)
###############################################################################
###############################################################################

# the original mesh has length equal to 1 along its maximum axis.
# the following two lines are for rescaling the entering mesh to a value = 0.015 m; 
mesh2110$vertex = mesh2110$vertex*0.015
mesh95291$vertex = mesh95291$vertex*0.015 

# define the function to compute with the coarser mesh 
f_infty_born2110=function(f,mesh1){abs(tetrascatt(parameters=list(cw=c0,g=g,h=h),freq=f,
                                                  mesh=mesh2110,
                                                  kversor=c(-1,0,0))$finf)}
# define the function to compute with the finner mesh 
f_infty_born95291=function(f,mesh1){abs(tetrascatt(parameters=list(cw=c0,g=g,h=h),freq=f,
                                                   mesh=mesh95291,
                                                   kversor=c(-1,0,0))$finf)}

#paste the values into a data frame 
dat_alternative=data.frame(Frequency=Frequency_Hz,
                           finf.cop.N.2110  = unlist(lapply(Frequency_Hz,f_infty_born2110)),
                           finf.cop.N.95291 = unlist(lapply(Frequency_Hz,f_infty_born95291))
)


# give the data frame suitable structure to use ggplot functionality.  
dat=dat_alternative
dat0=tidyr::pivot_longer(dat,cols =2:3 )%>%rename(case=name,finf=value)
dat0=dat0%>%mutate(finf=as.complex(finf), TS = 20*log10(abs(finf)),
                   case=ifelse(case == "finf.cop.N.2110",
                               "Copepod (N=2110)","Copepod (N=95291)"),
                   finfabs=abs(finf))  

# External data from Boundary Element Method
# based on Gonzalez, J. D., Lavia, E. F., Blanc, S., Maas, M., & Madirolas, A. (2020). 
# Boundary element method to analyze acoustic scattering from a coupled 
# swimbladder-fish body configuration. Journal of Sound and Vibration, 486, 115609.

# load data
file_BEM=file.path("external_data","s0_bem.dat")
BEMdata=read.csv2(file_BEM,sep=" ")
# give proper format
datosExternosBEM=c()
datosExternosBEM$Frequency = as.numeric(BEMdata$f)
datosExternosBEM$case="BEM"
datosExternosBEM$finf=as.numeric(BEMdata$finf)
datosExternosBEM$finfabs=abs(as.numeric(BEMdata$finf))
datosExternosBEM$TS=20*log10(abs(datosExternosBEM$finf))

# The different models are part of the same data frame.
dat0=rbind(dat0,as_tibble(datosExternosBEM))

#plot the results

mip=ggplot(dat0,aes(x=Frequency/1000,y=finfabs,colour=case,lty=case,size=case))+ 
  geom_line()+labs(x="Frequency (kHz)")+ylab(TeX("$|f_\\infty|$"))+
  scale_colour_manual("type", values = c("black", "red","green")) +
  scale_linetype_manual("type", values = c("solid", "solid","dashed")) +
  scale_size_manual("type", values = c(4.2,1,2.2), guide = "none")+
  theme(legend.key = element_blank(),legend.title=element_blank(),text=element_text(size=41),
        legend.key.width = unit(4,"line"),legend.key.height = unit(4,"line"),
        legend.position="top",axis.title.y = element_text(angle=0,vjust=0.5))+
  scale_y_continuous(label=scientific_10)


fact=10
file_output=file.path("Figs","fig_copepod_gg.pdf" )
pdf(file=file_output,width =2.5*fact ,height =fact)
  print(mip)
dev.off()

