wordforexactintegral="Analytic Born"
source("set_the_packages.R")
library(latex2exp)
library(ggplot2)
library(dplyr)
library(tidyr)
library(ggplot2)
source("utils.R")

library(tetrascatt)
c0=1477.4
rho0=1026.8
c1=1480.3
rho1=1028.9
h=c1/c0
g=rho1/rho0
a=0.01; 

fexact=function(f){finf.sphere3.exactIntegral(f, a=a, c=c0, h=h, g=g, rho=rho0) }
Frequency_kHz= 1:400 #(kHz)
Frequency_Hz= Frequency_kHz*1000;  #(Hz)

# Upload the meshes and define auxiliar function to compute scattering based 
# tetrascatt model 


mesh350=file.path("Mesh","Sphere_001_350","Sphere_001_350.mesh")
mesh350=read_mesh(meshfile=mesh350)


mesh9521=file.path("Mesh","Sphere_001_9521","Sphere_001_9521.mesh")
mesh9521=read_mesh(meshfile=mesh9521)

f_infty_born350=function(f,mesh1){abs(tetrascatt(parameters=list(cw=c0,g=g,h=h),freq=f,
                                      mesh=mesh350,
                                      kversor=c(1,0,0))$finf)}

f_infty_born9521=function(f,mesh1){abs(tetrascatt(parameters=list(cw=c0,g=g,h=h),freq=f,
                                             mesh=mesh9521,
                                             kversor=c(1,0,0))$finf)}


# set the data intitially, and compute scattering from tetrascatt 
dat0=data.frame(Frequency=Frequency_Hz,
           finf.Sphere.N.350= unlist(lapply(Frequency_Hz,f_infty_born350)),
           finf.Sphere.N.9521=unlist(lapply(Frequency_Hz,f_infty_born9521))
           )

# Compute scattering from  Born through the exact integral 
fexact=lapply(Frequency_Hz,fexact)%>%unlist(); 

# tidy the data to format longer, for further use of ggplot 
dat0=tidyr::pivot_longer(dat0,cols =2:3 )%>%
  rename(case=name,finf=value)%>%
  filter(Frequency!=0)

# change the label for the size of the mesh in each case. 
dat0=dat0%>%mutate(finf=as.complex(finf), TS = 20*log10(abs(finf)),
                   case=ifelse(case == "finf.Sphere.N.350",
                               "Sphere (N=350)","Sphere (N=9521)"),
                   finfabs=abs(finf))  

# struct for the exact integral, in format longer,  for ggplot. 
dat0exact=tibble(Frequency=Frequency_Hz,finf=fexact,
                 case=wordforexactintegral,finfabs=abs(finf),
                 TS=20*log10(finfabs));
dat0exact=dat0exact[-1,]

# add tetrascat model results, and analytic Born results in the same data frame.
dat0=rbind(dat0exact,dat0)
unique(dat0$case)

fact=10
file_output=file.path("Figs",
                      "fig_Spheres_integral_born_validation_gg.pdf")

pdf(file=file_output,width =2.5*fact ,height =fact)

ggplot(dat0,aes(x=Frequency/1000,y=finfabs,colour=case,lty=case,size=case))+ #geom_point(size=1)+
  geom_line()+labs(x="Frequency (kHz)")+ylab(TeX("$|f_\\infty|$"))+
  scale_colour_manual("type", values = c("black", "red","green")) +
  scale_linetype_manual("type", values = c("solid", "solid","dashed")) +
  scale_size_manual("type", values = c(4.2,1,2.2), guide = "none")+
  theme(legend.key = element_blank(),legend.title=element_blank(),text=element_text(size=41),
        legend.key.width = unit(4,"line"),legend.key.height = unit(4,"line"),
        legend.position="top",axis.title.y = element_text(angle=0,vjust=0.5))+
  scale_y_continuous(label=scientific_10)

dev.off()
