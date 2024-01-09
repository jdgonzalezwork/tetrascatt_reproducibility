rm(list=ls())
wordforexactintegral="Analytic Born"
source("set_the_packages.R")
library(latex2exp)
library(ggplot2)
library(dplyr)
library(tidyr)
library(ggplot2)
source("utils.R")
library("tetrascatt")

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
####################### BROADSIDE INCIDENCE ###################################
###############################################################################
###############################################################################
fexact=function(f){ finf.spheroid.exactIntegral_broadside(f, a=a,e=7, c=c0, h=h, g=g, rho=rho0) }

# upload the meshes involved in the figure 

mesh5391=file.path("Mesh","Spheroid_5391","Spheroid_5391.mesh")
mesh5391=read_mesh(mesh5391)

mesh25574=file.path("Mesh","Spheroid_25574","Spheroid_25574.mesh")
mesh25574=read_mesh(mesh25574)

# compute the tetrascatt values for both meshes 
f_infty_born5391=function(f){abs(tetrascatt(parameters=list(cw=c0,g=g,h=h),freq=f,
                                                 mesh=mesh5391,
                                                 kversor=c(1,0,0))$finf)}
f_infty_born25574=function(f){abs(tetrascatt(parameters=list(cw=c0,g=g,h=h),freq=f,
                                                  mesh=mesh25574,
                                                  kversor=c(1,0,0))$finf)}

# Paste results into a data.frame.

dat_alternative=data.frame(Frequency=Frequency_Hz,
                           finf.Spheroid.N.5391= unlist(lapply(Frequency_Hz,f_infty_born5391)),
                           finf.Spheroid.N.25574=unlist(lapply(Frequency_Hz,f_infty_born25574))
)


dat=dat_alternative

# Data in Format longer for using ggplot
dat0=tidyr::pivot_longer(dat,cols =2:3 )%>%rename(case=name,finf=value)

# label with suitable names. 
dat0=dat0%>%mutate(finf=as.complex(finf), TS = 20*log10(abs(finf)),
                   case=ifelse(case == "finf.Spheroid.N.5391",
                               "Spheroid (N=5391)","Spheroid (N=25574)"),
                   finfabs=abs(finf))  

## Compute exact integral born. 

dat0exact=tibble(Frequency=dat$Frequency,finf= fexact(dat$Frequency),
                 case=wordforexactintegral,finfabs=abs(finf),
                 TS=20*log10(finfabs));

dat0=rbind(dat0exact,dat0)


# arrange the case variable for controlling the labels order
lev0=sort(unique(dat0$case));
lev1=lev0[c(1,3,2)]
dat0=dat0%>%mutate(case=factor(case,levels=lev1))

fact=10
file_output=file.path("Figs",
                      "fig_Spheroid_broadside_gg.pdf.pdf")

pdf(file=file_output,width =2.5*fact ,height =fact)

ggplot(dat0,aes(x=Frequency/1000,y=finfabs,colour=case,lty=case,size=case))+ #geom_point(size=1)+
geom_line()+labs(x="Frequency (kHz)")+ylab(TeX("$|f_\\infty|$"))+
  scale_colour_manual("type", values = c("black","red","green" )) +
  scale_linetype_manual("type", values = c("solid", "solid","dashed")) +
  scale_size_manual("type", values = c(4.2,1,2.2), guide = "none")+
  theme(legend.key = element_blank(),legend.title=element_blank(),text=element_text(size=41),
        legend.key.width = unit(4,"line"),legend.key.height = unit(4,"line"),
        legend.position="top",axis.title.y = element_text(angle=0,vjust=0.5))+
  scale_y_continuous(label=scientific_10)

dev.off()

###############################################################################
####################### END              ###################################
###############################################################################
###############################################################################



###############################################################################
####################### END-ON INCIDENCE ###################################
###############################################################################
###############################################################################
Frequency_kHz= 1:100 #(kHz)
Frequency_Hz= 1000*Frequency_kHz #(Hz)

fexact=function(f){ finf.spheroid.exactIntegral_end_on(f, a=a,e=7,
                                                       c=c0, h=h, g=g, rho=rho0) }


# upload the meshes involved in the figure 

mesh5391=file.path("Mesh","Spheroid_5391","Spheroid_5391.mesh")
mesh5391=read_mesh(mesh5391)

mesh25574=file.path("Mesh","Spheroid_25574","Spheroid_25574.mesh")
mesh25574=read_mesh(mesh25574)

# compute the tetrascatt values for both meshes 
f_infty_born5391=function(f){abs(tetrascatt(parameters=list(cw=c0,g=g,h=h),freq=f,
                                            mesh=mesh5391,
                                            kversor=c(0,0,1))$finf)}
f_infty_born25574=function(f){abs(tetrascatt(parameters=list(cw=c0,g=g,h=h),freq=f,
                                             mesh=mesh25574,
                                             kversor=c(0,0,1))$finf)}

# Paste results into a data.frame.

dat_alternative=data.frame(Frequency=Frequency_Hz,
                           finf.Spheroid.N.5391= unlist(lapply(Frequency_Hz,f_infty_born5391)),
                           finf.Spheroid.N.25574=unlist(lapply(Frequency_Hz,f_infty_born25574))
)


dat=dat_alternative

# Data in Format longer for using ggplot
dat0=tidyr::pivot_longer(dat,cols =2:3 )%>%rename(case=name,finf=value)

# label with suitable names. 
dat0=dat0%>%mutate(finf=as.complex(finf), TS = 20*log10(abs(finf)),
                   case=ifelse(case == "finf.Spheroid.N.5391",
                               "Spheroid (N=5391)","Spheroid (N=25574)"),
                   finfabs=abs(finf))  

## Compute exact integral born. 

dat0exact=tibble(Frequency=dat$Frequency,finf= fexact(dat$Frequency),
                 case=wordforexactintegral,finfabs=abs(finf),
                 TS=20*log10(finfabs));

dat0=rbind(dat0exact,dat0)


# arrange the case variable for controlling the labels order
lev0=sort(unique(dat0$case));
lev1=lev0[c(1,3,2)]
dat0=dat0%>%mutate(case=factor(case,levels=lev1))

fact=10
file_output=file.path("Figs",
                      "fig_Spheroid_end_on_gg.pdf.pdf")

pdf(file=file_output,width =2.5*fact ,height =fact)

ggplot(dat0,aes(x=Frequency/1000,y=finfabs,colour=case,lty=case,size=case))+ #geom_point(size=1)+
  geom_line()+labs(x="Frequency (kHz)")+ylab(TeX("$|f_\\infty|$"))+
  scale_colour_manual("type", values = c("black","red","green" )) +
  scale_linetype_manual("type", values = c("solid", "solid","dashed")) +
  scale_size_manual("type", values = c(4.2,1,2.2), guide = "none")+
  theme(legend.key = element_blank(),legend.title=element_blank(),text=element_text(size=41),
        legend.key.width = unit(4,"line"),legend.key.height = unit(4,"line"),
        legend.position="top",axis.title.y = element_text(angle=0,vjust=0.5))+
  scale_y_continuous(label=scientific_10)

dev.off()


  
  