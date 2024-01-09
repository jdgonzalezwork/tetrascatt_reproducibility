source("set_the_packages.R")
library(latex2exp)
library(ggplot2)
library(dplyr)
library(tidyr)
library(ggplot2)
library(tetrascatt)
source("utils.R")

mesh9521=file.path("Mesh","Sphere_001_9521","Sphere_001_9521.mesh")
mesh9521=read_mesh(meshfile=mesh9521)


c0=1477.4
rho0=1026.8
c1=1480.3
rho1=1028.9
h=c1/c0
g=rho1/rho0
a=0.01;
fs=c(38,120,200,333)
datacum=c()
index=1

gg=seq(0.9,1.1,length.out=100)
hh=seq(0.9,1.1,length.out=100)
dat1=expand.grid(g=gg,h=hh)

for (index in 1:length(fs)){
  f=fs[index]*1000
  
  # The analytic solution is the accurate method for modeling the scattering 
  #of a sphere, without any approximations. 
  #The only concession is the truncation of the theoretically infinite 
  #expansion involved in the computation. This approach is based on Hickling 
  #(1950, JASA). The implementation was adopted from the ZooScatR package by 
  #Gastauer and Chu (2019)  
  fexact=function(g,h){ finf.sphere3(f, a=a, c=c0, h=h, g=g, rho=rho0) }
  
  
  # CASO Analytic Integral Born
  f_infty_born=function(g,h,mesh){finf.sphere3.exactIntegral(f, a=a, c=c0, h=h, g=g, rho=rho0)}
  
  #
  res= lapply(1:nrow(dat1), FUN=function(i){fexact(g=dat1[i,1],h=dat1[i,2])})%>%unlist()

 # born tetrascatt results 
  res9521= lapply(1:nrow(dat1), 
                  FUN= function(i){f_infty_born(g=dat1[i,1],h=dat1[i,2])}
  )%>%unlist()
  
  # error estimation (TS is a functions located in utils.R)
  error_dB= abs(TS(res) - TS(res9521))
  dat2=cbind(dat1,error_dB,Frequency_Hz=f)%>%as_tibble()
  datacum=rbind(dat2,datacum)
}


datacum=datacum%>%mutate(Frequency_kHz=paste0("f=",Frequency_Hz/1000, " (kHz)"))
level0=sort(unique(datacum$Frequency_kHz),decreasing = T)
level1=level0[c(1,4,3,2)]
datacum=datacum%>%mutate(Frequency_kHz =factor(Frequency_kHz,
                                               levels=level1,ordered = F))


jet_colors <- c(
  "#00008F", "#0000FF", "#0080FF", "#00FFFF", "#80FF80",
           "#FFFF00", "#FF8000", "#FF0000", "#800000"
)



saturation_value=5
dat3=datacum%>%mutate(error_dB=ifelse(error_dB>saturation_value,
                                      saturation_value,
                                      error_dB))

mip=ggplot(dat3,
           aes(x =g, y = h, fill = error_dB))+
  geom_raster(interpolate = T) +
  scale_fill_gradientn(colors = jet_colors,breaks = 0:5) +
  labs(title = "")+facet_wrap(~Frequency_kHz)+
  theme(axis.title.y = element_text(angle=0,vjust=0.5),
        text=element_text(size=35),
        legend.title=element_blank(),
        axis.text.x = element_text(size = 20),  # Set font size for X-axis ticks
        axis.text.y = element_text(size = 20)
  )+
  guides(fill = guide_colorbar(
    label.theme = element_text(size = 20),
    barwidth = 1.5,
    barheight =30,
  )
  )




fact=15
file_output=file.path("Figs",
                      "fig_born_errorTS_sphereTS_exactSolution_gg.pdf")

pdf(file=file_output,width =1*fact ,height =fact)
print(mip)
dev.off()

