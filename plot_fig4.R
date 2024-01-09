wordforexactintegral="Analytic Born"
source("set_the_packages.R")
library(latex2exp)
library(ggplot2)
library(dplyr)
library(tidyr)
library(ggplot2)
library(tetrascatt)
source("utils.R")

c0=1477.4
rho0=1026.8
c1=1480.3
rho1=1028.9
h=c1/c0
#
g=0.96
a=0.01;
f=300*1000;
fexact=function(g,h){ finf.sphere3.exactIntegral(f, a=a, c=c0, h=h, g=g, rho=rho0) }

# Data of the Domain for the heat map
## 
gg=seq(0.9,1.1,length.out=25)
hh=seq(0.9,1.1,length.out=200)
dat1=expand.grid(g=gg,h=hh)

# upload the meshes involved in the figure 

mesh350=file.path("Mesh","Sphere_001_350","Sphere_001_350.mesh")
mesh350=read_mesh(meshfile=mesh350)


mesh9521=file.path("Mesh","Sphere_001_9521","Sphere_001_9521.mesh")
mesh9521=read_mesh(meshfile=mesh9521)

# define the born function fo g,h domain
f_infty_born=function(g,h,mesh1){tetrascatt(parameters=list(cw=c0,g=g,h=h),freq=f,
                                      mesh=mesh1,
                                      kversor=c(1,0,0))}

# compute scattering for the coarser Sphere
finf.Sphere.N.350  = lapply(hh, 
                          FUN=function(t){f_infty_born(g=g,h=t,mesh1=mesh350)$finf}
                          )%>%unlist()%>%abs()
# compute scattering for the finner Sphere

finf.Sphere.N.9521 = lapply(hh, 
                           FUN=function(t){f_infty_born(g=g,h=t,mesh1=mesh9521)$finf}
                           )%>%unlist()%>%abs()
# compute scattering for the exact integral Born Approximation
# along the line g=constant.
f_exact_h = lapply(hh, FUN=function(t){fexact(g=g,h=t)})%>%unlist()%>%abs()

# paste the three results
dat=tibble(h=hh,
           finf.Sphere.N.350=finf.Sphere.N.350,
           finf.Sphere.N.9521=finf.Sphere.N.9521,
           f_exact_h=f_exact_h);

# Format longer for using ggplot
dat0=tidyr::pivot_longer(dat,cols =2:4 )%>%rename(case=name,finf=value)

# change some labels
dat0=dat0%>%mutate(TS = 20*log10(abs(finf)),
                   case=ifelse(case == "finf.Sphere.N.350",
                               "Sphere (N=350)",case),
                   case=ifelse(case == "finf.Sphere.N.9521",
                               "Sphere (N=9521)",case),
                   case=ifelse(case == "f_exact_h",
                               wordforexactintegral,case),
                   finfabs=abs(finf))



fact=10
file_output=file.path("Figs",
                      "fig_error_born_heatmap_detalle_gg.pdf")
pdf(file=file_output,width =2.5*fact ,height =fact)
mip0=ggplot(dat0,aes(x=h,y=finfabs,colour=case,lty=case,size=case))+ #geom_point(size=1)+
  geom_line()+labs(x="h")+ylab(TeX("$|f_\\infty|$"))+
  scale_colour_manual("type", values = c("black", "red","green")) +
  scale_linetype_manual("type", values = c("solid", "solid","dashed")) +
  scale_size_manual("type", values = 1*c(4.2,1,1.2), guide = "none")+
  theme(legend.key = element_blank(),legend.title=element_blank(),text=element_text(size=51),
        legend.key.width = unit(4,"line"),legend.key.height = unit(4,"line"),
        legend.position="top",axis.title.y = element_text(angle=0,vjust=0.5))+
  scale_y_continuous(label=scientific_10)+ 
  guides(#color = guide_legend(override.aes = list(size = 1)),
         linetype = guide_legend(override.aes = list(size = c(4.2,1,1.2)))
)
print(mip0)
dev.off()




### Heat map

# compute the exact integral solution for the hg grid
res= lapply(1:nrow(dat1), FUN=function(i){fexact(g=dat1[i,1],h=dat1[i,2])})%>%unlist()

# compute the exact integral solution for the coarser mesh
res350= lapply(1:nrow(dat1), 
            FUN= function(i){f_infty_born(g=dat1[i,1],h=dat1[i,2],mesh1=mesh350)$finf}
            )%>%unlist()

# compute the exact integral solution for the finner mesh
res9521= lapply(1:nrow(dat1), 
               FUN= function(i){f_infty_born(g=dat1[i,1],h=dat1[i,2],mesh1=mesh9521)$finf}
               )%>%unlist()

error_rel350= abs((res - res350))/(abs(res))
error_rel9521= abs((res - res9521))/(abs(res))

dat2a=cbind(dat1,relative_error=error_rel9521,case="Sphere (N=9521)")%>%as_tibble()
dat2b=cbind(dat1,relative_error=error_rel350,case="Sphere (N=350)")%>%as_tibble()
dat2=rbind(dat2a,dat2b)

# Define Jet color scales like OCTAVE/Matlab
jet_colors <- c(
  "#00008F", "#0000FF", "#0080FF", "#00FFFF", "#80FF80",
  "#FFFF00", "#FF8000", "#FF0000", "#800000"
)

# saturation value for relative error
saturation_value=1

dat3=dat2%>%mutate(relative_error=ifelse(relative_error>saturation_value,
                                   saturation_value,
                                   relative_error))#%>%filter(abs(gamm)>0.01)

mip=ggplot(dat3,
       aes(x =g, y = h, fill = relative_error))+
  geom_raster(interpolate = F) +
  scale_fill_gradientn(colors = jet_colors,breaks = c(0.0,0.25, 0.5,0.75, 1)) +
  labs(title = "")+facet_wrap(~case)+
  theme(axis.title.y = element_text(angle=0,vjust=0.5),
        text=element_text(size=51),
        legend.title=element_blank())+
  guides(fill = guide_colorbar(
  label.theme = element_text(size = 20),
    barwidth = 1.5,
    barheight =15,
    )
  )


mip 

#print the result in a pdf file 
fact=10
file_output=file.path("Figs", "fig_error_born_heatmap_gg.pdf")
  pdf(file=file_output,width =2.5*fact ,height =fact)
  print(mip)
dev.off()

