# This is for scientific number style
library(ZooScatR)
scientific_10 <- function(x) {
  Z= x==0
  ret=parse(text=gsub("e", " %*% 10^", scales::scientific_format()(x)))
  ret[Z]="0"
  ret
}

# This is for computing exact results 
# The exact implementation was taken from ZooscatR package.
# Mainly we use the Bessel function  and the 
# Hickling modal solution implementation from that package. 
# The only change we did was the ouput is now a complex number
# the previous package gave absolute values and Target Strength only.

# ZooScat must be previously installed 
# For example by running: 
# devtools::install_github("AustralianAntarcticDivision/ZooScatR", build_vignettes = F, force_deps=TRUE)

finf.sphere3=function (f, a, c, h, g, rho) 
{
  k0 <- k(c, f)
  ka <- k0 * a
  rho_c <- rho * c
  k1 <- k(h * c, f)
  k1a <- k1 * a
  Cn_fun <- function(n) {
    ((Sbessj_p(k1a, n) * Sneum(ka, n))/(Sbessj(k1a, n) *Sbessj_p(ka, n)) - 
       g * h * (Sneum_p(ka, n)/Sbessj_p(ka,   n)))/((Sbessj_p(k1a, n) * Sbessj(ka, n))/(Sbessj(k1a, n) * Sbessj_p(ka, n)) - g * h)
  }
  n <- seq(0, round(ka + 60, 0))
  Cn <- apply(matrix(n), 1, Cn_fun)
  A <- -1/(1 + (0+1i) * Cn)
  fbs <- -(0+1i)/k0 * sum((-1)^n * (2 * n + 1) * A)
  return(fbs)
}

deblank=function(t){gsub(" ","",t)}






#Spherical Bessel function definition
jesf <- function(x, n) {
  ret <- (sqrt(pi/(2*x)))*besselJ(x,n+1/2)
  return(ret)
}


finf.sphere3.exactIntegral=function (f, a, c, h, g, rho){
    # Inciidence angle theta that correspons to  
    # backscatering is 180 deg or pi. 
    theta=pi    
    gk=(1-g*h*h)/(g*h*h)
    gp=(g-1)/g
    k=2*pi/(c/f);
    k1=k/h
    #
    # 
    # The exact integral for DWBA is 
    res=(a^3)*(k1^2)*(gk*h^2+gp*cos(theta))*jesf(2*a*k1*sin(theta/2),1)/(2*a*k1*sin(theta/2))
    # if we want TS: 
    #ts<-c(ts,10*log(Mod(res)^2,10))
    res
}


finf.spheroid.exactIntegral=function (f, a, c, h, g, rho,e){
  # Scattering by computing the exact integral when the 
  # scatterer is an spheroid. 
  # a: minor axis 
  # e: b/a, where b is the major axis. 
  theta=pi    
  gk=(1-g*h*h)/(g*h*h)
  gp=(g-1)/g
  k=2*pi/(c/f);
  k1=k/h
  #Compute the 
  res=(a^3)*(k1^2)*e*(gk*h^2+gp*cos(theta))*jesf(2*a*k1*sin(theta/2),1)/(2*a*k1*sin(theta/2))
  res
}


finf.spheroid.exactIntegral_end_on=function (f, a, c, h, g, rho,e){
  #angulo de incidencia (backscatering=180)
  # a: minor axis 
  # e: b/a, where b is the major axis. 
  b= e*a; 
  theta=pi    
  gk=(1-g*h*h)/(g*h*h)
  gp=(g-1)/g
  k=2*pi/(c/f);
  k1=k/h
  #calculo el resultado de la integral spheroid
  arg=2*b*k1*sin(theta/2);
  res=(a^3)*(k1^2)*e*(gk*h^2+gp*cos(theta))*jesf(arg,1)/arg
  res
}








TS<-function(x){20*log10(abs(x))}

finf.spheroid.exactIntegral_broadside=function(f, a, c, h, g, rho,e){
  #angulo de incidencia (backscatering=180)
  # a: minor axis 
  # e: b/a, where b is the major axis. 
  theta=pi/2 
  #calculo el resultado de la integral spheroid
  res=finf_dwba_angular_fluid_spheroid_aux(f=f,c=c,g=g,h=h,a=a,b=e*a,theta_inc=theta,
                                           theta_obs=pi-theta,phi_inc=pi,phi_obs=0,
                                           version="Chu")
  res
}


finf_dwba_angular_fluid_spheroid_aux = function(f, c, g, h, a, b, theta_inc, theta_obs,
                                             phi_inc, phi_obs,version="Jones" ){ 
  k0=k(c,f)
  k1 = k0 / h ;
  ar = b / a ;
  # Prevaluar
  stinc = sin( theta_inc ) 
  stobs = sin( theta_obs ) 
  ctinc = cos( theta_inc ) 
  ctobs = cos( theta_obs ) 
  cpincpobs = cos( phi_inc - phi_obs ) 
  mur2 = stinc * ( stinc - stobs * cpincpobs ) + stobs * ( stobs - stinc * cpincpobs ) 
  muz2 = ( ctinc - ctobs )^2 
  arg = k1 * a * sqrt( mur2 + ar^2 * muz2 )
  gamma_k = ( 1 - g * h^2 ) / ( g * h^2 )
  gamma_rho = ( g - 1 ) / g 
  cosTheton = ctinc * ctobs + stinc * stobs * cpincpobs ;
  
  if (version == "Jones"){
    ret=  k0^2 * a^3 * ar * ( gamma_k + gamma_rho * cosTheton ) *
    #  Sbessj( 1, arg ) / arg 
    jesf(arg,1)/arg
    
  }
  
  if ( version == "Chu") {
    ret=k1^2 * a^3 * ar * ( h^2 * gamma_k + gamma_rho * cosTheton ) *
      jesf(arg,1)/arg ;
  }
  
}




finf.spheroid.exactIntegral_end_on_vieja=function (f, a, c, h, g, rho,e){
  #angulo de incidencia (backscatering=180)
  # a: minor axis 
  # e: b/a, where b is the major axis. 
  theta=0;
  #calculo el resultado de la integral spheroid
  res=finf_dwba_angular_fluid_spheroid_aux(f=f,c=c,g=g,h=h,a=a,b=e*a,theta_inc=theta,
                                           theta_obs=pi-theta,phi_inc=pi,phi_obs=0,
                                           version="Chu")
  res
}



mygamma=function (h, g){
  #angulo de incidencia (backscatering=180)
  theta=pi    
  gk=(1-g*h*h)/(g*h*h)
  gp=(g-1)/g
  res=(gk+gp*cos(theta))
  res
}



# finf.sphere3 = function (f, a, c, h, g, rho){ 
#   theta = pi
#   k0 <- k(c, f)
#   #kr <- k0 * r
#   ka <- k0 * a
#   rho_c <- rho * c
#   k1 <- k(h * c, f)
#   k1a <- k1 * a
#   cosTheta <- cos(theta)
#   ConvLim <- 1e-10
#   MaxCount <-round(ka + 60, 0)
#   m <- 0
#   count = 1
#   done = 0
#   
#   while (done == 0) {
#   #  Jm_kr <- Sbessj(kr, m)
#     Jm_ka <- Sbessj(ka, m)
#     Jm_k1a <- Sbessj(k1a, m)
#    # Nm_kr <- Sneum(kr, m)
#     Nm_ka <- Sneum(ka, m)
#     Alpham_k1a <- (2 * m + 1) * Sbessj_p(k1a, m)
#     Alpham_ka <- (2 * m + 1) * Sbessj_p(ka, m)
#     Betam_ka <- (2 * m + 1) * Sneum_p(ka, m)
#     #Alpham_kr <- (2 * m + 1) * Sbessj_p(kr, m)
#     #Betam_kr <- (2 * m + 1) * Sneum_p(kr, m)
#     Pm <- LegPoly(m, cosTheta)
#     Cm <- ((Alpham_k1a/Alpham_ka) * (Nm_ka/Jm_k1a) - (Betam_ka/Alpham_ka) * 
#              g * h)/((Alpham_k1a/Alpham_ka) * (Jm_ka/Jm_k1a) - 
#                        g)
#     #Am <- -((-(0+1i))^m) * (2 * m + 1)/(1 + (0+1i) * Cm)
#     #  p.sm <- -(((-(0+1i))^m) * (2 * m + 1)/(1 + (0+1i) * Cm)) *  Pm * (Jm_kr + (0+1i) * Nm_kr)
#     #p.im <- ((-(0+1i))^m) * (2 * m + 1) * Pm * Jm_kr
#     TS.pm <- ((-1)^m) * (2 * m + 1)/(1 + (0+1i) * Cm)
#     if (m == 0) {
#       TS = TS.pm
#     }
#     else {
#       TS = TS + TS.pm
#     }
#     m <- m + 1
#     count = count + 1
#     #if (max(abs(p.sm)) < ConvLim) {
#     #  done = 1
#     #  message("Converged")
#     #}
#     if (count > MaxCount) {
#       done = 1
#     #  message("Convergence failed")
#     }
#   }
#   fbs= (a/ka)*TS 
#   return(fbs)
#   
# #  TS <- (2/ka)^2 * abs(TS)^2 * (pi * a * a) = 4pia^2/(a^2 k^2) sum^2 => sum/4pi = (1/k)^2 sum^2 
# #  TS <- 10 * log10(TS/(4 * pi))
#   #return(TS)
# }





