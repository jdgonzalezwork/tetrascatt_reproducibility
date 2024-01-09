# Install the necessary packages 
packages=c("latex2exp","dplyr","tidyr","ggplot2","tetrascatt"); 
for (i in 1:length(packages)){
  
  if (packages[i] %in% rownames(installed.packages())==FALSE){ 
    install.packages(packages[i])}
  
  require(packages[i], character.only = TRUE)
}

# This is package is not included in CRAN, so 
# it should be installed by other mechanism
if ("ZooScatR" %in% rownames(installed.packages())==FALSE){
              devtools::install_github("AustralianAntarcticDivision/ZooScatR", 
                         build_vignettes = F, 
                         force_deps=TRUE)
  }