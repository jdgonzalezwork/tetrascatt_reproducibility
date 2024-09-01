# Data and Reproducibility of the Article “TetraScatt model: Born approximation for the estimation of acoustic dispersion of fluid-like objects of arbitrary geometries.”

## Summary

This repository is for reproducing the analysis and model results of the
article *Tetrascatt model ...* whose authors are [Edmundo F.
Lavia](https://scholar.google.com/citations?user=-IUa7a0AAAAJ&hl=en),  [Guadalupe Cascallares](https://scholar.google.com/citations?user=PNstC0kAAAAJ&hl=en), and [Juan Domingo Gonzalez.](https://scholar.google.com/citations?user=NdCPiVcAAAAJ&hl=en&oi=ao).
Firstly, you should install the tetrascatt package from CRAN, by typing

    install.packages("tetrascatt")

Once you have already installed the [tetrascatt](https://cran.r-project.org/web/packages/tetrascatt/index.html), the last thing you should do in order to reproduce the results is to download this repository and run in R: 
    source("plot_fig3.R")

This command will do the following steps
-   Install the package “tetrascatt” from CRAN. This packages implements the model developed in the Ref. [1]

-   Install the necessary packages from CRAN, namely

    -   [`ggplot2`](https://cran.r-project.org/web/packages/ggplot2/index.html)
    -   [`dplyr`](https://cran.r-project.org/web/packages/dplyr/index.html)
    -   [`latex2exp`](https://cran.r-project.org/web/packages/latex2exp/index.html)
    -   [`tetrascatt`](https://cran.r-project.org/web/packages/otrimle/index.html)

-   Install the package that is not in CRAN, [`ZooScatR`](https://github.com/AustralianAntarcticDivision/ZooScatR)
    NOTE: `devtools` should be previously installed in R.

-   Upload external data such as tables including results that comes for complex and demanding models as BEM, or Spheroidal Wave functions, upload the volumetric meshes involving in the using of tetrascat model, and the auxiliary routines that aid to plot the results. Finally, if success, the program is supposed to reproduce figure 3,
4,5,6,7,8, and 10 from reference [1], for doing that you should run `source("plot_fig4.R")`,`source("plot_fig5.R")`, etc.

**References**:  
[\[1\] Lavia, E. F., Cascallares, G., & Gonzalez, J. D. (2023). TetraScatt model: Born approximation for the estimation of acoustic dispersion of fluid-like objects of arbitrary geometries. arXiv preprint arXiv:2312.16721.](https://arxiv.org/pdf/2312.16721.pdf)
