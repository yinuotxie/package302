<!-- badges: start -->
  [![Travis build status](https://travis-ci.com/yinuotxie/package302.svg?branch=master)](https://travis-ci.com/yinuotxie/package302)
  [![Codecov test coverage](https://codecov.io/gh/yinuotxie/package302/branch/master/graph/badge.svg)](https://codecov.io/gh/yinuotxie/package302?branch=master)
<!-- badges: end -->

## Installation

To download the package302 package, use the code below.

``` r
# install.packages("devtools")
devtools::install_github("yinuotxie/package302")
library(package302)
```

## Use

The vignette demonstrates example usage of all main functions. Please [file an issue](https://github.com/yinuotxie/package302/issues) if you have a request for a tutorial that is not currently included. You can see the vignette by using the following code (note that this requires a TeX installation to view properly):


``` r
# install.packages("devtools")
devtools::install_github("yinuotxie/package302", build_vignette = TRUE, build_opts = c())
library(package302)
# Use this to view the vignette in the package302 HTML help
help(package = "package302", help_type = "html")
# Use this to view the vignette as an isolated HTML file
utils::browseVignettes(package = "package302")
```
