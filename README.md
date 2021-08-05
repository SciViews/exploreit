
# exploreit <a href='https://www.sciviews.org/exploreit'><img src='man/figures/logo.png' align="right" height="139" /></a>

<!-- badges: start -->
[![R-CMD-check](https://github.com/SciViews/exploreit/workflows/R-CMD-check/badge.svg)](https://github.com/SciViews/exploreit/actions)
[![Codecov test coverage](https://codecov.io/gh/SciViews/exploreit/branch/master/graph/badge.svg)](https://codecov.io/gh/SciViews/exploreit?branch=master)
[![CRAN status](https://www.r-pkg.org/badges/version/exploreit)](https://CRAN.R-project.org/package=exploreit)
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

The {exploreit} package homogenizes the user interface to various multivariate analyses like PCA, MDS, K-means, hierarchical clustering, and more to match the SciViews::R style.

## Installation

You can install the released version of {exploreit} from [CRAN](https://CRAN.R-project.org) with (note: not yet!):

``` r
install.packages("exploreit")
```

You can also install the latest development version. Make sure you have the {remotes} R package installed:

```r
install.packages("remotes")
```

Use `install_github()` to install the {exploreit} package from GitHub (source from **master** branch will be recompiled on your machine):

```r
remotes::install_github("SciViews/exploreit")
```

R should install all required dependencies automatically, and then it should compile and install {exploreit}.


## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(exploreit)
## basic example code
```

For further instructions, please, refer to the help pages at https://www.sciviews.org/exploreit/.

## Code of Conduct

Please note that the exploreit project is released with a [Contributor Code of Conduct](https://contributor-covenant.org/version/2/0/CODE_OF_CONDUCT.html). By contributing to this project, you agree to abide by its terms.