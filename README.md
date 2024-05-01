# Exploratory Data Analysis for 'SciViews::R' <a href='https://www.sciviews.org/exploreit'><img src='man/figures/logo.png' align='right' height='139'/></a>

<!-- badges: start -->

[![R-CMD-check](https://github.com/SciViews/exploreit/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/SciViews/exploreit/actions/workflows/R-CMD-check.yaml) [![Codecov test coverage](https://codecov.io/gh/SciViews/exploreit/branch/main/graph/badge.svg)](https://codecov.io/gh/SciViews/exploreit?branch=main) [![CRAN status](https://www.r-pkg.org/badges/version/exploreit)](https://CRAN.R-project.org/package=exploreit) [![r-universe status](https://sciviews.r-universe.dev/badges/exploreit)](https://sciviews.r-universe.dev/exploreit) [![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)

<!-- badges: end -->

The {exploreit} package homogenizes the user interface to various multivariate analyses like PCA, CA, MFA, MDS, K-means, hierarchical clustering, and more to match the `SciViews::R` style.

## Installation

{exploreit} is not available from CRAN yet. You should install it from the [SciViews R-Universe](https://sciviews.r-universe.dev). {chart} is an alternate formula interface to {ggplot2}. {tabularise} produces publication-ready (rich-formatted) tabular output. The {equatags} and {equatiomatic} packages are optional, but they are useful to display equations, both inline in R Markdown/Quarto documents and in {tabularise} tables. {data.io} is useful too because it manages labels and units that {chart} uses. To install these six packages and their dependencies, run the following command in R:

``` r
install.packages(c('modelit', 'chart', 'tabularise', 'equatags', 'equatiomatic', 'data.io'),
  repos = c('https://sciviews.r-universe.dev', 'https://cloud.r-project.org'))
```

You can also install the latest development version of {exploreit}. Make sure you have the {remotes} R package installed:

``` r
# install.packages("remotes")
remotes::install_github("SciViews/exploreit")
```

## Usage

You can get further help about this package this way: Make the {exploreit} package available in your R session:

``` r
library(exploreit)
```

Get help about this package:

``` r
library(help = "exploreit")
help("exploreit-package")
vignette("exploreit") # None is installed with install_github()
```

For further instructions, please, refer to the help pages at <https://www.sciviews.org/exploreit/>.

## Code of Conduct

Please note that the {exploreit} package is released with a [Contributor Code of Conduct](https://contributor-covenant.org/version/2/1/CODE_OF_CONDUCT.html). By contributing to this project, you agree to abide by its terms.

