
<!-- README.md is generated from README.Rmd. Please edit that file -->

# dySEM <img src="man/figures/logo.png" align="right" />

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://www.tidyverse.org/lifecycle/#maturing)
[![CRAN
status](https://www.r-pkg.org/badges/version/dySEM)](https://CRAN.R-project.org/package=dySEM)
<!-- badges: end -->

`dySEM` helps automate the process of scripting, fitting, and reporting
on latent models of dyadic data via [`lavaan`](http://lavaan.ugent.be).

The package currently provides functionality regarding the following
types of latent dyadic data models:

1.  **Dyadic Confirmatory Factor Analysis**

<!-- end list -->

  - Configural Invariance
  - Loading Invariance
  - Intercept Invariance
  - Residual Invariance
  - Fully Indistinguishable (+ Adjusted Fit Measures)
  - Dyadic Invariance Model Comparisons and Effect Size Computation

<!-- end list -->

2.  **Latent Actor-Partner Interdependence Models**

<!-- end list -->

  - Configural Invariance
  - Loading Invariance
  - Intercept Invariance
  - Comparison of Structural Actor and Partner Paths
  - Comparison of Latent Means

<!-- end list -->

3.  **Latent Common Fate Models**

<!-- end list -->

  - Configural Invariance
  - Loading Invariance
  - Intercept Invariance

The package was developed and used in the course of the research
described in [Sakaluk, Fisher, & Kilshaw](https://psyarxiv.com/9vcnz/)
(in press). Our goals for future functionality in `dysem` include adding
support for longitudinal dyadic SEM models, dyadic SEM simulations, and
data restructuring. Please get in touch with John if you are interested
in contributing to the package–all nice humans with a knack for R/dyadic
data analysis/SEM are welcome\!

The `dySEM` logo was designed by Wolfgang Deranleau.

## Installation

You can install the development version from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("jsakaluk/dySEM")
```

## dySEM Workflow

`dySEM` is able to automate much of the process of conducting latent
dyadic data analysis because of predictable (albeit imperfect) patterns
in variable naming, and the repetitious nature of
[`lavaan`](http://lavaan.ugent.be) syntax. For `dySEM`, all you need is
a clean dyad-structured data file, and indicator/manifest variables that
follow a re-occuring naming pattern consiting of a shared item stem,
unique item numbers, and a distinguishing character that indicates to
which dyad partner the variable applies; the names may or may not
include delimeters to separate elements of the name (e.g., ecr1\_a,
ecr2\_a, …ecr10\_a, ecr1\_b, ecr2\_b, …ecr10\_b).

A full vignette paper is in the works, but at a glance, the typical
`dySEM` workflow is as follows:

1.  Use `dvn2` (short for the second rendition of the Dyad Variable
    Names function) to extract variable names of the indicators for your
    latent variable(s). All you need to provide are the item stem(s) for
    your latent X and/or Y variable, dilimeter(s) (if any), and the
    distinguishing character that indicate which variables are from
    which partner. The `x_order` and/or `y_order` arguments are used to
    indicate whether your variable names follow the order of (s)tem,
    (i)tem, (p)artner (“sip”), or (s)tem, (p)artner, (i)tem (“spi”). The
    resulting `dvn` object should contain all that is needed for the
    rest of the packge to work, and its just a simple list of variable
    names, items (per dyad member), and dyad member distinguishing
    characters for latent X and Y.

<!-- end list -->

``` r
#Four items from the PRQC in the DRES data set (Raposo & Muise, under review) from each dyad member
dat = DRES %>% 
  select(., PRQC_1.1:PRQC_4.1, PRQC_1.2:PRQC_4.2)

#Extract item info with dvn2
dvn <- dvn2(dat, x_order = "sip", x_stem = "PRQC", x_delim1="_", x_delim2 = ".", distinguish_1="1", distinguish_2="2")
dvn
#> [[1]]
#> [1] "PRQC_1.1" "PRQC_2.1" "PRQC_3.1" "PRQC_4.1"
#> 
#> [[2]]
#> [1] "PRQC_1.2" "PRQC_2.2" "PRQC_3.2" "PRQC_4.2"
#> 
#> [[3]]
#> [1] 4
#> 
#> [[4]]
#> [1] "1"
#> 
#> [[5]]
#> [1] "2"
```

2.  Use one of the script-writing `dySEM` functions (e.g., `dyadCFA`,
    `apimSEM`, `INULL`), and supply it your saved `dvn`. It will save a
    `lavaan`-friendly script object and output a reproducible version of
    the script as a .txt to your current working directory (post this
    somewhere like the [OSF](http://osf.io) to make your reviewers happy
    :) )

<!-- end list -->

``` r
#Generate script for a dyadic CFA model with loading-invariance contraints
dres.loading = dyadCFA(dvn, lvname = "PRQC",  model = "loading")
```

3.  Fit the scripted model using one of the `lavaan` model-fitting
    functions (e.g., `cfa`); this ensures you retain complete control
    over how your model is fitted (e.g., estimator selection, missing
    data treatment, etc.)

<!-- end list -->

``` r
#Fit model via lavaan's cfa function
dres.loading.fit <- cfa(dres.loading, data = dat, std.lv = F, auto.fix.first= F, meanstructure = T)
```

4.  Use of of the output-generating `dySEM` functions (e.g., `dyOutput`,
    `dyMIOutput`), which sift through the tremendous amount of
    information in your fitted `lavaan` object to return reproducible
    tables (of measurement and/or structural parameter estimates, of
    model fit indexes and comparisons), and/or (for some select) path
    diagrams generated using `semPlot::semPaths`.

<!-- end list -->

``` r
#Output tables of measurement parameters and model fit indexes to working directory
dyOutput(dvn, model = "cfa", dres.loading.fit, tabletype = "both")
```
