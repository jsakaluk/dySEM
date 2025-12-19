# Fitting and Interpreting the Multiple Correlated Dyadic Factors Model (M-CDFM)

## Fair Use of this Tutorial

This tutorial is a supplemental material from the following article:

Sakaluk, J. K., & Camanto, O. J. (2025). *Dyadic Data Analysis via
Structural Equation Modeling with Latent Variables: A Tutorial with the
dySEM package for R*.

This article is intended to serve as the primary citation of record for
the `dySEM` package’s functionality for the techniques described in this
tutorial. **If this tutorial has informed your modeling strategy
(including but not limited to your use of `dySEM`), please cite this
article**.

The citation of research software aiding in analyses—like the use of
`dySEM`—is a required practice, according to the Journal Article
Reporting Standards (JARS) for Quantitative Research in Psychology
([Appelbaum et al.,
2018](https://psycnet.apa.org/fulltext/2018-00750-002.html)).

Furthermore, citations remain essential for our development team to
demonstrate the impact of our work to our local institutions and our
funding sources, and with your support, we will be able to continue
justifying our time and efforts spent improving `dySEM` and expanding
its reach.

## Overview

The M-CDFM is a **“multi-construct”** dyadic SEM (i.e., used to
represent dyadic data about more than two constructs—like different
features of relationship quality, e.g., Fletcher et al., 2000).

It contains:

- parallel/identical sets of latent variables, onto which
- each partner’s observed variables discriminantly load (i.e., one
  partner’s observed variables onto their respective factors, and the
  other partner’s observed variables onto their respective factors)

It also features several varieties of covariances (or correlations,
depending on scale-setting/output standardization):

- those between two latent variables for the same construct across
  partners (effectively, latent “intraclass” correlation coefficients,
  when standardized; e.g., Partner A’s latent commitment with Partner
  B’s latent commitment),
- those between two latent variables for different constructs within
  each partner (e.g., effectively, latent “intrapartner” correlation
  coefficients, when standardized; e.g., Partner A’s latent commitment
  with Partner A’s latent passion),
- those between two latent variables for different constructs across
  partners (e.g., effectively, latent “interpartner” correlation
  coefficients, when standardized; e.g., Partner A’s latent commitment
  with Partner B’s latent passion) , and
- several between the residual variances of the same observed variables
  across each partner (e.g., between Item 1 for Partner A and Partner B;
  another between Item 2 for Partner A and Partner B, etc.,).

The M-CDFM is typically the model people mean when they refer to “dyadic
CFA”, though dyadic CFA is more of a statistical framework that could be
used to fit multi-construct dyadic data that corresponds to other data
generating mechanisms of uni-construct dyadic data (e.g., from multiple
constructs embodying Univariate Dyadic Factor Models). It is therefore
likely the most common model used for psychometric tests of dyadic data.

## Packages and Data

This exemplar makes use of the `dplyr`, `gt`, `dySEM`, and `lavaan`
(Rosseel, 2012) packages.

``` r

library(dplyr) #for data management
library(gt) #for reproducible tabling
library(dySEM) #for dyadic SEM scripting and outputting
library(lavaan) #for fitting dyadic SEMs
```

For this exemplar, we use two built-in datasets from `dySEM` (focusing
more on one). Of primary interest, the
[`imsM`](https://jsakaluk.github.io/dySEM/reference/imsM.html) dataset
corresponds to data from the “global” items from 282 mixed-sex couples
responding to the full Rusbult et al. (1998) Investment Model Scale
(IMS) (including 5 items each for satisfaction, quality of alternatives,
investment, and commitment). More information about this data set can be
found in [Sakaluk et al. (2021)](https://doi.org/10.1111/pere.12341).

All variables in this data frame follow a [“sip” naming pattern](NA):

``` r

imsM_dat <- imsM

imsM_dat |> 
  as_tibble()
#> # A tibble: 282 × 40
#>    sat.g1_f sat.g2_f sat.g3_f sat.g4_f sat.g5_f qalt.g1_f qalt.g2_f qalt.g3_f
#>       <int>    <int>    <int>    <int>    <int>     <int>     <int>     <int>
#>  1        2        1        1        2        1         3         1         5
#>  2        9        9        9        9        9         5         1         1
#>  3        6        5        7        5        7         5         3         5
#>  4        9        5        9        9        9         8         9         9
#>  5        1        7        1        2        2         9         7         8
#>  6        8        8        8        8        8         1         1         5
#>  7        9        9        9        9        9         1         1         1
#>  8        1        1        1        1        1         1         1         1
#>  9        6        5        4        6        6         7         5         7
#> 10        9        9        9        9        9         1         1         1
#> # ℹ 272 more rows
#> # ℹ 32 more variables: qalt.g4_f <int>, qalt.g5_f <int>, invest.g1_f <int>,
#> #   invest.g2_f <int>, invest.g3_f <int>, invest.g4_f <int>, invest.g5_f <int>,
#> #   com1_f <int>, com2_f <int>, com3_f <int>, com4_f <int>, com5_f <int>,
#> #   sat.g1_m <int>, sat.g2_m <int>, sat.g3_m <int>, sat.g4_m <int>,
#> #   sat.g5_m <int>, qalt.g1_m <int>, qalt.g2_m <int>, qalt.g3_m <int>,
#> #   qalt.g4_m <int>, qalt.g5_m <int>, invest.g1_m <int>, invest.g2_m <int>, …
```

We will also use data from the
[`prqcQ`](https://jsakaluk.github.io/dySEM/reference/prqcQ.html)
dataset, which contains data from 118 queer couples responding to the
Perceived Relationship Quality Components Inventory (Fletcher et al.,
2000) (including 3 items each for satisfaction, commitment, intimacy,
trust, passion, and love). While substantive analyses of this data set
have not yet been reported, the data collection took place in parallel
to the analyses reported in [Sakaluk et
al. (2021)](https://doi.org/10.1111/pere.12341).

In contrast to `imsM`, variable names in the `prqcQ` data frame follow
the [“spi” naming
pattern](https://jsakaluk.github.io/dySEM/articles/varnames.html):

``` r

prqc_dat <- prqcQ

prqc_dat |> 
  as_tibble()
#> # A tibble: 118 × 36
#>    prqc.1_1 prqc.1_2 prqc.1_3 prqc.1_4 prqc.1_5 prqc.1_6 prqc.1_7 prqc.1_8
#>       <int>    <int>    <int>    <int>    <int>    <int>    <int>    <int>
#>  1        5        6        5        5        4        6        6        6
#>  2        7        7        7        7        7        7        7        7
#>  3        4        3        4        7        7        7        1        2
#>  4        5        5        7        6        5        7        5        7
#>  5        7        7        7        7        7        7        7        7
#>  6        5        5        5        6        7        7        6        6
#>  7        7        7        7        7        6        6        6        7
#>  8        7        7        7        7        7        7        7        7
#>  9        4        4        4        6        6        6        4        6
#> 10        7        7        7        7        7        7        7        7
#> # ℹ 108 more rows
#> # ℹ 28 more variables: prqc.1_9 <int>, prqc.1_10 <int>, prqc.1_11 <int>,
#> #   prqc.1_12 <int>, prqc.1_13 <int>, prqc.1_14 <int>, prqc.1_15 <int>,
#> #   prqc.1_16 <int>, prqc.1_17 <int>, prqc.1_18 <int>, prqc.2_1 <int>,
#> #   prqc.2_2 <int>, prqc.2_3 <int>, prqc.2_4 <int>, prqc.2_5 <int>,
#> #   prqc.2_6 <int>, prqc.2_7 <int>, prqc.2_8 <int>, prqc.2_9 <int>,
#> #   prqc.2_10 <int>, prqc.2_11 <int>, prqc.2_12 <int>, prqc.2_13 <int>, …
```

## 1. Scraping the Variables

Users will take note that the `imsM` dataset contains variables for
items for which there are distinctive stems for each subscale (e.g.,
“sat”, “invest”, “com”), whereas the `prqcQ` dataset contains a
repetitive uninformative stem (“prqc”), despite the presence of
multidimensionality. Rest assured,
[`scrapeVarCross()`](https://jsakaluk.github.io/dySEM/reference/scrapeVarCross.html)
has been developed with functionality to scrape variable information
from both kinds of stems from multidimensional instruments. Further,
scraping variable information remains relatively straightforward (and
the key to unlocking the powerful functionality of `dySEM`‘s scripters),
with just one added “twist” from the simpler case of uni-construct or
bi-construct models: the creation of a *named list* spelling out the
naming ’formula’ for each set of indicators.

### Distinctive Indicator Stems

The case when indicators have distinctive stems—usually indicating the
putative factor onto which they load (e.g., “sat”, “com”)—is the most
straightforward to scrape with
[`scrapeVarCross()`](https://jsakaluk.github.io/dySEM/reference/scrapeVarCross.md).

In this case, users will need to provide the names of the latent
variables, the stem for each set of indicators, and the delimiters used
in the variable names. In the case of the `imsM` exemplar (i.e., when
stems are distinct), the named list must contain the following four
elements/vectors (and the names of these elements/vectors must follow
what is shown below):

- **lvnames**: a vector of the (arbitrary) names that will be used for
  the latent variables in the `lavaan` script
- **stem**: a vector containing each distinctive stem for each set of
  indicators (e.g., “sat.g”, “qalt.g”, “invest.g”, “com”)—
  [`scrapeVarCross()`](https://jsakaluk.github.io/dySEM/reference/scrapeVarCross.md)
  will use these to search for indicators with matching stems
- **delim1**: a vector containing the first delimiter used in the
  variable names (e.g., ““,”“,”“,”“)—
  [`scrapeVarCross()`](https://jsakaluk.github.io/dySEM/reference/scrapeVarCross.md)
  will use these to search for indicators with matching patterns of
  delimination
- **delim2**: a vector containing the second delimiter used in the
  variable names (e.g., “*”, ”*”, “*”, ”*”)—
  [`scrapeVarCross()`](https://jsakaluk.github.io/dySEM/reference/scrapeVarCross.md)
  will use these to search for indicators with matching patterns of
  delimination

Each of these elements/vectors must be of the same length, and the order
of the elements in each vector must correspond to the order of the
latent variables in `lvnames`. And so, saving this named list (in
essence, the “recipe card” of indicator names for each factor) into its
own object:

``` r

#When different factor use distinct stems:
imsList <- list(lvnames = c("Sat", "Q_Alt", "Invest", "Comm"),
stem = c("sat.g", "qalt.g", "invest.g", "com"),
delim1 = c("", "", "", ""),
delim2 = c("_", "_", "_", "_"))
```

This list can then be passed to
[`scrapeVarCross()`](https://jsakaluk.github.io/dySEM/reference/scrapeVarCross.md)
to scrape the variable names from the `imsM` data frame, using the
`var_list` argument. The `var_list_order` argument must then be set to
indicate that the variables follow the [“sip” naming
pattern](https://jsakaluk.github.io/dySEM/articles/varnames.html), and
distinguishing characters must also be specified with the
`distinguish_1` and `distinguish_2` arguments[¹](#fn1):

``` r

dvnIMS <- scrapeVarCross(imsM,
var_list = imsList,
var_list_order = "sip",
distinguish_1 = "f",
distinguish_2 = "m")
#> 
#> ── Variable Scraping Summary ──
#> 
#> ✔ Successfully scraped 4 latent variables
#> ℹ Sat: 5 indicators for P1 (f), 5 indicators for P2 (m)
#> ℹ Q_Alt: 5 indicators for P1 (f), 5 indicators for P2 (m)
#> ℹ Invest: 5 indicators for P1 (f), 5 indicators for P2 (m)
#> ℹ Comm: 5 indicators for P1 (f), 5 indicators for P2 (m)
#> ℹ Total indicators: 40
```

The resulting *dvn* remains as unremarkable as ever, but continues to
serve as the foundation for unlocking the functionality of
multi-construct scripters in `dySEM`. The only difference from the
one-construct use-case of
[`scrapeVarCross()`](https://jsakaluk.github.io/dySEM/reference/scrapeVarCross.md)
is that the `$p1xvarnames` and `$p2xvarnames` elements now contain a
nested set of vectors of variable names (as opposed to only set of
variable names):

``` r

dvnIMS
#> $p1xvarnames
#> $p1xvarnames$Sat
#> [1] "sat.g1_f" "sat.g2_f" "sat.g3_f" "sat.g4_f" "sat.g5_f"
#> 
#> $p1xvarnames$Q_Alt
#> [1] "qalt.g1_f" "qalt.g2_f" "qalt.g3_f" "qalt.g4_f" "qalt.g5_f"
#> 
#> $p1xvarnames$Invest
#> [1] "invest.g1_f" "invest.g2_f" "invest.g3_f" "invest.g4_f" "invest.g5_f"
#> 
#> $p1xvarnames$Comm
#> [1] "com1_f" "com2_f" "com3_f" "com4_f" "com5_f"
#> 
#> 
#> $p2xvarnames
#> $p2xvarnames$Sat
#> [1] "sat.g1_m" "sat.g2_m" "sat.g3_m" "sat.g4_m" "sat.g5_m"
#> 
#> $p2xvarnames$Q_Alt
#> [1] "qalt.g1_m" "qalt.g2_m" "qalt.g3_m" "qalt.g4_m" "qalt.g5_m"
#> 
#> $p2xvarnames$Invest
#> [1] "invest.g1_m" "invest.g2_m" "invest.g3_m" "invest.g4_m" "invest.g5_m"
#> 
#> $p2xvarnames$Comm
#> [1] "com1_m" "com2_m" "com3_m" "com4_m" "com5_m"
#> 
#> 
#> $xindper
#> [1] 20
#> 
#> $dist1
#> [1] "f"
#> 
#> $dist2
#> [1] "m"
#> 
#> $indnum
#> [1] 40
```

### Non-Distinctive Indicator Stems

The case when indicators have non-distinctive stems only requires that
your named list—that you later supply to
[`scrapeVarCross()`](https://jsakaluk.github.io/dySEM/reference/scrapeVarCross.md)—has
two more elements/vectors. Currently, **this functionality assumes
indicators from the same latent variable (e.g., items measuring
Satisfaction) are clustered together, sequentially, in item number
(e.g., prqc.1_1 prqc.1_2 prqc.1_3) rather than being interspersed with
items from other latent variables (e.g., prqc.1_1 prqc.1_5, prqc.1_8)**.

The additional arguments that must be added to the named list are:

- **min_num**: a vector containing the item number in the sequence of
  non-distinctively named indicators that corresponds to the first item
  for each latent variable (e.g., 1, 4, 7, 10, 13, 16 for Satisfaction,
  Commitment, Intimacy, Trust, Passion, and Love, respectively), and
- **max_num**: a vector containing the item number in the sequence of
  non-distinctively named indicators that corresponds to the last item
  for each latent variable (e.g., 3, 6, 9, 12, 15, 18 for Satisfaction,
  Commitment, Intimacy, Trust, Passion, and Love, respectively).

``` r

#When different factor use non-distinct stems:
prqcList <- list(lvnames = c("Sat", "Comm", "Intim", "Trust", "Pass", "Love"),
stem = c("prqc", "prqc", "prqc", "prqc", "prqc", "prqc"),
delim1 = c(".", ".", ".", ".", ".", "."),
delim2 = c("_", "_", "_", "_", "_", "_"),
min_num = c(1, 4, 7, 10, 13, 16),
max_num = c(3, 6, 9, 12, 15, 18))
```

This named list can then be passed to
[`scrapeVarCross()`](https://jsakaluk.github.io/dySEM/reference/scrapeVarCross.md)
to the same effect (noting that in this case, the `prqcQ` data set
follows a [“spi” naming
pattern](https://jsakaluk.github.io/dySEM/articles/varnames.html), and
“1” and “2” as the distinguishing characters):

``` r

dvnPRQC<- scrapeVarCross(prqcQ,
var_list = prqcList,
var_list_order = "spi",
distinguish_1 = "1",
distinguish_2 = "2")
#> 
#> ── Variable Scraping Summary ──
#> 
#> ✔ Successfully scraped 6 latent variables
#> ℹ Sat: 3 indicators for P1 (1), 3 indicators for P2 (2)
#> ℹ Comm: 3 indicators for P1 (1), 3 indicators for P2 (2)
#> ℹ Intim: 3 indicators for P1 (1), 3 indicators for P2 (2)
#> ℹ Trust: 3 indicators for P1 (1), 3 indicators for P2 (2)
#> ℹ Pass: 3 indicators for P1 (1), 3 indicators for P2 (2)
#> ℹ Love: 3 indicators for P1 (1), 3 indicators for P2 (2)
#> ℹ Total indicators: 36

dvnPRQC
#> $p1xvarnames
#> $p1xvarnames$Sat
#> [1] "prqc.1_1" "prqc.1_2" "prqc.1_3"
#> 
#> $p1xvarnames$Comm
#> [1] "prqc.1_4" "prqc.1_5" "prqc.1_6"
#> 
#> $p1xvarnames$Intim
#> [1] "prqc.1_7" "prqc.1_8" "prqc.1_9"
#> 
#> $p1xvarnames$Trust
#> [1] "prqc.1_10" "prqc.1_11" "prqc.1_12"
#> 
#> $p1xvarnames$Pass
#> [1] "prqc.1_13" "prqc.1_14" "prqc.1_15"
#> 
#> $p1xvarnames$Love
#> [1] "prqc.1_16" "prqc.1_17" "prqc.1_18"
#> 
#> 
#> $p2xvarnames
#> $p2xvarnames$Sat
#> [1] "prqc.2_1" "prqc.2_2" "prqc.2_3"
#> 
#> $p2xvarnames$Comm
#> [1] "prqc.2_4" "prqc.2_5" "prqc.2_6"
#> 
#> $p2xvarnames$Intim
#> [1] "prqc.2_7" "prqc.2_8" "prqc.2_9"
#> 
#> $p2xvarnames$Trust
#> [1] "prqc.2_10" "prqc.2_11" "prqc.2_12"
#> 
#> $p2xvarnames$Pass
#> [1] "prqc.2_13" "prqc.2_14" "prqc.2_15"
#> 
#> $p2xvarnames$Love
#> [1] "prqc.2_16" "prqc.2_17" "prqc.2_18"
#> 
#> 
#> $xindper
#> [1] 18
#> 
#> $dist1
#> [1] "1"
#> 
#> $dist2
#> [1] "2"
#> 
#> $indnum
#> [1] 36
```

Whether your *dvn* contains variable information for indicators with
distinctive or non-distinctive stems makes no difference to the
functionality you can draw upon from `dySEM`, or how you pass this
information to scripters—it remains the same (and straightforward) for
both kinds of indicators.

## 2. Scripting the Model(s)

Continuing with the `imsM` exemplar for this rest of this vignette, we
can now quickly script a series of M-CDFMs using the
[`scriptCFA()`](https://jsakaluk.github.io/dySEM/reference/scriptCFA.html)
function (so named given that most imagine a M-CDFM model when
conducting “dyadic CFA”), and incrementally layer on more dyadic
invariance constraints via the `constr_dy_meas` argument.

The `writeTo` and `fileName` arguments (not depicted here) remain
available to those wanting to export a `.txt` file of their concatenated
`lavaan` script(s) (e.g., for posting on the OSF). `scaleset` remains
fixed-factor, by default, but this is changeable to marker-variable
(“MV”) for those interested. Given the consequential nature of this
choice (e.g., for interpreting the scale of parameter estimates), we
strongly recommend making this argument explicit (despite its implicit
default to “FF”):

``` r

script.ims.config <-  scriptCFA(dvnIMS, scaleset = "FF", constr_dy_meas = "none", constr_dy_struct = "none")

script.ims.load <-  scriptCFA(dvnIMS, scaleset = "FF", constr_dy_meas = c("loadings"), constr_dy_struct = "none")

script.ims.int <-  scriptCFA(dvnIMS, scaleset = "FF", constr_dy_meas = c("loadings", "intercepts"),constr_dy_struct = "none")

script.ims.res <-  scriptCFA(dvnIMS, scaleset = "FF",constr_dy_meas = c("loadings", "intercepts", "residuals"), constr_dy_struct = "none")
```

You can concatenate these scripts, if you wish, using the base
[`cat()`](https://rdrr.io/r/base/cat.html) function, to make them more
human-readable in your console, but beware: the amount of `lavaan`
scripting is considerable (and therefore we do not show what it returns
here):

``` r

cat(script.ims.config)
```

## 3. Fitting the Model(s)

All of these models can now be fit with whichever `lavaan` wrapper the
user prefers, and with their chosen analytic options (e.g., estimator,
missing data treatment, etc.); our demonstration uses default
specifications for [`cfa()`](https://rdrr.io/pkg/lavaan/man/cfa.html):

``` r

ims.config.mod <- cfa(script.ims.config, data = imsM_dat)

ims.load.mod <- cfa(script.ims.load, data = imsM_dat)

ims.int.mod <- cfa(script.ims.int, data = imsM_dat)

ims.res.mod <- cfa(script.ims.res, data = imsM_dat)
```

## 4. Outputting and Interpreting the Model(s)

As with the [CDFM](https://jsakaluk.github.io/dySEM/articles/CDFM.html),
competing nested M-CDFMs—like for dyadic invariance testing with a
larger measure—can be compared with the base
[`anova()`](https://rdrr.io/r/stats/anova.html) function:

``` r

anova(ims.config.mod, ims.load.mod, ims.int.mod, ims.res.mod)
#> 
#> Chi-Squared Difference Test
#> 
#>                 Df   AIC   BIC  Chisq Chisq diff    RMSEA Df diff Pr(>Chisq)
#> ims.config.mod 692 36038 36633 1494.0                                       
#> ims.load.mod   708 36050 36588 1538.2     44.181 0.083109      16  0.0001851
#> ims.int.mod    724 36043 36524 1563.0     24.784 0.046399      16  0.0737307
#> ims.res.mod    744 36105 36515 1664.9    101.906 0.126728      20  5.733e-13
#>                   
#> ims.config.mod    
#> ims.load.mod   ***
#> ims.int.mod    .  
#> ims.res.mod    ***
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

Or, for a more reproducible-reporting-friendly option, `dySEM`’s
[`outputInvarCompTab()`](https://jsakaluk.github.io/dySEM/reference/outputInvarCompTab.html)
function is available, and amenable to other tabling packages/functions
(e.g., [`gt::gt()`](https://gt.rstudio.com/reference/gt.html), as we
demonstrate below), and/or can write an `.rtf` if the user makes use of
the `writeTo` and `fileName` arguments:

``` r

mods <- list(ims.config.mod, ims.load.mod, ims.int.mod, ims.res.mod)

outputInvarCompTab(mods) |> 
  gt()
```

| mod        | chisq    | df  | pvalue | aic      | bic      | rmsea | cfi   | chisq_diff | df_diff | p_diff | aic_diff | bic_diff | rmsea_diff | cfi_diff |
|------------|----------|-----|--------|----------|----------|-------|-------|------------|---------|--------|----------|----------|------------|----------|
| configural | 1494.001 | 692 | 0      | 36037.69 | 36632.63 | 0.067 | 0.920 | NA         | NA      | NA     | NA       | NA       | NA         | NA       |
| loading    | 1538.182 | 708 | 0      | 36049.88 | 36588.15 | 0.068 | 0.918 | 44.181     | 16      | 0.000  | 12.181   | -44.479  | 0.000      | -0.003   |
| intercept  | 1562.966 | 724 | 0      | 36042.66 | 36524.27 | 0.067 | 0.917 | 24.784     | 16      | 0.074  | -7.216   | -63.877  | 0.000      | -0.001   |
| residual   | 1664.873 | 744 | 0      | 36104.57 | 36515.35 | 0.070 | 0.909 | 101.906    | 20      | 0.000  | 61.906   | -8.919   | 0.002      | -0.008   |

Were one strictly to follow only the likelihood ratio test statistic as
a guide, they would reject the possibility of the relatively low bar of
loading invariance for the IMS, $\chi^{2}$ (16) = 44.18, *p* \<.001. Of
course, other methods of model comparison and selection (and their
respective thresholds) are available, and whatever method is chosen
probably ought to be preregistered.

Meanwhile, the standard outputters from `dySEM` are available to help
researchers either table and/or visualize their results. Given the
complexity of the M-CDFM (often many more factors and items) than the
CDFM, we recommend tabling (vs. visualizing) model output:

``` r

outputParamTab(dvn = dvnIMS, model = "cfa", fit = ims.config.mod,
               tabletype = "measurement") |> 
  gt()
```

| Latent Factor | Indicator   | Loading | SE    | Z      | p-value | Std. Loading | Intercept |
|---------------|-------------|---------|-------|--------|---------|--------------|-----------|
| Sat1          | sat.g1_f    | 1.953   | 0.094 | 20.767 | \< .001 | 0.959        | 7.459     |
| Sat1          | sat.g2_f    | 1.683   | 0.102 | 16.485 | \< .001 | 0.814        | 7.169     |
| Sat1          | sat.g3_f    | 2.057   | 0.104 | 19.767 | \< .001 | 0.932        | 6.996     |
| Sat1          | sat.g4_f    | 2.002   | 0.095 | 21.146 | \< .001 | 0.968        | 7.459     |
| Sat1          | sat.g5_f    | 2.023   | 0.102 | 19.919 | \< .001 | 0.937        | 7.227     |
| Q_Alt1        | qalt.g1_f   | 2.186   | 0.128 | 17.110 | \< .001 | 0.867        | 2.965     |
| Q_Alt1        | qalt.g2_f   | 1.909   | 0.145 | 13.119 | \< .001 | 0.724        | 3.510     |
| Q_Alt1        | qalt.g3_f   | 1.839   | 0.136 | 13.478 | \< .001 | 0.717        | 3.639     |
| Q_Alt1        | qalt.g4_f   | 2.013   | 0.126 | 15.936 | \< .001 | 0.832        | 3.141     |
| Q_Alt1        | qalt.g5_f   | 2.162   | 0.127 | 16.996 | \< .001 | 0.867        | 2.961     |
| Invest1       | invest.g1_f | 1.530   | 0.106 | 14.445 | \< .001 | 0.778        | 7.631     |
| Invest1       | invest.g2_f | 1.670   | 0.126 | 13.251 | \< .001 | 0.720        | 7.075     |
| Invest1       | invest.g3_f | 1.671   | 0.094 | 17.820 | \< .001 | 0.892        | 7.675     |
| Invest1       | invest.g4_f | 1.209   | 0.159 | 7.611  | \< .001 | 0.443        | 6.059     |
| Invest1       | invest.g5_f | 1.493   | 0.097 | 15.384 | \< .001 | 0.810        | 7.588     |
| Comm1         | com1_f      | 1.405   | 0.080 | 17.617 | \< .001 | 0.877        | 8.247     |
| Comm1         | com2_f      | 1.655   | 0.083 | 19.885 | \< .001 | 0.944        | 8.106     |
| Comm1         | com3_f      | -0.555  | 0.191 | -2.903 | 0.004   | -0.178       | 3.173     |
| Comm1         | com4_f      | -0.754  | 0.150 | -5.030 | \< .001 | -0.310       | 2.184     |
| Comm1         | com5_f      | 1.694   | 0.098 | 17.359 | \< .001 | 0.868        | 7.812     |
| Sat2          | sat.g1_m    | 1.866   | 0.099 | 18.820 | \< .001 | 0.908        | 7.463     |
| Sat2          | sat.g2_m    | 1.755   | 0.104 | 16.933 | \< .001 | 0.830        | 7.239     |
| Sat2          | sat.g3_m    | 2.075   | 0.105 | 19.813 | \< .001 | 0.934        | 7.039     |
| Sat2          | sat.g4_m    | 1.962   | 0.094 | 20.858 | \< .001 | 0.962        | 7.494     |
| Sat2          | sat.g5_m    | 2.118   | 0.114 | 18.614 | \< .001 | 0.902        | 7.157     |
| Q_Alt2        | qalt.g1_m   | 2.173   | 0.125 | 17.426 | \< .001 | 0.870        | 3.129     |
| Q_Alt2        | qalt.g2_m   | 2.089   | 0.132 | 15.875 | \< .001 | 0.820        | 3.329     |
| Q_Alt2        | qalt.g3_m   | 2.017   | 0.138 | 14.583 | \< .001 | 0.755        | 3.710     |
| Q_Alt2        | qalt.g4_m   | 2.276   | 0.122 | 18.604 | \< .001 | 0.907        | 3.137     |
| Q_Alt2        | qalt.g5_m   | 2.394   | 0.128 | 18.655 | \< .001 | 0.909        | 3.216     |
| Invest2       | invest.g1_m | 1.472   | 0.098 | 14.993 | \< .001 | 0.800        | 7.757     |
| Invest2       | invest.g2_m | 1.575   | 0.120 | 13.145 | \< .001 | 0.717        | 7.290     |
| Invest2       | invest.g3_m | 1.503   | 0.087 | 17.188 | \< .001 | 0.875        | 7.875     |
| Invest2       | invest.g4_m | 1.304   | 0.149 | 8.736  | \< .001 | 0.502        | 6.439     |
| Invest2       | invest.g5_m | 1.425   | 0.095 | 14.996 | \< .001 | 0.798        | 7.796     |
| Comm2         | com1_m      | 1.675   | 0.081 | 20.788 | \< .001 | 0.962        | 8.102     |
| Comm2         | com2_m      | 1.634   | 0.080 | 20.436 | \< .001 | 0.954        | 8.125     |
| Comm2         | com3_m      | -0.788  | 0.197 | -3.999 | \< .001 | -0.242       | 3.482     |
| Comm2         | com4_m      | -1.201  | 0.133 | -9.037 | \< .001 | -0.525       | 2.149     |
| Comm2         | com5_m      | 1.482   | 0.094 | 15.797 | \< .001 | 0.813        | 7.929     |

We have also provided a `tabletype = "correlation"` option to assist
with providing reproducible correlation tables for reporting on the
intraclass, intrapartner, and interpartner correlations, from larger
multi-construct models:

``` r

outputParamTab(dvn = dvnIMS, model = "cfa", fit = ims.config.mod,
               tabletype = "correlation") |> 
  gt()
```

|         | Sat1         | Q_Alt1       | Invest1      | Comm1        | Sat2         | Q_Alt2       | Invest2     | Comm2 |
|---------|--------------|--------------|--------------|--------------|--------------|--------------|-------------|-------|
| Sat1    | —            | —            | —            | —            | —            | —            | —           | —     |
| Q_Alt1  | -0.253\*\*\* | —            | —            | —            | —            | —            | —           | —     |
| Invest1 | 0.723\*\*\*  | -0.177\*\*   | —            | —            | —            | —            | —           | —     |
| Comm1   | 0.749\*\*\*  | -0.303\*\*\* | 0.819\*\*\*  | —            | —            | —            | —           | —     |
| Sat2    | 0.762\*\*\*  | -0.192\*\*   | 0.694\*\*\*  | 0.669\*\*\*  | —            | —            | —           | —     |
| Q_Alt2  | -0.317\*\*\* | 0.612\*\*\*  | -0.358\*\*\* | -0.389\*\*\* | -0.445\*\*\* | —            | —           | —     |
| Invest2 | 0.591\*\*\*  | -0.186\*\*   | 0.751\*\*\*  | 0.649\*\*\*  | 0.725\*\*\*  | -0.467\*\*\* | —           | —     |
| Comm2   | 0.553\*\*\*  | -0.205\*\*   | 0.615\*\*\*  | 0.696\*\*\*  | 0.709\*\*\*  | -0.472\*\*\* | 0.753\*\*\* | —     |

## 5. Optional Indexes and Output

Finally, like with the CDFM, users can use additional `dySEM` functions
to output additional information about their model, such as the
computation of Lagrange multiplier tests to identify for which items and
measurement model parameters there is significant noninvariance:

``` r

outputConstraintTab(ims.load.mod) |> 
  gt()
```

| param1                 | constraint | param2                 | chi2   | df  | pvalue | sig    |
|------------------------|------------|------------------------|--------|-----|--------|--------|
| Sat1 =~ sat.g1_f       | ==         | Sat2 =~ sat.g1_m       | 2.027  | 1   | 0.154  | NA     |
| Sat1 =~ sat.g2_f       | ==         | Sat2 =~ sat.g2_m       | 1.209  | 1   | 0.271  | NA     |
| Sat1 =~ sat.g3_f       | ==         | Sat2 =~ sat.g3_m       | 0.112  | 1   | 0.738  | NA     |
| Sat1 =~ sat.g4_f       | ==         | Sat2 =~ sat.g4_m       | 0.913  | 1   | 0.339  | NA     |
| Sat1 =~ sat.g5_f       | ==         | Sat2 =~ sat.g5_m       | 1.940  | 1   | 0.164  | NA     |
| Q_Alt1 =~ qalt.g1_f    | ==         | Q_Alt2 =~ qalt.g1_m    | 3.382  | 1   | 0.066  | NA     |
| Q_Alt1 =~ qalt.g2_f    | ==         | Q_Alt2 =~ qalt.g2_m    | 0.019  | 1   | 0.891  | NA     |
| Q_Alt1 =~ qalt.g3_f    | ==         | Q_Alt2 =~ qalt.g3_m    | 0.064  | 1   | 0.801  | NA     |
| Q_Alt1 =~ qalt.g4_f    | ==         | Q_Alt2 =~ qalt.g4_m    | 0.868  | 1   | 0.352  | NA     |
| Q_Alt1 =~ qalt.g5_f    | ==         | Q_Alt2 =~ qalt.g5_m    | 0.379  | 1   | 0.538  | NA     |
| Invest1 =~ invest.g1_f | ==         | Invest2 =~ invest.g1_m | 0.079  | 1   | 0.778  | NA     |
| Invest1 =~ invest.g2_f | ==         | Invest2 =~ invest.g2_m | 0.002  | 1   | 0.960  | NA     |
| Invest1 =~ invest.g3_f | ==         | Invest2 =~ invest.g3_m | 1.040  | 1   | 0.308  | NA     |
| Invest1 =~ invest.g4_f | ==         | Invest2 =~ invest.g4_m | 1.265  | 1   | 0.261  | NA     |
| Invest1 =~ invest.g5_f | ==         | Invest2 =~ invest.g5_m | 0.083  | 1   | 0.773  | NA     |
| Comm1 =~ com1_f        | ==         | Comm2 =~ com1_m        | 20.009 | 1   | 0.000  | \*\*\* |
| Comm1 =~ com2_f        | ==         | Comm2 =~ com2_m        | 5.970  | 1   | 0.015  | \*     |
| Comm1 =~ com3_f        | ==         | Comm2 =~ com3_m        | 0.624  | 1   | 0.430  | NA     |
| Comm1 =~ com4_f        | ==         | Comm2 =~ com4_m        | 4.835  | 1   | 0.028  | \*     |
| Comm1 =~ com5_f        | ==         | Comm2 =~ com5_m        | 12.220 | 1   | 0.000  | \*\*\* |

In this particular example, significant noninvariance is present in all
but the third item of the *Commitment* factor the the IMS.

------------------------------------------------------------------------

1.  In future updates of
    [`scrapeVarCross()`](https://jsakaluk.github.io/dySEM/reference/scrapeVarCross.md)
    we may consider enabling users to specify different naming patterns
    and/or distinguishing characters for indicators from each latent
    variable, but for now, the same pattern and characters must be used
    (and we assume that most users already follow this kind of variable
    naming convention)
