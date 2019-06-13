
<!-- README.md is generated from README.Rmd. Please edit that file -->

# dySEM

<!-- badges: start -->

<!-- badges: end -->

The goal of dySEM is help automate the process of using lavaan() to fit
latent dyadic data analysis models. Currently the package supports the
fitting of dyadic measurement invariance models (including assessing
magnitude of dyadic noninvariance), and latent Actor-Partner
Interdependence Models (APIMs). Once fitted, automated output includes
tables (of measurement and/or structural parameters) and/or path
diagrams (of unstandardized, standardized or labeled parameter
estimates).

## Installation

You can install the development version from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("jsakaluk/dySEM")
```

## Example

To use dySEM(), you will need to have a data set imported, that is in
wide (or dyad) format (i.e., each row is a dyad, with separate columns
for variables from Partner 1 and Partner 2). Currently, the package
requires that your indicator variables for Factors X and Y follow some
repetitious ***stem*** (e.g., “prqc” in “prqc1.1”, “prqc1.2”), and that
the same variable for different partners has a ***distinguishing
character*** which indicates which variable is for which partner (e.g.,
the “1” or “2” in “prqc1.1”, “prqc1.2”), which may (or may not) have a
***separator*** (e.g., the “.” in “prqc1.1”, “prqc1.2”) in between the
***stem*** and ***distinguishing character***

The workflow of using dySEM() proceeds in four steps.

1.  Use the dyadVarNames() function to extract some information about
    your indicator variables for your latent variable(s).
2.  Use one of the script-writing functions (e.g., dyadCFA(), apimSEM())
    to write the lavaan() script for your desired measurment and/or
    structural model. You will need to specify a few options, but most
    of the work is done using the outputted object from the previous
    step via dyadVarNames(). Your lavaan() script is both saved to your
    global environment and outputted to your working directory for
    reproducibility purposes.
3.  Use one of the lavaan() model fitting functions (e.g., cfa(), sem())
    to fit the model specified by your chosen dySEM() script-writing
    function and save the output.
4.  Use dyOutput() to automatically export reproducible results in the
    form of tables and/or path diagrams to your current working
    directory.

<!-- end list -->

``` r
library(dySEM)
library(lavaan)
#> This is lavaan 0.6-3
#> lavaan is BETA software! Please report any bugs.
## dyadic CFA/invariance example 

#Specify the stem of variable names for indicators of latent factor x and/or y, 
#the separator (if any) between stem and partner distinguisher, and the partner distinguisher for each partner
dvn = dyadVarNames(dat, xvar="X", yvar="Y", sep = ".",distinguish1 = "1", distinguish2 = "2")

#Use dyadCFA() to write lavaan() script for fitting configural invariance model. lvxname is arbitrary, but lv argument must match factor from dyadVarNames().
con.config.script = dyadCFA(dvn, lvxname = "Conflict", lv="X", model = "configural")


#Use lavaan::cfa() to fit the model specified by dyadCFA(). Disable cfa()
#Scale-setting options, and request mean structure. 
con.fit.config <- cfa(con.config.script, data = dat, std.lv = F, auto.fix.first= F, meanstructure = T)

#Optional: use dydMACS() on a dyadic configural invariance model to compute the magnitude of noninvariance in indicators of the factor
con.dmacs = dydMACS(con.fit.config)

#Rinse and repeat for meaurement model of factor Y
#Save script
sat.config.script = dyadCFA(dvn, lvyname = "Satisfaction", lv="Y", model = "configural")

#Fit configural invariance model
sat.fit.config <- cfa(sat.config.script, data = dat, std.lv = F, auto.fix.first= F, meanstructure = T)

#Calculate dydMACS
sat.dmacs = dydMACS(sat.fit.config)

#Use apimSEM() to write latent APIM syntax with configural invariance
apim.script.config = apimSEM(dvn, lvxname = "Conflict", lvyname = "Satisfaction", model = "configural")

#Fit the model written by apimSEM() with lavaan
apim.fit.config <- cfa(apim.script.config, data = dat, std.lv = F, auto.fix.first= F, meanstructure = T)


#Use dyOutput to save reproducible output to your working directory,
#in this case, both tables of measurement model parameters, and structural
#parameters, and a path diagram with unstandardized estimates
dyOutput(dvn, apim.fit.config, tabletype = "both", figtype = "unstandardized",
         dydMACS.x = con.dmacs, dydMACS.y = sat.dmacs)
#> Output stored in /Users/johnsakaluk/Dropbox/Research/Sakaluk/Dyadic Invariance/Data and Scripts/dySEM/apim.png
```