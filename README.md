
<!-- README.md is generated from README.Rmd. Please edit that file -->

# RIVD

<!-- badges: start -->

<!-- badges: end -->

> *There’s [R/Pharma](http://rinpharma.com) and
> [R/medicine](https://r-medicine.com), now, let’s get R/IVD started
> :sunglasses:*

The goal of RIVD is to deliver **simple tools** and **adapted outputs**
for *in vitro diagnostic* (IVD) assays performances assessment. The
results can be used in early development phase, or *hopefully* for
building a validation report in compliance with regulatory frameworks.
The package is intended to follow the recommendations formulated in the
**[CLSI
guidelines](https://clsi.org)**.

## Installation

<!-- You can install the released version of RIVD from [CRAN](https://CRAN.R-project.org) with: -->

<!-- ``` r -->

<!-- install.packages("RIVD") -->

<!-- ``` -->

You can install the development version from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("Felixmil/RIVD")
```

## Examples

### Performance table for assay’s precision.

``` r
library(RIVD)
library(knitr)

data(Glucose,package="VCA")

kable(perfPrecision(Glucose, 'result','run', 'day'))
```

|                    |       DF |    %Total |       SD |   CV\[%\] |
| ------------------ | -------: | --------: | -------: | --------: |
| Withing laboratory | 64.77732 | 100.00000 | 3.596325 | 1.4726965 |
| within-day         | 19.00000 |  15.14319 | 1.399483 | 0.5730889 |
| within-run         | 20.00000 |  23.77537 | 1.753568 | 0.7180867 |
| error              | 40.00000 |  61.08144 | 2.810694 | 1.1509803 |

### Visualisation of data generated by the precision assessment protocol

``` r
library(RIVD)

data(Glucose,package="VCA")
plotPrecision(Glucose, 'result','run', 'day')
```

<img src="man/figures/README-unnamed-chunk-2-1.png" width="100%" />
