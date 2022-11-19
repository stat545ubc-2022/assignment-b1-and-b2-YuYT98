
<!-- README.md is generated from README.Rmd. Please edit that file -->

# maeFormula

<!-- badges: start -->
<!-- badges: end -->

The goal of maeFormula is to compute the mae value based on two given
input lists.

## Installation

You can install the development version of maeFormula from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("stat545ubc-2022/assignment-b1-and-b2-YuYT98")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(maeFormula)
## basic example code
predicted_list_1 = list(1,2,3)
actual_list_1 = list(4,5,6)
maeFormula(predicted_list_1, actual_list_1)
#> [1] 2
```

The code above shows how maeFormula function works. It takes two
arguments and the type of each argument has to be a list. Also, the
length of two input list has to be the same. No NULL or NA values are
allowed in two input lists.
