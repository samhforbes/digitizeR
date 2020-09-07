
<!-- README.md is generated from README.Rmd. Please edit that file -->

# digitizeR

<!-- badges: start -->

<!-- badges: end -->

The goal of digitizeR is to take digitizations made with a polhemus
digitizer, and prepare them for use in image reconstructed fNIRS.

## Installation

You can install the development version from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("samhforbes/digitizeR")
```

## Example

A csv of participants’ cap and head sizes is required, where the first
column is ID, second is head size, and third is cap size. If you don’t
wish to separate by cap size then a single number can be input for cap
size.

Note that the output of polhemus digitizations is assumed to be a txt
file with sources first, followed by detectors. Numbers are taken in cm
rather than inches, and as part of this package converted into mm for
use with Homer2.
