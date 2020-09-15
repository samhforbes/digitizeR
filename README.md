
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
size for all participants.

Note that the output of polhemus digitizations is assumed to be a txt
file with landmarks (5), then sources first, followed by detectors.
Numbers are taken in cm rather than inches, and as part of this package
converted into mm for use with Homer2.

The usual pipeline for people interested in creating good templates for
each cap size will be: \* Load the package

``` r
library(digitizeR)
```

  - Read in the caps (note the type argument, we are working on making
    this more flexible)

<!-- end list -->

``` r
data <- read_in_caps(digipath = 'sam/all_digi',
             pattern = '.txt',
             capsize = 'sam/my_capsize_document.csv',
             type = 'Gates')
```

  - Select only those with the right number of points (37 in this case)

<!-- end list -->

``` r
sub_data <- select_caps_by_npoints(data = data, 
                                   npoints = 37)
```

  - Create the template

<!-- end list -->

``` r
templates = prepare_and_make_templates(original_data = sub_data,
                           permitted_dist = 10,
                           npoints = 37)
```

  - Then simply save the templates

<!-- end list -->

``` r
save_templates(template = templates,
               data = sub_data,
               path = 'sam/template_output')
```

There are also plotting functions to visualise the locations of the
points.

There are further functions to then apply all this methodolody to
individual templates to produce cleaned caps at an individual level. See
the documentation for details.
