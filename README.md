
<!-- README.md is generated from README.Rmd. Please edit this file -->

## sdApps

The `sdApps` package is a collection of companion applications that
support the [surveydown](https://surveydown.org/) survey platform.

## Installation

The `sdApps` package is not yet on CRAN, but the development version can
be installed from GitHub:

``` r
# install.packages("pak")
pak::pak('surveydown-dev/sdApps)
```

## Dashboard

The **surveydown Dashboard** is a Shiny application that provides a
user-friendly interface for managing and viewing survey response data
from live surveys. To launch the dashboard, run the following in the R
console:

``` r
sdApps::sd_dashboard()
```

## Studio

The **surveydown Studio** is a Shiny application that provides a GUI for
creating and managing surveys. To launch the dashboard, run the
following in the R console:

``` r
sdApps::sd_studio()
```
