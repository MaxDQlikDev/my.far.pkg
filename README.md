
<!-- README.md is generated from README.Rmd. Please edit that file -->

# my.far.pkg

<!-- badges: start -->

[![Travis build
status](https://travis-ci.com/MaxDQlikDev/my.far.pkg.svg?branch=master)](https://travis-ci.com/MaxDQlikDev/my.far.pkg)
<!-- badges: end -->

The goal of my.far.pkg is to read data of Fatality Analysis Reporting
System (FARS) from the National Highway Traffic Safety. Also, provides
some map plotting functionality.

## Loading

``` r
library(my.far.pkg)
```

## Function `fars_summarize_years`

Summary info per each year-month:

``` r
fars_summarize_years(c(2014, 2015))
```

## Function `fars_map_state`

Plot accidents onto the map.

``` r
plot(fars_summarize_years(2014))
```
