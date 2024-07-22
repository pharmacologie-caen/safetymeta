
<!-- README.md is generated from README.Rmd. Please edit that file -->

# safetymeta

<!-- badges: start -->

[![R-CMD-check](https://github.com/pharmacologie-caen/safetymeta/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/pharmacologie-caen/safetymeta/actions/workflows/R-CMD-check.yaml)
[![codecov](https://codecov.io/gh/pharmacologie-caen/safetymeta/graph/badge.svg?token=5BCy4WfqJa)](https://codecov.io/gh/pharmacologie-caen/safetymeta)
<!-- badges: end -->

The goal of safetymeta is to help manage adverse events aggregated
datasets in order to perform a safety meta-analysis.

> Visit the [package
> website](https://pharmacologie-caen.github.io/safetymeta/)

## Installation

You can install the development version of safetymeta from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("pharmacologie-caen/safetymeta")
```

## Pre-requisites

Users are assumed to be familiar with

- The principles of meta-analysis.

- R (basic knowledge, a good start can be found
  [here](https://r4ds.hadley.nz/)).

- The [PRISMA guidelines](https://www.prisma-statement.org/).

## What the package does not cover

- Safety meta-analysis of individual patient level data.

## Overview

In this package, you will find steps to manage adverse events datasets,
avoiding common pitfalls in order to provide accurate, reproducible
workflows when conducting a safety meta-analysis.

# Usage

``` r
library(safetymeta)
```

## Define priority among safety data sources

``` r
source_order <- 
  c("src1", "src2")

e_data_source <-
  e_data |> 
  sort_sources(
    method = "source_list",
    source_name = "source",
    source_list_order = source_order
  )
```

## Group events into terms

``` r
e_data_source |> 
  group_events(
    t_groups = t_groups,
    n_event = "n_event_arm1",
    n_arm = "n_arm1",
    event_name = "event_name",
    study_id = "study_id"
  )
#> # A tibble: 4 × 6
#>     src study_id term_name             n_event_arm1 n_arm1 details_n_event_arm1 
#>   <int> <chr>    <chr>                        <dbl>  <dbl> <chr>                
#> 1     1 NCT1     myocardial_infarction            2     20 myocardial infarctio…
#> 2     1 NCT2     myocardial_infarction            5     27 myocardial infarctio…
#> 3     1 NCT2     skin                            10     27 eczema (10)          
#> 4     2 NCT3     skin                            20     50 pruritus (20)
```

## Website, vignettes

The best place to browse the package documentation is [the
website](https://pharmacologie-caen.github.io/safetymeta/)

You can also look at the “basic_workflow” vignette.
