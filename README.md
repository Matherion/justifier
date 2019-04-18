
<!-- README.md is generated from README.Rmd. Please edit that file -->

# <img src='img/justifier-logo.png' align="right" height="200" /> justifier: Human and machine-readable justifications and justified decisions based on YAML

<!-- badges: start -->

[![Travis build
status](https://travis-ci.org/academy-of-behavior-change/justifier.svg?branch=master)](https://travis-ci.org/academy-of-behavior-change/justifier)
[![AppVeyor build
status](https://ci.appveyor.com/api/projects/status/github/academy-of-behavior-change/justifier?branch=master&svg=true)](https://ci.appveyor.com/project/academy-of-behavior-change/justifier)
[![Coverage
status](https://codecov.io/gh/academy-of-behavior-change/justifier/branch/master/graph/badge.svg)](https://codecov.io/github/academy-of-behavior-change/justifier?branch=master)

<!-- badges: end -->

The goal of `justifier` is to provide a simple human- and
machine-readable standard for documenting justifications for decisions.
`justifier` was primarily developed to enable documenting teh
development of behavior change interventions and the planning and
execution of scientific studies, but it can also be used to document
decisions in organisations, enabling accumulation of decisions and types
of justifications over time.

This meets the increasing demand for accountability of professionals
(see e.g. <doi:10.1386/jots.9.3.271_1>).

## Installation

You can install the released version of `justifier` from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("justifier");
```

You can install the development version of `justifier` from
[GitLab](https://gitlab.com) with:

``` r
devtools::install_gitlab("r-packages/justifier");
```

(assuming you have `devtools` installed; otherwise, install that first
using the `install.packages` function)
