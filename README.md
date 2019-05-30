
<!-- README.md is generated from README.Rmd. Please edit that file -->

# <img src='img/justifier-logo.png' align="right" height="200" /> justifier: Human and Machine-Readable Justifications and Justified Decisions Based on YAML

<!-- badges: start -->

[![AppVeyor build
status](https://ci.appveyor.com/api/projects/status/github/academy-of-behavior-change/justifier?branch=master&svg=true)](https://ci.appveyor.com/project/academy-of-behavior-change/justifier)
[![Coverage
status](https://codecov.io/gh/academy-of-behavior-change/justifier/branch/master/graph/badge.svg)](https://codecov.io/github/academy-of-behavior-change/justifier?branch=master)

<!-- badges: end -->

The goal of `justifier` is to provide a simple human- and
machine-readable standard for documenting justifications for decisions.
`justifier` was primarily developed to enable documenting the
development of behavior change interventions and the planning and
execution of scientific studies, but it can also be used to document
decisions in organisations, enabling accumulation of decisions and types
of justifications over time.

The pkgdown website for this project is located at
<https://r-packages.gitlab.io/justifier>, and three vignettes (in
progress) are located at:

  - <https://r-packages.gitlab.io/justifier/articles/general-introduction-to-justifier.html>
  - <https://r-packages.gitlab.io/justifier/articles/justifier-in-study-design.html>
  - <https://r-packages.gitlab.io/justifier/articles/justifier-in-intervention-development.html>

This meets the increasing demand for accountability of professionals
(see e.g. Van Woerkum & Aarts, 2012).

## Installation

You can install the released version of `justifier` from
[CRAN](https://CRAN.R-project.org) with:

``` r
stop("Not ran");
install.packages("justifier");
```

You can install the development version of `justifier` from
[GitLab](https://gitlab.com) with:

``` r
devtools::install_gitlab("r-packages/justifier");
```

(assuming you have `devtools` installed; otherwise, install that first
using the `install.packages` function)

## References

van Woerkum, C. and Aarts, N. (2012), ‘Accountability: New challenges,
new forms’, *Journal of Organizational Transformation & Social Change*,
9, pp. 271–283, .
