# COVID-19 Longitudinal

Spline and regression discontinuity models for the longitudinal cooperation COVID-19 project.

## Getting Started

### Installing

To run this code, you will need to [install R](https://www.r-project.org/) and the following R packages:

```
install.packages(c("cowplot", "ggeffects", "lme4", "mgcv", "targets", "tarchetypes", "tidymv", "tidyverse"))
```

### Executing code

1. Set the working directory to this code repository
2. Load the `targets` package with `library(targets)`
3. To run all analyses, run `tar_make()`
4. To load individual targets into your environment, run `tar_load(targetName)`

## Help

Any issues, please email scott.claessens@gmail.com.

## Authors

Scott Claessens, scott.claessens@gmail.com
