library(targets)
library(tarchetypes)
source("R/functions.R")
options(tidyverse.quiet = TRUE)
tar_option_set(packages = c("brms", "cowplot", "tidyverse"))
# workflow
list(
  # data file
  tar_target(dataFile, "data/Cohorts 1&2 -- T1-T14 - new covid.csv", format = "file"),
  tar_target(d, loadData(dataFile)),
  # fit spline models
  tar_target(m1.0, fitModel0(d, var = "NBTNeigh")),
  tar_target(m1.1, fitModel1(d, var = "NBTNeigh")),
  tar_target(m1.2, fitModel2(d, var = "NBTNeigh")),
  tar_target(m2.0, fitModel0(d, var = "NBTHumanity")),
  tar_target(m2.1, fitModel1(d, var = "NBTHumanity")),
  tar_target(m2.2, fitModel2(d, var = "NBTHumanity")),
  tar_target(m3.0, fitModel0(d, var = "WillingnessNeigh")),
  tar_target(m3.1, fitModel1(d, var = "WillingnessNeigh")),
  tar_target(m3.2, fitModel2(d, var = "WillingnessNeigh")),
  tar_target(m4.0, fitModel0(d, var = "WillingnessHumanity")),
  tar_target(m4.1, fitModel1(d, var = "WillingnessHumanity")),
  tar_target(m4.2, fitModel2(d, var = "WillingnessHumanity")),
  # spline model comparison
  tar_target(loo1.0, loo(m1.0)),
  tar_target(loo1.1, loo(m1.1)),
  tar_target(loo1.2, loo(m1.2)),
  tar_target(loo2.0, loo(m2.0)),
  tar_target(loo2.1, loo(m2.1)),
  tar_target(loo2.2, loo(m2.2)),
  tar_target(loo3.0, loo(m3.0)),
  tar_target(loo3.1, loo(m3.1)),
  tar_target(loo3.2, loo(m3.2)),
  tar_target(loo4.0, loo(m4.0)),
  tar_target(loo4.1, loo(m4.1)),
  tar_target(loo4.2, loo(m4.2)),
  tar_target(looCompare1a, loo_compare(loo1.0, loo1.1, loo1.2)),
  tar_target(looCompare2a, loo_compare(loo2.0, loo2.1, loo2.2)),
  tar_target(looCompare3a, loo_compare(loo3.0, loo3.1, loo3.2)),
  tar_target(looCompare4a, loo_compare(loo4.0, loo4.1, loo4.2)),
  # fit regression discontinuity models
  tar_target(m1.3, fitModel3(d, var = "NBTNeigh")),
  tar_target(m2.3, fitModel3(d, var = "NBTHumanity")),
  tar_target(m3.3, fitModel3(d, var = "WillingnessNeigh")),
  tar_target(m4.3, fitModel3(d, var = "WillingnessHumanity")),
  # rd model comparison
  tar_target(loo1.3, loo(m1.3)),
  tar_target(loo2.3, loo(m2.3)),
  tar_target(loo3.3, loo(m3.3)),
  tar_target(loo4.3, loo(m4.3)),
  tar_target(looCompare1b, loo_compare(loo1.0, loo1.1, loo1.3)),
  tar_target(looCompare2b, loo_compare(loo2.0, loo2.1, loo2.3)),
  tar_target(looCompare3b, loo_compare(loo3.0, loo3.1, loo3.3)),
  tar_target(looCompare4b, loo_compare(loo4.0, loo4.1, loo4.3)),
  # plots
  tar_target(plot1, plotPred(m1.1, m1.2, m2.1, m2.2,
                             m3.1, m3.2, m4.1, m4.2)),
  tar_target(plot2, plotRD(m1.3, m2.3, m3.3, m4.3))
)