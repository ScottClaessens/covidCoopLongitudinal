library(targets)
library(tarchetypes)
source("R/functions.R")
options(tidyverse.quiet = TRUE)
tar_option_set(packages = c("cowplot", "emmeans", "ggeffects", 
                            "lme4", "mgcv", "tidymv", "tidyverse"))
# workflow
list(
  # data file
  tar_target(dataFile, "data/Cohorts 1&2 -- T1-T14 - new covid.csv", format = "file"),
  tar_target(d, loadData(dataFile)),
  
  #### full dataset
  
  # fit discrete time models
  tar_target(m1.1, fitModel(d, var = "NBTNeigh")),
  tar_target(m1.2, fitModel(d, var = "NBTHumanity")),
  tar_target(m1.3, fitModel(d, var = "WillingnessNeigh")),
  tar_target(m1.4, fitModel(d, var = "WillingnessHumanity")),
  tar_target(m1.5, fitModel(d, var = "PFINeighEmpathy")),
  tar_target(m1.6, fitModel(d, var = "PFIHumanityEmpathy")),
  tar_target(m1.7, fitModel(d, var = "PFINeighSharedFate")),
  tar_target(m1.8, fitModel(d, var = "PFIHumanitySharedFate")),
  # plots
  tar_target(plot1, plotDiscrete(m1.1, m1.2, m1.3, m1.4,
                                 m1.5, m1.6, m1.7, m1.8,
                                 title = "Cohorts 1 and 2 (n = 1018)",
                                 file = "figures/plotDiscrete_cohorts1and2.pdf")),
  
  #### cohort 1 only
  
  # cohort 1 data only
  tar_target(dC1, filter(d, Cohort == 1)),
  # fit discrete time models
  tar_target(m2.1, fitModel(dC1, var = "NBTNeigh")),
  tar_target(m2.2, fitModel(dC1, var = "NBTHumanity")),
  tar_target(m2.3, fitModel(dC1, var = "WillingnessNeigh")),
  tar_target(m2.4, fitModel(dC1, var = "WillingnessHumanity")),
  tar_target(m2.5, fitModel(dC1, var = "PFINeighEmpathy")),
  tar_target(m2.6, fitModel(dC1, var = "PFIHumanityEmpathy")),
  tar_target(m2.7, fitModel(dC1, var = "PFINeighSharedFate")),
  tar_target(m2.8, fitModel(dC1, var = "PFIHumanitySharedFate")),
  # plots
  tar_target(plot2, plotDiscrete(m2.1, m2.2, m2.3, m2.4,
                                 m2.5, m2.6, m2.7, m2.8,
                                 title = "Cohort 1 only (n = 512)",
                                 file = "figures/plotDiscrete_cohort1only.pdf"))
)
