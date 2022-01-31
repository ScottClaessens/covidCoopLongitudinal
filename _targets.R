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
  
  # fit spline models
  tar_target(m1.1.0, fitModel0(d, var = "NBTNeigh")),
  tar_target(m1.1.1, fitModel1(d, var = "NBTNeigh")),
  tar_target(m1.1.2, fitModel2(d, var = "NBTNeigh")),
  tar_target(m1.2.0, fitModel0(d, var = "NBTHumanity")),
  tar_target(m1.2.1, fitModel1(d, var = "NBTHumanity")),
  tar_target(m1.2.2, fitModel2(d, var = "NBTHumanity")),
  tar_target(m1.3.0, fitModel0(d, var = "WillingnessNeigh")),
  tar_target(m1.3.1, fitModel1(d, var = "WillingnessNeigh")),
  tar_target(m1.3.2, fitModel2(d, var = "WillingnessNeigh")),
  tar_target(m1.4.0, fitModel0(d, var = "WillingnessHumanity")),
  tar_target(m1.4.1, fitModel1(d, var = "WillingnessHumanity")),
  tar_target(m1.4.2, fitModel2(d, var = "WillingnessHumanity")),
  tar_target(m1.5.0, fitModel0(d, var = "PFINeighEmpathy")),
  tar_target(m1.5.1, fitModel1(d, var = "PFINeighEmpathy")),
  tar_target(m1.5.2, fitModel2(d, var = "PFINeighEmpathy")),
  tar_target(m1.6.0, fitModel0(d, var = "PFIHumanityEmpathy")),
  tar_target(m1.6.1, fitModel1(d, var = "PFIHumanityEmpathy")),
  tar_target(m1.6.2, fitModel2(d, var = "PFIHumanityEmpathy")),
  tar_target(m1.7.0, fitModel0(d, var = "PFINeighSharedFate")),
  tar_target(m1.7.1, fitModel1(d, var = "PFINeighSharedFate")),
  tar_target(m1.7.2, fitModel2(d, var = "PFINeighSharedFate")),
  tar_target(m1.8.0, fitModel0(d, var = "PFIHumanitySharedFate")),
  tar_target(m1.8.1, fitModel1(d, var = "PFIHumanitySharedFate")),
  tar_target(m1.8.2, fitModel2(d, var = "PFIHumanitySharedFate")),
  # spline model comparison
  tar_target(compare1.1a, AIC(m1.1.0, m1.1.1, m1.1.2)),
  tar_target(compare1.2a, AIC(m1.2.0, m1.2.1, m1.2.2)),
  tar_target(compare1.3a, AIC(m1.3.0, m1.3.1, m1.3.2)),
  tar_target(compare1.4a, AIC(m1.4.0, m1.4.1, m1.4.2)),
  tar_target(compare1.5a, AIC(m1.5.0, m1.5.1, m1.5.2)),
  tar_target(compare1.6a, AIC(m1.6.0, m1.6.1, m1.6.2)),
  tar_target(compare1.7a, AIC(m1.7.0, m1.7.1, m1.7.2)),
  tar_target(compare1.8a, AIC(m1.8.0, m1.8.1, m1.8.2)),
  # fit discrete time models
  tar_target(m1.1.3, fitModel3(d, var = "NBTNeigh")),
  tar_target(m1.2.3, fitModel3(d, var = "NBTHumanity")),
  tar_target(m1.3.3, fitModel3(d, var = "WillingnessNeigh")),
  tar_target(m1.4.3, fitModel3(d, var = "WillingnessHumanity")),
  tar_target(m1.5.3, fitModel3(d, var = "PFINeighEmpathy")),
  tar_target(m1.6.3, fitModel3(d, var = "PFIHumanityEmpathy")),
  tar_target(m1.7.3, fitModel3(d, var = "PFINeighSharedFate")),
  tar_target(m1.8.3, fitModel3(d, var = "PFIHumanitySharedFate")),
  # plots
  tar_target(plot1, plotSpline(m1.1.1, m1.1.2, m1.2.1, m1.2.2,
                               m1.3.1, m1.3.2, m1.4.1, m1.4.2,
                               m1.5.1, m1.5.2, m1.6.1, m1.6.2,
                               m1.7.1, m1.7.2, m1.8.1, m1.8.2,
                               d, title = "Cohorts 1 and 2 (n = 1018)", 
                               file = "figures/plotSpline_cohorts1and2.pdf")),
  tar_target(plot2, plotDiscrete(m1.1.3, m1.2.3, m1.3.3, m1.4.3,
                                 m1.5.3, m1.6.3, m1.7.3, m1.8.3,
                                 title = "Cohorts 1 and 2 (n = 1018)",
                                 file = "figures/plotDiscrete_cohorts1and2.pdf")),
  
  #### cohort 1 only
  
  # cohort 1 data only
  tar_target(dC1, filter(d, Cohort == 1)),
  # fit spline models
  tar_target(m2.1.0, fitModel0(dC1, var = "NBTNeigh")),
  tar_target(m2.1.1, fitModel1(dC1, var = "NBTNeigh")),
  tar_target(m2.1.2, fitModel2(dC1, var = "NBTNeigh")),
  tar_target(m2.2.0, fitModel0(dC1, var = "NBTHumanity")),
  tar_target(m2.2.1, fitModel1(dC1, var = "NBTHumanity")),
  tar_target(m2.2.2, fitModel2(dC1, var = "NBTHumanity")),
  tar_target(m2.3.0, fitModel0(dC1, var = "WillingnessNeigh")),
  tar_target(m2.3.1, fitModel1(dC1, var = "WillingnessNeigh")),
  tar_target(m2.3.2, fitModel2(dC1, var = "WillingnessNeigh")),
  tar_target(m2.4.0, fitModel0(dC1, var = "WillingnessHumanity")),
  tar_target(m2.4.1, fitModel1(dC1, var = "WillingnessHumanity")),
  tar_target(m2.4.2, fitModel2(dC1, var = "WillingnessHumanity")),
  tar_target(m2.5.0, fitModel0(dC1, var = "PFINeighEmpathy")),
  tar_target(m2.5.1, fitModel1(dC1, var = "PFINeighEmpathy")),
  tar_target(m2.5.2, fitModel2(dC1, var = "PFINeighEmpathy")),
  tar_target(m2.6.0, fitModel0(dC1, var = "PFIHumanityEmpathy")),
  tar_target(m2.6.1, fitModel1(dC1, var = "PFIHumanityEmpathy")),
  tar_target(m2.6.2, fitModel2(dC1, var = "PFIHumanityEmpathy")),
  tar_target(m2.7.0, fitModel0(dC1, var = "PFINeighSharedFate")),
  tar_target(m2.7.1, fitModel1(dC1, var = "PFINeighSharedFate")),
  tar_target(m2.7.2, fitModel2(dC1, var = "PFINeighSharedFate")),
  tar_target(m2.8.0, fitModel0(dC1, var = "PFIHumanitySharedFate")),
  tar_target(m2.8.1, fitModel1(dC1, var = "PFIHumanitySharedFate")),
  tar_target(m2.8.2, fitModel2(dC1, var = "PFIHumanitySharedFate")),
  # spline model comparison
  tar_target(compare2.1a, AIC(m2.1.0, m2.1.1, m2.1.2)),
  tar_target(compare2.2a, AIC(m2.2.0, m2.2.1, m2.2.2)),
  tar_target(compare2.3a, AIC(m2.3.0, m2.3.1, m2.3.2)),
  tar_target(compare2.4a, AIC(m2.4.0, m2.4.1, m2.4.2)),
  tar_target(compare2.5a, AIC(m2.5.0, m2.5.1, m2.5.2)),
  tar_target(compare2.6a, AIC(m2.6.0, m2.6.1, m2.6.2)),
  tar_target(compare2.7a, AIC(m2.7.0, m2.7.1, m2.7.2)),
  tar_target(compare2.8a, AIC(m2.8.0, m2.8.1, m2.8.2)),
  # fit discrete time models
  tar_target(m2.1.3, fitModel3(dC1, var = "NBTNeigh")),
  tar_target(m2.2.3, fitModel3(dC1, var = "NBTHumanity")),
  tar_target(m2.3.3, fitModel3(dC1, var = "WillingnessNeigh")),
  tar_target(m2.4.3, fitModel3(dC1, var = "WillingnessHumanity")),
  tar_target(m2.5.3, fitModel3(dC1, var = "PFINeighEmpathy")),
  tar_target(m2.6.3, fitModel3(dC1, var = "PFIHumanityEmpathy")),
  tar_target(m2.7.3, fitModel3(dC1, var = "PFINeighSharedFate")),
  tar_target(m2.8.3, fitModel3(dC1, var = "PFIHumanitySharedFate")),
  # plots
  tar_target(plot3, plotSpline(m2.1.1, m2.1.2, m2.2.1, m2.2.2,
                               m2.3.1, m2.3.2, m2.4.1, m2.4.2,
                               m2.5.1, m2.5.2, m2.6.1, m2.6.2,
                               m2.7.1, m2.7.2, m2.8.1, m2.8.2,
                               d, title = "Cohort 1 only (n = 512)", 
                               file = "figures/plotSpline_cohort1only.pdf")),
  tar_target(plot4, plotDiscrete(m2.1.3, m2.2.3, m2.3.3, m2.4.3,
                                 m2.5.3, m2.6.3, m2.7.3, m2.8.3,
                                 title = "Cohort 1 only (n = 512)",
                                 file = "figures/plotDiscrete_cohort1only.pdf"))
)