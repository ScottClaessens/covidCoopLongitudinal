# custom functions

# load and wrangle data
loadData <- function(dataFile) {
  read_csv(dataFile) %>%
    select_at(vars(starts_with("NBTNeigh.") | 
                     starts_with("NBTHumanity.") |
                     starts_with("WillingnessNeigh.") |
                     starts_with("WillingnessHumanity."))) %>%
    mutate(id = 1:nrow(.)) %>%
    pivot_longer(
      cols = !id, 
      names_pattern = "(.*)\\.(.*)", 
      names_to = c(".value", "time")
      ) %>%
    mutate(time = (as.numeric(time) - 1) / max(as.numeric(time) - 1))
}

# fit no change model
fitModel0 <- function(d, var) {
  # formula
  formula <- bf(paste0(var, " ~ 1 + (1 | id)"))
  # fit model
  brm(formula = formula, data = d, family = cumulative,
      prior = c(prior(normal(0, 2), class = Intercept),
                prior(exponential(2), class = sd)),
      iter = 4000, cores = 4, seed = 2113,
      control = list(adapt_delta = 0.95))
}

# fit linear change model
fitModel1 <- function(d, var) {
  # formula
  formula <- bf(paste0(var, " ~ 1 + time + (1 | id)"))
  # fit model
  brm(formula = formula, data = d, family = cumulative,
      prior = c(prior(normal(0, 2), class = Intercept),
                prior(normal(0, 1), class = b),
                prior(exponential(2), class = sd)),
      iter = 4000, cores = 4, seed = 2113,
      control = list(adapt_delta = 0.95))
}

# fit spline model
fitModel2 <- function(d, var) {
  # formula
  formula <- bf(paste0(var, " ~ 1 + s(time, k = 14) + (1 | id)"))
  # fit model
  brm(formula = formula, data = d, family = cumulative,
      prior = c(prior(normal(0, 2), class = Intercept),
                prior(normal(0, 1), class = b),
                prior(exponential(2), class = sd),
                prior(exponential(3), class = sds)),
      iter = 4000, cores = 4, seed = 2113,
      control = list(adapt_delta = 0.95))
}

# plot posterior predictions
plotPred <- function(m1.1, m1.2, m2.1, m2.2,
                     m3.1, m3.2, m4.1, m4.2) {
  # plot function
  plotFun <- function(m1, m2) {
    # get straight line
    linear <- plot(conditional_effects(m1), plot = FALSE)[[1]]
    # plot spline
    plot(conditional_effects(m2), plot = FALSE, points = TRUE,
         point_args = list(width = 0.04, height = 0.4, alpha = 0.01, size = 0.4))[[1]] +
      geom_line(data = linear$data, linetype = "dashed", alpha = 0.5) +
      scale_y_continuous(limits = c(0.5, 7.5), breaks = 1:7) +
      scale_x_continuous(name = "Timepoint",
                         labels = function(x) (13*x) + 1, 
                         breaks = c(0:13) / 13) +
      theme_classic()
  }
  # make plots
  p1 <- plotFun(m1.1, m1.2)
  p2 <- plotFun(m2.1, m2.2)
  p3 <- plotFun(m3.1, m3.2)
  p4 <- plotFun(m4.1, m4.2)
  # put together
  out <- plot_grid(p1, p2, p3, p4, nrow = 2)
  # save
  ggsave(out, filename = "figures/plotSpline.pdf", height = 6, width = 6)
  return(out)
}

# fit regression discontinuity model
fitModel3 <- function(d, var) {
  # add threshold after t2
  d$thresh <- ifelse(d$time > 0.1, "above", "below")
  # formula
  formula <- bf(paste0(var, " ~ 1 + time + thresh + time:thresh + (1 + time + thresh + time:thresh | id)"))
  # fit model
  brm(formula = formula, data = d, family = cumulative,
      prior = c(prior(normal(0, 2), class = Intercept),
                prior(normal(0, 1), class = b),
                prior(exponential(2), class = sd)),
      iter = 4000, cores = 4, seed = 2113,
      control = list(adapt_delta = 0.95))
}

# plot regression discontinuity
plotRD <- function(m1.3, m2.3, m3.3, m4.3) {
  # generic plotting function
  plotFun <- function(model, name = "") {
    # get fitted values
    new <- data.frame(time = seq(0, 1, length.out = 100))
    new$thresh <- ifelse(new > 0.1, "above", "below")
    f <- fitted(model, newdata = new, re_formula = NA, summary = FALSE)
    # get expected value
    f <- (f[,,1]*1) + (f[,,2]*2) + (f[,,3]*3) + (f[,,4]*4) + (f[,,5]*5) + (f[,,6]*6) + (f[,,7]*7)
    # summarise
    tibble(
      est = apply(f, 2, median),
      low = apply(f, 2, quantile, 0.025),
      upp = apply(f, 2, quantile, 0.975),
      time = new$time
    ) %>%
      ggplot(aes(x = time, y = est, ymin = low, ymax = upp)) +
      geom_ribbon(fill = "grey") +
      geom_line() +
      geom_vline(xintercept = 0.096, colour = "white", size = 2.2) +
      scale_y_continuous(name = name, 
                         limits = c(0.5, 7.5), breaks = 1:7) +
      scale_x_continuous(name = "Timepoint",
                         labels = function(x) (13*x) + 1, 
                         breaks = c(0:13) / 13) +
      theme_classic()
  }
  # individual plots
  pA <- plotFun(m1.3, name = "NBTNeigh")
  pB <- plotFun(m2.3, name = "NBTHumanity")
  pC <- plotFun(m3.3, name = "WillingnessNeigh")
  pD <- plotFun(m4.3, name = "WillingnessHumanity")
  # put together
  out <- plot_grid(pA, pB, pC, pD, nrow = 2)
  # save
  ggsave(out, filename = "figures/plotRD.pdf", height = 6, width = 6)
  return(out)
}
