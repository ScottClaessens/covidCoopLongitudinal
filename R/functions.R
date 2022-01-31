# custom functions

# load and wrangle data
loadData <- function(dataFile) {
  read_csv(dataFile) %>%
    select_at(vars(starts_with("NBTNeigh.") | 
                   starts_with("NBTHumanity.") |
                   starts_with("WillingnessNeigh.") |
                   starts_with("WillingnessHumanity.") |
                   starts_with("PFINeighEmpathy.") |
                   starts_with("PFIHumanityEmpathy.") |
                   starts_with("PFINeighSharedFate.") |
                   starts_with("PFIHumanitySharedFate.") |
                   starts_with("Cohort"))) %>%
    mutate(id = 1:nrow(.)) %>%
    pivot_longer(
      cols = c(!id:Cohort), 
      names_pattern = "(.*)\\.(.*)", 
      names_to = c(".value", "time")
      ) %>%
    mutate(time = (as.numeric(time) - 1) / max(as.numeric(time) - 1))
}

# fit no change model with random intercept
fitModel0 <- function(d, var) gam(formula(paste0(var, " ~ 1 + s(id, bs = 're')")), data = d)

# fit linear change model with random intercept
fitModel1 <- function(d, var) gam(formula(paste0(var, " ~ 1 + time + s(id, bs = 're')")), data = d)

# fit spline model with random intercept
fitModel2 <- function(d, var) gam(formula(paste0(var, " ~ 1 + s(time) + s(id, bs = 're')")), data = d)

# plot posterior predictions
plotSpline <- function(m1.1, m1.2, m2.1, m2.2,
                       m3.1, m3.2, m4.1, m4.2,
                       m5.1, m5.2, m6.1, m6.2,
                       m7.1, m7.2, m8.1, m8.2,
                       d, title, file) {
  # plot function
  plotFun <- function(m1, m2) {
    # get straight line
    linear <- plot_smooths(model = m1, series = time)
    # plot spline
    plot_smooths(model = m2, series = time) +
      geom_line(data = linear$data, linetype = "dashed", alpha = 0.5) +
      scale_y_continuous(limits = c(0.9, 7.1), breaks = 1:7) +
      scale_x_continuous(name = "Timepoint",
                         labels = function(x) (13*x) + 1, 
                         breaks = c(0:13) / 13) +
      theme_classic() +
      theme(axis.text.x = element_text(size = 7))
  }
  # make plots
  p1 <- plotFun(m1.1, m1.2)
  p2 <- plotFun(m2.1, m2.2)
  p3 <- plotFun(m3.1, m3.2)
  p4 <- plotFun(m4.1, m4.2)
  p5 <- plotFun(m5.1, m5.2)
  p6 <- plotFun(m6.1, m6.2)
  p7 <- plotFun(m7.1, m7.2)
  p8 <- plotFun(m8.1, m8.2)
  # put together
  out <- plot_grid(p1, p2, p3, p4, p5, p6, p7, p8, nrow = 2)
  out <- plot_grid(ggdraw() + draw_label(title, x = 0.15), 
                   out, nrow = 2, rel_heights = c(0.1, 1))
  # save
  ggsave(out, filename = file, height = 4.5, width = 8)
  return(out)
}

# fit discrete time model
fitModel3 <- function(d, var) {
  # discrete time
  d$timeDiscrete <- ifelse(d$time == 0, "1", 
                           ifelse(d$time == (1/13), "2", 
                                  ifelse(d$time == (2/13), "3", "4+")))
  # formula
  formula <- formula(paste0(var, " ~ 1 + timeDiscrete + (1 | id)"))
  # fit model
  lmer(formula = formula, data = d)
}

# plot discrete time models
plotDiscrete <- function(m1.3, m2.3, m3.3, m4.3, 
                         m5.3, m6.3, m7.3, m8.3, 
                         title, file) {
  # generic plotting function
  plotFun <- function(model, var) {
    # calculate contrasts
    con <-
      emmeans(model, "timeDiscrete") %>%
      contrast('tukey') %>%
      broom::tidy()
    # plot
    p <-
      ggpredict(model, terms = "timeDiscrete") %>%
      ggplot(aes(x = x, y = predicted, group = 1)) +
      geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2) +
      geom_point(size = 0.3) +
      scale_y_continuous(name = var, limits = c(0.9, 7.1), breaks = 1:7) +
      xlab("Timepoint") +
      theme_classic()
    # add contrasts
    y <- 7
    if (con$adj.p.value[3] < 0.05) {p <- p + geom_path(data = data.frame(x = c("1", "4+"), predicted = rep(y, 2)), colour = "lightgrey", size = 0.3); y <- y - 0.2}
    if (con$adj.p.value[2] < 0.05) {p <- p + geom_path(data = data.frame(x = c("1", "3" ), predicted = rep(y, 2)), colour = "lightgrey", size = 0.3); y <- y - 0.2}
    if (con$adj.p.value[5] < 0.05) {p <- p + geom_path(data = data.frame(x = c("2", "4+"), predicted = rep(y, 2)), colour = "lightgrey", size = 0.3); y <- y - 0.2}
    if (con$adj.p.value[1] < 0.05) {p <- p + geom_path(data = data.frame(x = c("1", "2" ), predicted = rep(y, 2)), colour = "lightgrey", size = 0.3); y <- y - 0.2}
    if (con$adj.p.value[4] < 0.05) {p <- p + geom_path(data = data.frame(x = c("2", "3" ), predicted = rep(y, 2)), colour = "lightgrey", size = 0.3); y <- y - 0.2}
    if (con$adj.p.value[6] < 0.05) {p <- p + geom_path(data = data.frame(x = c("3", "4+"), predicted = rep(y, 2)), colour = "lightgrey", size = 0.3); y <- y - 0.2}
    if (sum(con$adj.p.value < 0.05) > 0) p <- p + geom_text(data = data.frame(x = 2.5, predicted = 7.1), label = "*", colour = "lightgrey")
  }
  # individual plots
  p1 <- plotFun(m1.3, var = "NBTNeigh")
  p2 <- plotFun(m2.3, var = "NBTHumanity")
  p3 <- plotFun(m3.3, var = "WillingnessNeigh")
  p4 <- plotFun(m4.3, var = "WillingnessHumanity")
  p5 <- plotFun(m5.3, var = "PFINeighEmpathy")
  p6 <- plotFun(m6.3, var = "PFIHumanityEmpathy")
  p7 <- plotFun(m7.3, var = "PFINeighSharedFate")
  p8 <- plotFun(m8.3, var = "PFIHumanitySharedFate")
  # put together
  out <- plot_grid(p1, p2, p3, p4, p5, p6, p7, p8, nrow = 2)
  out <- plot_grid(ggdraw() + draw_label(title, x = 0.2), 
                   out, nrow = 2, rel_heights = c(0.1, 1))
  # save
  ggsave(out, filename = file, height = 4.5, width = 6)
  return(out)
}
