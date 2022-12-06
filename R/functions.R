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

# fit discrete time model
fitModel <- function(d, var) {
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
plotDiscrete <- function(m1, m2, m3, m4, 
                         m5, m6, m7, m8, 
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
  p1 <- plotFun(m1, var = "NBTNeigh")
  p2 <- plotFun(m2, var = "NBTHumanity")
  p3 <- plotFun(m3, var = "WillingnessNeigh")
  p4 <- plotFun(m4, var = "WillingnessHumanity")
  p5 <- plotFun(m5, var = "PFINeighEmpathy")
  p6 <- plotFun(m6, var = "PFIHumanityEmpathy")
  p7 <- plotFun(m7, var = "PFINeighSharedFate")
  p8 <- plotFun(m8, var = "PFIHumanitySharedFate")
  # put together
  out <- plot_grid(p1, p2, p3, p4, p5, p6, p7, p8, nrow = 2)
  out <- plot_grid(ggdraw() + draw_label(title, x = 0.2), 
                   out, nrow = 2, rel_heights = c(0.1, 1))
  # save
  ggsave(out, filename = file, height = 4.5, width = 6)
  return(out)
}
