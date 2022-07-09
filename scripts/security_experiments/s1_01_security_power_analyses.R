# setup

library(tidyverse)
library(gtable)
library(Superpower)
library(grid)
library(gridExtra)
library(pwr)

set.seed(1234)

load(here::here("output", "pretest_output.RData"))

Superpower_options("plot" = TRUE, "verbose" = FALSE) 

nsims <- 2000

base_mean <- unname(base_mean)

base_sd <- unname(base_sd)

pretest_d2 <- base_mean+base_sd*.2


cgwtools::resave(nsims, base_mean,base_sd, file = here::here("output", "pretest_output.RData"))



# Basic d=.2 t-test,    n=542 ______________________________________________________________________________________
power_ttest_security_d2 <- pwr.t.test(d=.2,power=.95,sig.level=.05,
                                    type="two.sample",alternative="greater")

power_ttest_security_d2_paired <- pwr.t.test(d=.2,power=.95,sig.level=.05,
                                      type="paired",alternative="greater")

power_ttest_security_d4_paired <- pwr.t.test(d=.4,power=.95,sig.level=.05,
                                             type="paired",alternative="greater")

power_ttest_security_d1_paired <- pwr.t.test(d=.1,power=.95,sig.level=.05,
                                             type="paired",alternative="greater")



# Basic f = .1 2-level ANOVA, n = 650.6, more power needed than t-test ______________________________________________________
power_anova_security_f1 <- pwr.anova.test(
  power=.95,
  k = 2,
  f = .1,
  sig.level = .05)


# But we're really looking for an effect we can generate from a mixed design, 
# THEN pass to a independent two-sample study (1b; cohen's d), and other factorial designs (1c)




#with .5 correlation; similar to pretest correlation; additionally  allows from conversion from dz = d = 2f  _______________________________________

security_design_2b2w_f1156_5 <- ANOVA_design(design = "2b*2w",
                                  n = 489, 
                                  mu = c(pretest_d2,pretest_d2, 
                                         base_mean, base_mean), 
                                  sd = base_sd,
                                  r = .5,
                                  labelnames = c("inequality", "unequal", "equal",
                                                 "stake", "histake", "lostake"
                                  ))


ggplot2::ggsave(here::here("figures", "security_design_2b2w_f1156_5.png"), height = 3, width =5)

security_powerplot_2b2w_f1156_5 <- plot_power(security_design_2b2w_f1156_5, min_n = 20, max_n = 500, 
                                   desired_power = 95)

cell_n2b2w_f1156_5 <- security_powerplot_2b2w_f1156_5$anova_n$n[1]

cgwtools::resave(cell_n2b2w_f1156_5, file = here::here("output", "pretest_output.RData"))


# security_powerplot_2b2w_f1156_5$plot_ANOVA

# this adds 'power" as a y axis label for each facet before extraction 
gt = ggplot_gtable(ggplot_build(security_powerplot_2b2w_f1156_5$plot_ANOVA))
which.ylab = grep('ylab-l', gt$layout$name)
which.axes = grep('axis-l', gt$layout$name)
axis.rows  = gt$layout$t[which.axes]
label.col  = gt$layout$l[which.ylab]
gt = gtable::gtable_add_grob(gt, rep(gt$grobs[which.ylab], length(axis.rows)), axis.rows, label.col)
gt = gtable::gtable_filter  (gt, 'ylab-l', invert = TRUE) 

# g1_d2 <- ggplotGrob(security_powerplot_2b2w_f1156_5$plot_ANOVA)
# gtable_show_layout(g1_d2)
# grid_ineq_extract_d2 = g1_d2[c(0:6,10:16),]
# ineq_grid_5 <- grid.arrange(grid_ineq_extract_d2)

gplot_inequality_d2 <- ggplotify::as.ggplot(gt[c(0:7,12:16),]) # extract desired facet

# # Export

gplot_inequality_d2
ggsave(filename=here::here("figures","security_powerplot_2b2w_f1156_5.png"), plot=gplot_inequality_d2,width = 4, height = 3)


# f = 0.1156; smaller than .25 correlation, and more importantly - doesn't equate to d = .2
security_exact2b2w_f1156_5 <- ANOVA_exact(security_design_2b2w_f1156_5, 
                                alpha = 0.05)

cgwtools::resave(security_exact2b2w_f1156_5, file = here::here("output", "pretest_output.RData"))

# Study 1c (meritocracy) 2b*2b cohen's f = .1

# detecting .01 partial η2, with 95% power
security_2b2b_f01_design <- ANOVA_design(design = "2b*2b",
                                   n = 327, 
                                   mu = c(0.7444, base_mean, base_mean, base_mean), 
                                   sd = base_sd, 
                                   # r = 0.8, 
                                   labelnames = c("inequality", "yes", "no", "undeserved", "yes", "no"))


ggplot2::ggsave(here::here("figures", "security_design_2b2b_f01.png"), height = 3, width =5)


security_2b2b_f01_powerplot <- plot_power(security_2b2b_f01_design, min_n = 20, max_n = 500, desired_power = 95)

cell_n_2b2b_f01 <- security_2b2b_f01_powerplot$anova_n$n[1]

cgwtools::resave(cell_n_2b2b_f01, file = here::here("output", "pretest_output.RData"))

gt = ggplot_gtable(ggplot_build(security_2b2b_f01_powerplot$plot_ANOVA))
which.ylab = grep('ylab-l', gt$layout$name)
which.axes = grep('axis-l', gt$layout$name)
axis.rows  = gt$layout$t[which.axes]
label.col  = gt$layout$l[which.ylab]
gt = gtable::gtable_add_grob(gt, rep(gt$grobs[which.ylab], length(axis.rows)), axis.rows, label.col)
gt = gtable::gtable_filter  (gt, 'ylab-l', invert = TRUE) 

gplot_inequality_2b2b <- ggplotify::as.ggplot(gt[c(0:6,10:16),])



gplot_inequality_2b2b



# Export
ggsave(filename=here::here("figures","security_powerplot_2b2b_f01.png"), plot=gplot_inequality_2b2b,width = 4, height = 3)

security_2b2b_f01_exact <- ANOVA_exact(security_2b2b_f01_design, 
                                 alpha = 0.05)

cgwtools::resave(security_2b2b_f01_exact, file = here::here("output", "pretest_output.RData"))





# TOST POWER ANALYSES________________________________________________________________________________


# Mixed ANOVA, pairwise comparisons, repeated measures ____________________________
# e.g., for comparing unequal_histak ~ unequal_lostak

#n = 326, or 652 in 2b*2w
t_tost_2b2w_sample <- power_t_TOST(
  n = NULL,
  delta = 0,
  sd = 1,
  low_eqbound = -.2,
  high_eqbound = .2,
  alpha = .05,
  power = .95,
  type = "paired"
)

#n = 386, or 772 in 2b*2w
t_tost_2b2w_bonf <- power_t_TOST(
  n = NULL,
  delta = 0,
  sd = 1,
  low_eqbound = -.2,
  high_eqbound = .2,
  alpha = .025,
  power = .95,
  type = "paired"
)

# Need to change sd? maybe not since we're looking at cohen's d
