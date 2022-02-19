# setup

library(tidyverse)
library(gtable)
library(Superpower)
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

# Basic f = .1 2-level ANOVA, n = 650.6, more power needed than t-test ______________________________________________________
power_anova_security_f1 <- pwr.anova.test(
  power=.95,
  k = 2,
  f = .1,
  sig.level = .05)


# But we're really looking for an effect we can generate from a mixed design, 
# THEN pass to a independent two-sample study (1b; cohen's d), and other factorial designs (1c)



# only between subjects effect n = 651, same as pwr function. More power needed than t-test______________________________________________

security_design_2bonly <- ANOVA_design(design = "2b",
                                  n = 651, 
                                  mu = c(pretest_d2,base_mean
                                  ), 
                                  sd = base_sd,
                                  # r = .25,
                                  labelnames = c("inequality", "unequal", "equal"
                                  ))


# ggplot2::ggsave(here::here("figures", "power_design_within_d2.png"), height = 3, width =5)

security_powerplot_2bonly <- plot_power(security_design_2bonly, min_n = 20, max_n = 700, 
                                   desired_power = 95)


cell_n_2bonly <- security_powerplot_2bonly$anova_n$n[1]

# cgwtools::resave(cell_n_2bonly, file = here::here("output", "pretest_output.RData"))
# 
# security_powerplot_2bonly$plot_ANOVA
# g1_d2 <- ggplotGrob(security_powerplot_2bonly$plot_ANOVA)
# 
# gtable_show_layout(g1_d2)
# 
# grid_ineq_extract_d2 <- g1_d2[c(7,12:16),]
# 
# ineq_grid_2bonly  <- grid.arrange(grid_ineq_extract_d2)
# 


# f = .1001; consistent with cohen's f = d/2 conversion
security_exact_2bonly <- ANOVA_exact(security_design_2bonly, 
                                alpha = 0.05)



#mixed model with no correlation like what seems to be in the pretest _______________________________________

security_design_0 <- ANOVA_design(design = "2b*2w",
                                  n = 326, 
                                  mu = c(pretest_d2,pretest_d2, 
                                         # pretest_d2, pretest_d2, 
                                         # base_mean, base_mean, 
                                         base_mean, base_mean
                                  ), 
                                  sd = base_sd,
                                  # r = .25,
                                  labelnames = c("inequality", "unequal", "equal",
                                                 "stake", "histake", "lostake"
                                                 
                                                 # , "prob", "hi prob", "lo prob"
                                                 # ,"price" , "hi", "lo"
                                  ))


# ggplot2::ggsave(here::here("figures", "power_design_within_d2.png"), height = 3, width =5)

security_powerplot_0 <- plot_power(security_design_0, min_n = 20, max_n = 400, 
                                   desired_power = 95)


cell_n_0 <- security_powerplot_0$anova_n$n[1]

# f = .1416; quite a bit larger than straight 2b study
security_exact_0 <- ANOVA_exact(security_design_0, 
                                alpha = 0.05)


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

security_powerplot_2b2w_f1156_5$plot_ANOVA
g1_d2 <- ggplotGrob(security_powerplot_2b2w_f1156_5$plot_ANOVA)

gtable_show_layout(g1_d2)

grid_ineq_extract_d2 = g1_d2[c(1:7,12:16),]

ineq_grid2b2w_f1156_5  <- grid.arrange(grid_ineq_extract_d2)
# Export
ggsave(filename=here::here("figures","security_powerplot_2b2w_f1156_5.png"), plot=ineq_grid2b2w_f1156_5, height = 3)


# f = 0.1156; smaller than .25 correlation, and more importantly - doesn't equate to d = .2
security_exact2b2w_f1156_5 <- ANOVA_exact(security_design_2b2w_f1156_5, 
                                alpha = 0.05)

cgwtools::resave(security_exact2b2w_f1156_5, file = here::here("output", "pretest_output.RData"))


#with .5 correlation and 2x6 design  ____________________________________________________________________

security_design_5_2_6 <- ANOVA_design(design = "2b*6w",
                                  n = 380, 
                                  mu = c(pretest_d2,pretest_d2, 
                                         pretest_d2,pretest_d2, 
                                         pretest_d2,pretest_d2, 
                                         base_mean, base_mean,
                                         base_mean, base_mean, 
                                         base_mean, base_mean), 
                                  sd = base_sd,
                                  r = .5,
                                  labelnames = c("inequality", "unequal", "equal",
                                                 "stake", "lo stakes/loprob/loprice",
                                                 "lo stakes/loprob/hiprice",
                                                 "lo stakes/hiprob",
                                                 "hi stakes/loprob/loprice ", "hi stakes/loprob/hiprice",
                                                 "hi stakes/hiprob"
                                  ))


ggplot2::ggsave(here::here("figures", "security_design_within_5_2_6.png"), height = 3, width =5)

security_powerplot_5_2_6 <- plot_power(security_design_5_2_6, min_n = 20, max_n = 500, 
                                   desired_power = 95)


cell_n_5_2_6 <- security_powerplot_5_2_6$anova_n$n[1]

cgwtools::resave(cell_n_5_2_6, file = here::here("output", "pretest_output.RData"))

security_powerplot_5_2_6$plot_ANOVA
g1_d2 <- ggplotGrob(security_powerplot_5_2_6$plot_ANOVA)

gtable_show_layout(g1_d2)

grid_ineq_extract_d2 = g1_d2[c(0:7,12:16),0:6]

ineq_grid_5_2_6  <- grid.arrange(grid_ineq_extract_d2)
# Export
ggsave(filename=here::here("figures","security_powerplot_within_5_2_6.png"), plot=ineq_grid_5_2_6, height = 3)


# f = .1311; smaller than .25 correlation, and more importantly - doesn't equate to d = .2
security_exact_5_2_6 <- ANOVA_exact(security_design_5_2_6, 
                                alpha = 0.05)

security_exact_5_2b6w_result <- security_exact_5_2_6$main_results

cgwtools::resave(security_exact_5_2b6w_result, file = here::here("output", "pretest_output.RData"))





#with .5 correlation and 2x4 design  _______________________________________

security_design_5_2b4w <- ANOVA_design(design = "2b*4w",
                                      n = 408, 
                                      mu = c(pretest_d2,pretest_d2, 
                                             pretest_d2,pretest_d2, 
                                             base_mean, base_mean,
                                             base_mean, base_mean), 
                                      sd = base_sd,
                                      r = .5,
                                      labelnames = c("inequality", "unequal", "equal",
                                                     "stake", "lo stakes/loprob",
                                                     "lo stakes/hiprob",
                                                     "hi stakes/loprob",
                                                     "hi stakes/hiprob"
                                      ))


ggplot2::ggsave(here::here("figures", "security_design_within_5_2b4w.png"), height = 3, width =5)

security_powerplot_5_2b4w <- plot_power(security_design_5_2b4w, min_n = 20, max_n = 500, 
                                       desired_power = 95)


cell_n_5_2b4w <- security_powerplot_5_2b4w$anova_n$n[1]

cgwtools::resave(cell_n_5_2b4w, file = here::here("output", "pretest_output.RData"))

security_powerplot_5_2b4w$plot_ANOVA
g1_d2 <- ggplotGrob(security_powerplot_5_2b4w$plot_ANOVA)

gtable_show_layout(g1_d2)

grid_ineq_extract_d2 = g1_d2[c(0:7,12:16),0:6]

ineq_grid_5_2b4w  <- grid.arrange(grid_ineq_extract_d2)
# Export
ggsave(filename=here::here("figures","security_powerplot_within_5_2b4w.png"), plot=ineq_grid_5_2b4w, height = 3)


# f = .1266
security_exact_5_2b4w <- ANOVA_exact(security_design_5_2b4w, 
                                    alpha = 0.05)

security_exact_5_2b4w_result <- security_exact_5_2b4w$main_results

cgwtools::resave(security_exact_5_2b4w_result, file = here::here("output", "pretest_output.RData"))



#with .5 correlation and 2x4 design, targetting f1  _______________________________________

security_design_5_2b4w_f1 <- ANOVA_design(design = "2b*4w",
                                       n = 653, 
                                       mu = c(.66587,.66587, 
                                              .66587,.66587, 
                                              base_mean, base_mean,
                                              base_mean, base_mean), 
                                       sd = base_sd,
                                       r = .5,
                                       labelnames = c("inequality", "unequal", "equal",
                                                      "stake", "lo stakes/loprob",
                                                      "lo stakes/hiprob",
                                                      "hi stakes/loprob",
                                                      "hi stakes/hiprob"
                                       ))


ggplot2::ggsave(here::here("figures", "security_design_within_5_2b4w_f1.png"), height = 3, width =5)

security_powerplot_5_2b4w_f1 <- plot_power(security_design_5_2b4w_f1, min_n = 20, max_n = 700, 
                                        desired_power = 95)


cell_n_5_2b4w_f1 <- security_powerplot_5_2b4w_f1$anova_n$n[1]

cgwtools::resave(cell_n_5_2b4w_f1, file = here::here("output", "pretest_output.RData"))

security_powerplot_5_2b4w_f1$plot_ANOVA
g1_d2 <- ggplotGrob(security_powerplot_5_2b4w_f1$plot_ANOVA)

gtable_show_layout(g1_d2)

grid_ineq_extract_d2 = g1_d2[c(0:7,12:16),0:6]

ineq_grid_5_2b4w_f1  <- grid.arrange(grid_ineq_extract_d2)
# Export
ggsave(filename=here::here("figures","security_powerplot_within_5_2b4w_f1.png"), plot=ineq_grid_5_2b4w_f1, height = 3)


# f = .9995
security_exact_5_2b4w_f1 <- ANOVA_exact(security_design_5_2b4w_f1, 
                                     alpha = 0.05)

security_exact_5_2b4w_f1_result <- security_exact_5_2b4w_f1$main_results

cgwtools::resave(security_exact_5_2b4w_f1_result, file = here::here("output", "pretest_output.RData"))



#with .5 correlation and 2x6 design, targetting f1  _______________________________________

security_design_5_2b6w_f1 <- ANOVA_design(design = "2b*6w",
                                          n = 652, 
                                          mu = c(.66415,.66415, 
                                                 .66415,.66415, 
                                                 .66415,.66415, 
                                                 base_mean, base_mean,
                                                 base_mean, base_mean, 
                                                 base_mean, base_mean), 
                                          sd = base_sd,
                                          r = .5,
                                          labelnames = c("inequality", "unequal", "equal",
                                                         "stake", "lo stakes/loprob/loprice",
                                                         "lo stakes/loprob/hiprice",
                                                         "lo stakes/hiprob",
                                                         "hi stakes/loprob/loprice ", "hi stakes/loprob/hiprice",
                                                         "hi stakes/hiprob"
                                          ))


ggplot2::ggsave(here::here("figures", "security_design_within_5_2b6w_f1.png"), height = 3, width =5)

security_powerplot_5_2b6w_f1 <- plot_power(security_design_5_2b6w_f1, min_n = 20, max_n = 700, 
                                           desired_power = 95)


cell_n_5_2b6w_f1 <- security_powerplot_5_2b6w_f1$anova_n$n[1]

cgwtools::resave(cell_n_5_2b6w_f1, file = here::here("output", "pretest_output.RData"))

security_powerplot_5_2b6w_f1$plot_ANOVA
g1_d2 <- ggplotGrob(security_powerplot_5_2b6w_f1$plot_ANOVA)

gtable_show_layout(g1_d2)

grid_ineq_extract_d2 = g1_d2[c(0:7,12:16),0:6]

ineq_grid_5_2b6w_f1  <- grid.arrange(grid_ineq_extract_d2)
# Export
ggsave(filename=here::here("figures","security_powerplot_within_5_2b6w_f1.png"), plot=ineq_grid_5_2b6w_f1, height = 3)


# f = .9999
security_exact_5_2b6w_f1 <- ANOVA_exact(security_design_5_2b6w_f1, 
                                        alpha = 0.05)

security_exact_5_2b6w_f1_result <- security_exact_5_2b6w_f1$main_results

cgwtools::resave(security_exact_5_2b6w_f1_result, file = here::here("output", "pretest_output.RData"))







#with .5 correlation; similar to pretest correlation; additionally  allows from conversion from dz = d = 2f  _______________________________________

security_design_5w <- ANOVA_design(design = "2b*2w*2w",
                                  n = 408, 
                                  mu = c(pretest_d2,pretest_d2, 
                                         pretest_d2,pretest_d2,
                                         base_mean, base_mean,
                                         base_mean, base_mean), 
                                  sd = base_sd,
                                  r = .5,
                                  labelnames = c("inequality", "unequal", "equal",
                                                 "stake", "histake", "lostake",
                                                 "prob", "prob50", "prob75"
                                  ))


ggplot2::ggsave(here::here("figures", "security_design_within_5w.png"), height = 3, width =5)

security_powerplot_5w <- plot_power(security_design_5w, min_n = 20, max_n = 700, 
                                   desired_power = 95)


cell_n_5w <- security_powerplot_5w$anova_n$n[1]
# 
# cgwtools::resave(cell_n_5w, file = here::here("output", "pretest_output.RData"))
# 
security_powerplot_5w$plot_ANOVA
g1_d2 <- ggplotGrob(security_powerplot_5w$plot_ANOVA)

gtable_show_layout(g1_d2)

grid_ineq_extract_d2 = g1_d2[c(1:7,20:24),0:7]

ineq_grid_5w  <- grid.arrange(grid_ineq_extract_d2)
# # Export
ggsave(filename=here::here("figures","security_powerplot_within_5w.png"), plot=ineq_grid_5w, height = 3)


# f = 0.1156; smaller than .25 correlation, and more importantly - doesn't equate to d = .2
security_exact_5w <- ANOVA_exact(security_design_5w, 
                                alpha = 0.05)










#with tiny correlation like what seems to be in the pretest; asymmetric correlation table though _______________________________________

security_design_25 <- ANOVA_design(design = "2b*2w",
                                   n = 408, 
                                   mu = c(pretest_d2,pretest_d2, 
                                          base_mean, base_mean), 
                                   sd = base_sd,
                                   r = .25,
                                   labelnames = c("inequality", "unequal", "equal",
                                                  "stake", "histake", "lostake"
                                                  
                                                  # , "prob", "hi prob", "lo prob"
                                                  # ,"price" , "hi", "lo"
                                   ))


# ggplot2::ggsave(here::here("figures", "power_design_within_d2.png"), height = 3, width =5)

security_powerplot_25 <- plot_power(security_design_25, min_n = 20, max_n = 700, 
                                    desired_power = 95)


cell_n_25 <- security_powerplot_25$anova_n$n[1]

# cgwtools::resave(cell_n_25, file = here::here("output", "pretest_output.RData"))

# security_powerplot_25$plot_ANOVA
# g1_d2 <- ggplotGrob(security_powerplot_25$plot_ANOVA)
# 
# gtable_show_layout(g1_d2)
# 
# grid_ineq_extract_d2 = g1_d2[c(7,20:24),]
# 
# ineq_grid_25  <- grid.arrange(grid_ineq_extract_d2)
# # Export
# ggsave(filename=here::here("figures","mixedpowerplot_d2.png"), plot=ineq_grid_25, height = 3)


# f = .1266; smaller than no correlation; as we'll see, larger than r = .8 and .5. Additionally, asymmetric correlation table
# What is happening here?
security_exact_25 <- ANOVA_exact(security_design_25, 
                                 alpha = 0.05)




# matrix with r = .25, attempt to fix correlation table _____________________________________________________

cor_1_25 <- matrix(c(1, .25,
                    .25, 1), nrow=2)

cor_2_25 <- cor_1_25*0

rho_mat_25 <- cbind(rbind(cor_1_25,cor_2_25),
                   rbind(cor_2_25,cor_1_25))



security_design_cormat_25 <- ANOVA_design(design = "2b*2w",
                                         n = 408, 
                                         mu = c(pretest_d2,pretest_d2, 
                                                base_mean, base_mean), 
                                         sd = base_sd,
                                         r = rho_mat_25,
                                         labelnames = c("inequality", "unequal", "equal",
                                                        "stake", "histake", "lostake"
                                                        
                                                        # , "prob", "hi prob", "lo prob"
                                                        # ,"price" , "hi", "lo"
                                         ))


# ggplot2::ggsave(here::here("figures", "power_design_within_d2.png"), height = 3, width =5)

security_powerplot_cormat_25 <- plot_power(security_design_cormat_25, min_n = 20, max_n = 700, 
                                          desired_power = 95)


cell_n_cormat_25 <- security_powerplot_cormat_25$anova_n$n[1]

# cgwtools::resave(cell_n_cormat_25, file = here::here("output", "pretest_output.RData"))

# security_powerplot_cormat_25$plot_ANOVA
# g1_d2 <- ggplotGrob(security_powerplot_cormat_25$plot_ANOVA)
# 
# gtable_show_layout(g1_d2)
# 
# grid_ineq_extract_d2 = g1_d2[c(7,20:24),]
# 
# ineq_grid_cormat_25  <- grid.arrange(grid_ineq_extract_d2)
# Export
# ggsave(filename=here::here("figures","mixedpowerplot_d2.png"), plot=ineq_grid_cormat_25, height = 3)


# again, reduced power, f = .1266; at least it's the same power as the scalar-assigned correlation. 
#Appears to rule out disruptive influence of asymmetric correlatoin matrix... so there's that
security_exact_cormat_25 <- ANOVA_exact(security_design_cormat_25, 
                                       alpha = 0.05)



# targeting value consistent with cohen's d = .2, correlation =.56  ______________________________________________________

# from pretest, ICC is .56, use as within-factor correlation

security_design_mixed_d2 <- ANOVA_design(design = "2b*2w*2w",
                                         n = 436, 
                                         mu = c(pretest_d2,pretest_d2, pretest_d2, pretest_d2, 
                                                base_mean, base_mean, base_mean, base_mean), 
                                         sd = base_sd, 
                                         r = pretest_icc$ICC_adjusted,
                                         labelnames = c("inequality", "yes", "no", 
                                                        "stake", "hi", "lo"
                                                        , "prob", "hi prob", "lo prob"
                                                        # ,"price" , "hi", "lo"
                                         ))


ggplot2::ggsave(here::here("figures", "power_design_within_d2.png"), height = 3, width =5)

security_powerplot_mixed_d2 <- plot_power(security_design_mixed_d2, min_n = 20, max_n = 500, 
                                          desired_power = 95)


cell_n_mixed_d2 <- security_powerplot_mixed_d2$anova_n$n[1]

cgwtools::resave(cell_n_mixed_d2, file = here::here("output", "pretest_output.RData"))
# 
# security_powerplot_mixed_d2$plot_ANOVA
# g1_d2 <- ggplotGrob(security_powerplot_mixed_d2$plot_ANOVA)
# 
# gtable_show_layout(g1_d2)
# 
# grid_ineq_extract_d2 = g1_d2[c(7,20:24),]
# 
# ineq_grid_mixed_d2  <- grid.arrange(grid_ineq_extract_d2)
# # Export
# ggsave(filename=here::here("figures","mixedpowerplot_d2.png"), plot=ineq_grid_mixed_d2, height = 3)


#cohens f = .1224
security_exact_mixed_d2 <- ANOVA_exact(security_design_mixed_d2, 
                                       alpha = 0.05)

cgwtools::resave(security_exact_mixed_d2, file = here::here("output", "pretest_output.RData"))


# So a mixed model doesn't seem to be much more efficient here. 
# Gives almost exactly the same amount of power as a regular anova:

pwr.anova.test(
  power=.95,
  k = 2,
  f = 0.1224,    #yields n=434
  sig.level = .05)




# Cohen's f =.1 (treatment mean = .668) seems to be smaller than a cohen's d = .2 (treatment mean of .6795). 

# In addition, mixed methods aren't giving us much power benefit
# Something is fishy here...

# Best guess is the use of mixed effects, especially the correlation

# without correlation, power is achieved at n = 164 (vs. n=436), and the effect is changed from f = .1224 to f = .2

# This tells us that the within-factor correlation of .56 is giving us a smaller effect and less power, 
#but the documentation seems to show that larger correlations lead to greater power?


# so it's all seeming to be the same, and not working the way it should - 
# why is a higher correlation reducing power instead of increasing it?
# best guess is that the pattern of means is causing issues? or I shouldn't


# after some further testing - it is definitely the sole between-subjects-effect that is responsible
#for the changing the relationship between correlation and effect size/power 

sample_design <- ANOVA_design(
  design = "2w*2b",
  n = 23,
  mu = c(-0.25, 0.25, 0.25,-0.25),
  sd = 1,
  r = .5,
  labelnames = c("age", "old", "young", 
                 "color", "blue", "red")
)

sample_result <- ANOVA_exact(sample_design,
                                 alpha_level = .05,
                                 verbose = FALSE)

sample_design_flip <- ANOVA_design(
  design = "2b*2w",
  n = 23,
  mu = c(0.25,-0.25,-0.25, 0.25),
  sd = 1,
  r = .5,
  labelnames = c("color", "blue", "red", 
                 "within", "old", "young")
)

sample_result_flip <- ANOVA_exact(sample_design_flip,
                            alpha_level = .05,
                            verbose = FALSE)


sample_design_flip_7 <- ANOVA_design(
  design = "2b*2w",
  n = 23,
  mu = c(0.25,-0.25,-0.25, 0.25),
  sd = 1,
  r = .7,
  labelnames = c("color", "blue", "red", 
                 "within", "old", "young")
)

sample_result_flip_7 <- ANOVA_exact(sample_design_flip_7,
                                 alpha_level = .05,
                                 verbose = FALSE)


# try with between effect

sample_design_flip_btwn <- ANOVA_design(
  design = "2b*2w",
  n = 23,
  mu = c(0.25, 0.25,-0.25,-0.25),
  sd = 1,
  r = .5,
  labelnames = c("color", "blue", "red", 
                 "within", "old", "young")
)

#f = 0.2952
sample_result_flip_btwn <- ANOVA_exact(sample_design_flip_btwn,
                                 alpha_level = .05,
                                 verbose = FALSE)


 
sample_design_flip_btwn_7 <- ANOVA_design(
  design = "2b*2w",
  n = 23,
  mu = c(0.25, 0.25,-0.25,-0.25),
  sd = 1,
  r = .7,
  labelnames = c("color", "blue", "red", 
                 "within", "old", "young")
)

#f 0.2773
sample_result_flip_btwn_7 <- ANOVA_exact(sample_design_flip_btwn_7,
                                   alpha_level = .05,
                                   verbose = FALSE)


# so we can replicate the effect size decrease of the correlation


sample_design_btwn <- ANOVA_design(
  design = "2w*2b",
  n = 23,
  mu = c(0.25, -0.25, 0.25,-0.25),
  sd = 1,
  r = .5,
  labelnames = c("within", "old", "young", 
                 "color", "blue", "red")
)

# f = 0.2952
sample_result_btwn <- ANOVA_exact(sample_design_btwn,
                             alpha_level = .05,
                             verbose = FALSE)

sample_design_btwn_7 <- ANOVA_design(
  design = "2w*2b",
  n = 23,
  mu = c(0.25, -0.25, 0.25,-0.25),
  sd = 1,
  r = .7,
  labelnames = c("within", "old", "young", 
                 "color", "blue", "red")
)

# f = 0.2773; exact same thing as 2b*2w
sample_result_btwn_7 <- ANOVA_exact(sample_design_btwn_7,
                                  alpha_level = .05,
                                  verbose = FALSE)


# go for extreme; r = .99
sample_design_btwn_99 <- ANOVA_design(
  design = "2w*2b",
  n = 23,
  mu = c(0.25, -0.25, 0.25,-0.25),
  sd = 1,
  r = .99,
  labelnames = c("within", "old", "young", 
                 "color", "blue", "red")
)

# f = 0.2563;
sample_result_btwn_99 <- ANOVA_exact(sample_design_btwn_99,
                                    alpha_level = .05,
                                    verbose = FALSE)


# go for extreme; r = .0
sample_design_btwn_0 <- ANOVA_design(
  design = "2w*2b",
  n = 23,
  mu = c(0.25, -0.25, 0.25,-0.25),
  sd = 1,
  r = .0,
  labelnames = c("within", "old", "young", 
                 "color", "blue", "red")
)

# f = 0.3615;
sample_result_btwn_0 <- ANOVA_exact(sample_design_btwn_0,
                                     alpha_level = .05,
                                     verbose = FALSE)

# before we break, a guess: for solely between-subjects effects - the effect comes from being able to distinguish between
# the conditions - introducing a correlation 





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

security_2b2b_f01_powerplot$plot_ANOVA
g1_d2 <- ggplotGrob(security_2b2b_f01_powerplot$plot_ANOVA)

gtable_show_layout(g1_d2)

grid_ineq_extract_d2 = g1_d2[c(0:6,10:16),]

ineq_grid_5  <- grid.arrange(grid_ineq_extract_d2)
# Export
ggsave(filename=here::here("figures","security_powerplot_2b2b_f01.png"), plot=ineq_grid_5, height = 3)

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
