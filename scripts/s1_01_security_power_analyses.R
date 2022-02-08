# setup

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
  # n = 650.6,
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

# cgwtools::resave(cell_n_0, file = here::here("output", "pretest_output.RData"))

# security_powerplot_0$plot_ANOVA
# g1_d2 <- ggplotGrob(security_powerplot_0$plot_ANOVA)
# 
# gtable_show_layout(g1_d2)
# 
# grid_ineq_extract_d2 <- g1_d2[c(7,12:16),]
# 
# ineq_grid_0  <- grid.arrange(grid_ineq_extract_d2)

# # Export
# ggsave(filename=here::here("figures","mixedpowerplot_d2.png"), plot=ineq_grid_0, height = 3)


# f = .1416; quite a bit larger than straight 2b study
security_exact_0 <- ANOVA_exact(security_design_0, 
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




#with .5 correlation like what allows from conversion from dz = d = 2f  _______________________________________

security_design_5 <- ANOVA_design(design = "2b*2w",
                                   n = 489, 
                                   mu = c(pretest_d2,pretest_d2, 
                                          base_mean, base_mean), 
                                   sd = base_sd,
                                   r = .5,
                                   labelnames = c("inequality", "unequal", "equal",
                                                  "stake", "histake", "lostake"
                                                  
                                                  # , "prob", "hi prob", "lo prob"
                                                  # ,"price" , "hi", "lo"
                                   ))


# ggplot2::ggsave(here::here("figures", "power_design_within_d2.png"), height = 3, width =5)

security_powerplot_5 <- plot_power(security_design_5, min_n = 20, max_n = 700, 
                                    desired_power = 95)


cell_n_5 <- security_powerplot_5$anova_n$n[1]

# cgwtools::resave(cell_n_5, file = here::here("output", "pretest_output.RData"))

# security_powerplot_5$plot_ANOVA
# g1_d2 <- ggplotGrob(security_powerplot_5$plot_ANOVA)
# 
# gtable_show_layout(g1_d2)
# 
# grid_ineq_extract_d2 = g1_d2[c(7,20:24),]
# 
# ineq_grid_5  <- grid.arrange(grid_ineq_extract_d2)
# # Export
# ggsave(filename=here::here("figures","mixedpowerplot_d2.png"), plot=ineq_grid_5, height = 3)


# f = 0.1156; smaller than .25 correlation
security_exact_5 <- ANOVA_exact(security_design_5, 
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



