library(tidyverse)
library(stevemisc) # for svnorm 
library(dotwhisker)
library(ggtext)

# library(modelr) # for data_grid
# library(ordinal)

source(here::here("scripts", "manuscript_funs.r"), local = knitr::knit_global())

load("C:/Users/dalla/Google Drive/offline_data_files/icvs_pwt_swiid/data/icvs_joined_samples.RData")

load(here::here("output","icvs_output.RData"))

load(file = "C:/Users/dalla/Google Drive/offline_data_files/icvs_pwt_swiid/data/m100_sims.RData")

# https://stat.ethz.ch/pipermail/r-help/2006-May/094765.html

# The sums of
# squares and degrees of freedom for the numerators are calculated as in
# a linear model.  There is a slot in an lmer model that is similar to
# the "effects" component in a lm model and that, along with the
# "assign" attribute for the model matrix provides the numerator of the
# F ratio.  The denominator is the penalized residual sum of squares
# divided by the REML degrees of freedom, which is n-p where n is the
# number of observations and p is the column rank of the model matrix
# for the fixed effects.


# For the time being, I would recommend using a Markov Chain Monte Carlo
# sample (function mcmcsamp) to evaluate the properties of individual
# coefficients (use HPDinterval or just summary from the "coda"
#               package).  Evaluating entire terms is more difficult but you can
# always calculate the F ratio and put a lower bound on the denominator
# degrees of freedom.

# organize sim data as tibble
m100_sims <- m100_sims %>%
  map_df(. %>% as_tibble())

m100_tidy <- tibble(
  term = names(m100_sims),
  estimate = summarise_all(m100_sims, list(mean)) %>%
    t() %>%
    as.vector(),
  std.error = m100_sims %>%
    summarise_all(list(sd)) %>%
    t() %>%
    as.vector()
)

# margin <- qt(0.975,df=n-1)*s/sqrt(n)


# Plot the results
m100_tidy %>%
  by_2sd(mod_joined1) %>%
  relabel_predictors(
    c(
      gini_2004_6_cent = "Income Inequality",
      gdppc_2004_6_scale = "GDP/capita",
      age_cent = "Age",
      employed1 = "Employed",
      male1 = "Male",
      police_effective = "Perceived 
      police effectiveness",
      income_quartile = "Income quartile"
    )
  ) %>%
  dwplot() +
  theme_bw() +
  geom_vline(xintercept = 0,
             colour = "grey60",
             linetype = "dashed") +
  theme(legend.position = "none") +
  labs(title = str_wrap(paste("Predicting crime victimization, max. var. Smaller sample,
                     n = ", format(nrow(mod_joined1),big.mark = ",", scientific = FALSE), 
                              "k = ", 
                              length((summary(as.factor(mod_joined1$country))[0!= summary(as.factor(mod_joined1$country))]))),45) ,
       x = "Coefficient Estimate")

#save the dot whisker plot
ggsave(here::here("figures", "ordinal_dotwhisker.png"),
       height = 3, width = 6)

# create regression result tibble for making a a table
tidy_ci <- m100_tidy %>%
  by_2sd(mod_joined1) %>%
  select(-by_2sd)

tidy_ci$ci95_lo <- tidy_ci$estimate - 1.96*tidy_ci$std.error # = 0.4539815 
tidy_ci$ci95_hi <- tidy_ci$estimate + 1.96*tidy_ci$std.error # = 0.4539815 

tidy_ci$z_score <- tidy_ci$estimate/tidy_ci$std.error #Wald tests
tidy_ci$p_value <-  as.numeric(format(2*pnorm(abs(tidy_ci$z_score), lower.tail=FALSE), 
                                      scientific = FALSE))

tidy_ci$p_value <-  ifelse(tidy_ci$p_value < .001,
                           "< .001",
                           paste(tidy_ci$p_value %>% round(3)))

tidy_ci$CI95 <- paste("[", round(tidy_ci$ci95_lo,3), ", ", 
                      round(tidy_ci$ci95_hi,3),"]",  sep="")
                      
tidy_ci <- round_df(tidy_ci, 3)

tidy_ci <- subset(tidy_ci, select = -c(ci95_lo,ci95_hi))

tidy_ci <- tidy_ci %>% relocate(CI95, .before = z_score)

tidy_ci$df <- format(m_ord_mod_max1$dims$df.residual,big.mark = ",", scientific = FALSE)

tidy_ci <- tidy_ci %>% relocate(df, .before = estimate)

# 
# tidy_ci %>%
#   kbl(booktabs = T) %>%
#   kable_styling(latex_options = c("striped"))


cgwtools::resave(tidy_ci, file = here::here("output","icvs_output.RData"))

# save(tidy_ci, file = here::here("output", "icvs_output.RData"))

# For minimized variable set - mod dataset

load(here::here("output", "m100_min_sims.RData"))


# organize sim data as tibble
m100_min_sims <- m100_min_sims %>%
  map_df(. %>% as_tibble())

m100_min_tidy <- tibble(
  term = names(m100_min_sims),
  estimate = summarise_all(m100_min_sims, list(mean)) %>%
    t() %>%
    as.vector(),
  std.error = m100_min_sims %>%
    summarise_all(list(sd)) %>%
    t() %>%
    as.vector()
)


# Plot the results
m100_min_tidy %>%
  by_2sd(mod_joined1) %>%
  relabel_predictors(
    c(
      gini_2004_6_cent = "Income Inequality",
      gdppc_2004_6_scale = "GDP/capita",
      age_cent = "Age",
      employed1 = "Employed",
      male1 = "Male"
    )
  ) %>%
  dwplot() +
  theme_bw() +
  geom_vline(xintercept = 0,
             colour = "grey60",
             linetype = "dashed") +
  theme(legend.position = "none") +
  labs(title = str_wrap(paste("Predicting crime victimization, min. var. Smaller sample,
                     n = ", format(nrow(mod_joined1),big.mark = ",", scientific = FALSE), 
                     "k = ", 
                     length((summary(as.factor(mod_joined1$country))[0!= summary(as.factor(mod_joined1$country))]))),45) ,
       x = "Coefficient Estimate")

#save the dot whisker plot
ggsave(here::here("figures", "min_ordinal_dotwhisker.png"),
       height = 3, width = 6)


# create regression result tibble for making a a table
min_tidy_ci <- m100_min_tidy %>%
  by_2sd(mod_joined1) %>%
  select(-by_2sd)

min_tidy_ci$ci95_lo <- min_tidy_ci$estimate - 1.96*min_tidy_ci$std.error # = 0.4539815 
min_tidy_ci$ci95_hi <- min_tidy_ci$estimate + 1.96*min_tidy_ci$std.error # = 0.4539815 

min_tidy_ci$z_score <- min_tidy_ci$estimate/min_tidy_ci$std.error #Wald tests
min_tidy_ci$p_value <-  as.numeric(format(2*pnorm(abs(min_tidy_ci$z_score), lower.tail=FALSE), 
                                      scientific = FALSE))

min_tidy_ci$p_value <-  ifelse(min_tidy_ci$p_value < .001,
                           "< .001",
                           paste(min_tidy_ci$p_value %>% round(3)))

min_tidy_ci$CI95 <- paste("[", round(min_tidy_ci$ci95_lo,3), ", ", 
                      round(min_tidy_ci$ci95_hi,3),"]",  sep="")

min_tidy_ci <- round_df(min_tidy_ci, 3)

min_tidy_ci <- subset(min_tidy_ci, select = -c(ci95_lo,ci95_hi))

min_tidy_ci <- min_tidy_ci %>% relocate(CI95, .before = z_score)


min_tidy_ci$df <- format(m_ord_mod1$dims$df.residual,big.mark = ",", scientific = FALSE)

min_tidy_ci <- min_tidy_ci %>% relocate(df, .before = estimate)

cgwtools::resave(min_tidy_ci, file = here::here("output","icvs_output.RData"))



####### for larger dataset (iv)

load(here::here("output","m100_iv_sims.RData"))


# organize sim data as tibble
m100_iv_sims <- m100_iv_sims %>%
map_df(. %>% as_tibble())

m100_iv_tidy <- tibble(
  term = names(m100_iv_sims),
  estimate = summarise_all(m100_iv_sims, list(mean)) %>%
    t() %>%
    as.vector(),
  std.error = m100_iv_sims %>%
    summarise_all(list(sd)) %>%
    t() %>%
    as.vector()
)

# margin <- qt(0.975,df=n-1)*s/sqrt(n)


# Plot the results
m100_iv_tidy %>%
  by_2sd(iv_joined1) %>%
  relabel_predictors(
    c(
      gini_2004_6_cent = "Income Inequality",
      gdppc_2004_6_scale = "GDP/capita",
      age_cent = "Age",
      employed1 = "Employed",
      male1 = "Male"
    )
  ) %>%
  dwplot() +
  theme_bw() +
  geom_vline(xintercept = 0,
             colour = "grey60",
             linetype = "dashed") +
  theme(legend.position = "none") +
  labs(title = str_wrap(paste("Predicting crime victimization, min. var. Larger sample, n = ",
                              format(nrow(iv_joined1),big.mark = ",", scientific = FALSE),
                     "k = ",length((summary(as.factor(iv_joined1$country))[0!= summary(as.factor(iv_joined1$country))]))),45), 
       x = "Coefficient Estimate")

#save the dot whisker plot
ggsave(here::here("figures", "iv_ordinal_dotwhisker.png"),
       height = 3, width = 6)



# create regression result tibble for making a a table
iv_tidy_ci <- m100_iv_tidy %>%
  by_2sd(iv_joined1) %>%
  select(-by_2sd)

iv_tidy_ci$ci95_lo <- iv_tidy_ci$estimate - 1.96*iv_tidy_ci$std.error # = 0.4539815 
iv_tidy_ci$ci95_hi <- iv_tidy_ci$estimate + 1.96*iv_tidy_ci$std.error # = 0.4539815 

iv_tidy_ci$z_score <- iv_tidy_ci$estimate/iv_tidy_ci$std.error #Wald tests
iv_tidy_ci$p_value <-  as.numeric(format(2*pnorm(abs(iv_tidy_ci$z_score), lower.tail=FALSE), 
                                          scientific = FALSE))

iv_tidy_ci$p_value <-  ifelse(iv_tidy_ci$p_value < .001,
                               "< .001",
                               paste(iv_tidy_ci$p_value %>% round(3)))

iv_tidy_ci$CI95 <- paste("[", round(iv_tidy_ci$ci95_lo,3), ", ", 
                          round(iv_tidy_ci$ci95_hi,3),"]",  sep="")

iv_tidy_ci <- round_df(iv_tidy_ci, 3)

iv_tidy_ci <- subset(iv_tidy_ci, select = -c(ci95_lo,ci95_hi))

iv_tidy_ci <- iv_tidy_ci %>% relocate(CI95, .before = z_score)

iv_tidy_ci$df <- format(m_ord_iv1$dims$df.residual,big.mark = ",", scientific = FALSE)

iv_tidy_ci <- iv_tidy_ci %>% relocate(df, .before = estimate)

cgwtools::resave(iv_tidy_ci, file = here::here("output","icvs_output.RData"))

