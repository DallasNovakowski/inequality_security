library(tidyverse)
library(stevemisc) # for svnorm 
library(dotwhisker)

# library(modelr) # for data_grid
# library(ordinal)

source(here::here("scripts", "manuscript_funs.r"), local = knitr::knit_global())

load(file = "C:/Users/dalla/Google Drive/offline_data_files/icvs_pwt_swiid/data/m100_sims.RData")

load("C:/Users/dalla/Google Drive/offline_data_files/icvs_pwt_swiid/data/mod_joined1.RData")


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
      police_effective = "Perceived police effectiveness",
      income_quartile = "Income quartile"
    )
  ) %>%
  dwplot() +
  theme_bw() +
  geom_vline(xintercept = 0,
             colour = "grey60",
             linetype = "dashed") +
  theme(legend.position = "none") +
  labs(title = "Predicting crime victimization",
       x = "Coefficient Estimate")

#save the dot whisker plot
ggsave(here::here("figures", "ordinal_dotwhisker.png"),
       height = 3)

# create regression result tibble for making a a table
tidy_ci <- m100_tidy %>%
  by_2sd(mod_joined1) %>%
  select(-by_2sd)

tidy_ci$ci95_lo <- tidy_ci$estimate - 1.96*tidy_ci$std.error # = 0.4539815 
tidy_ci$ci95_hi <- tidy_ci$estimate + 1.96*tidy_ci$std.error # = 0.4539815 

tidy_ci$z_score <- tidy_ci$estimate/tidy_ci$std.error #Wald tests
tidy_ci$p_value <-  as.numeric(format(2*pnorm(abs(tidy_ci$z_score), lower.tail=FALSE), scientific = FALSE))

tidy_ci$p_value <-  ifelse(tidy_ci$p_value < .001,
                           "< .001",
                           paste(tidy_ci$p_value %>% round(3)))

tidy_ci <- round_df(tidy_ci, 3)

# 
# tidy_ci %>%
#   kbl(booktabs = T) %>%
#   kable_styling(latex_options = c("striped"))


cgwtools::resave(tidy_ci, file = here::here("output","icvs_output.RData"))

# save(tidy_ci, file = here::here("output", "icvs_output.RData"))

