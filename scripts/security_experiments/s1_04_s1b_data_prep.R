# Study 1b - 2w inequality, uncertain values

library(readr) #read_csv
library(tidyr) #pivot_wider
library(tidyverse) #select
library(afex)
library(cowplot)
library(gtools)
library(ggtext) #for markdown subtitles
# library(latex2exp) # for annotating with latex

source(file=here::here("scripts","manuscript_funs.R"))
source(file=here::here("scripts","tidy_ttest.R"))
source(file=here::here("scripts","tidy_mixed_med.R"))

set.seed(1234)

cbPalette <-
  c(
    "#999999",
    "#E69F00",
    "#56B4E9",
    "#009E73",
    "#F0E442",
    "#0072B2",
    "#D55E00",
    "#CC79A7"
  )

data <- read_csv("C:/Users/dalla/Google Drive/offline_data_files/security_game/ineq_security_within_1b_2022_7_7.csv")




data <- data %>%
  select(-contains(c("participant.label","completionlink","prolific")))

save(data, file = here::here("data", "ineq_security_within_1b_2022_07_07_anon.csv"))


names(data) <-
  gsub(
    x = names(data),
    pattern = "._",
    replacement = "_",
    fixed = TRUE
  )
names(data) <-
  gsub(
    x = names(data),
    pattern = ".",
    replacement = "_",
    fixed = TRUE
  )


data <- data %>%
  select(-starts_with(
    c(
      "intro_notask",
      "slider_task",
      "merit",
      "security_game_1",
      "session"
    )
  )) %>%
  select(-contains(
    c(
      "payoff",
      "mturk",
      "id_in",
      "player_role",
      "bot",
      "_ts_",
      "index",
      "current",
      "time_started",
      "visited",
      "submit_missing",
      ".y",
      "round_number",
      "_role",
      "id_in_group"
    )
  ))




names(data) <-
  gsub(
    x = names(data),
    pattern = "security_game_mixed",
    replacement = "round",
    fixed = TRUE
  )

names(data) <-
  gsub(
    x = names(data),
    pattern = "consent_1_player_",
    replacement = "",
    fixed = TRUE
  )

names(data) <-
  gsub(
    x = names(data),
    pattern = "attention_check_1_",
    replacement = "",
    fixed = TRUE
  )

names(data) <-
  gsub(
    x = names(data),
    pattern = "player_",
    replacement = "",
    fixed = TRUE
  )

names(data) <-
  gsub(
    x = names(data),
    pattern = "survey_1_",
    replacement = "",
    fixed = TRUE
  )

names(data) <-
  gsub(
    x = names(data),
    pattern = "consent_1_",
    replacement = "",
    fixed = TRUE
  )

names(data) <-
  gsub(
    x = names(data),
    pattern = "round_1_inequality",
    replacement = "inequality",
    fixed = TRUE
  )

names(data) <-
  gsub(
    x = names(data),
    pattern = "security_game_w_",
    replacement = "",
    fixed = TRUE
  )

data <- data %>%
  filter(!is.na(consent))


s1b_n_collected <- nrow(data)

data <- data %>%
  filter(consent==1)

non_consent <- s1b_n_collected - nrow(data)

data <- data %>%
  # left_join(out_wide, by = c("participant_code")) %>%
  select(-contains(c("prolific", "completion", "mturk"))) %>%
  filter(!is.na(atn_boost))
data <- data %>%
  janitor::clean_names()      # clean names to be all lowercase, replacing spaces with "_"

s1b_n_consented <- nrow(data)


unique(data$study_end_1_comments)

unique(data$atn_other)

# s1b-filter-attention ----

data <-  data %>%
  filter(!is.na(age) & !is.na(gender)) %>%
  filter(
    comp_check == "Deciding whether to purchase a security product") %>%
  filter(atn_other == "yes" | atn_other == "Yes"| atn_other == "&quot;yes&quot;" | atn_other == "\"yes\"" | atn_other == "YES") %>%
  filter(atn_boost > 3)

s1b_n_attended <- nrow(data)

consumed_vars <- colnames(data[grepl('security_consumed', colnames(data))])

page_vars <- colnames(data[grepl('page_in_round', colnames(data))])


consumed_vars <- colnames(data[grepl('security_consumed', colnames(data))])

page_vars <- colnames(data[grepl('page_in_round', colnames(data))])

data$consumed_total <-
  rowMeans(data[, consumed_vars], na.rm = TRUE) * NA ^ (rowMeans(!is.na(data[, consumed_vars])) == 0)

round_1_envy_vars <- colnames(data[grepl('ineq_1_p_partner_', colnames(data))])

round_2_envy_vars <- colnames(data[grepl('ineq_2_p_partner_', colnames(data))])

data$round_1_likely_envy <- rowMeans(data[, round_1_envy_vars], na.rm = TRUE) * NA ^ (rowMeans(!is.na(data[, round_1_envy_vars])) == 0)

data$round_2_likely_envy <- rowMeans(data[, round_2_envy_vars], na.rm = TRUE) * NA ^ (rowMeans(!is.na(data[, round_2_envy_vars])) == 0)

data$av_likely_envy <- rowMeans(data[, c("round_2_likely_envy","round_1_likely_envy")], na.rm = TRUE) * NA ^ (rowMeans(!is.na(data[, c("round_2_likely_envy","round_1_likely_envy")])) == 0)

data$av_p_likelihood <- rowMeans(data[, c("ineq_2_pre_partner_attempt","ineq_1_pre_partner_attempt")], na.rm = TRUE) * NA ^ (rowMeans(!is.na(data[, c("ineq_1_pre_partner_attempt","ineq_2_pre_partner_attempt")])) == 0)


s1b_female <- sum(data$gender == "Female")/nrow(data)


s1b_desc_columns <- c("Variable",	"Mean",	"SD")

Variable <- c("Age",
              "Perceived envy",
              "Perceived attack likelihood",
              "Security consumed")



Mean <-
  c(round(mean(data$age),1),
    round(mean(data$av_likely_envy),1),
    round(mean(data$av_p_likelihood),1),
    round(mean(data$consumed_total),1)
  )

SD <-
  c(round(sd(data$age),1),
    round(sd(data$av_likely_envy),1),
    round(sd(data$av_p_likelihood),1),
    round(sd(data$consumed_total),1)
  )


envy_alpha_1 <- ltm::cronbach.alpha(data
                                    [,round_1_envy_vars],CI=TRUE)

envy_alpha_2 <- ltm::cronbach.alpha(data
                                    [,c(round_2_envy_vars)],CI=TRUE)

envy_alpha_meanci <- NA
envy_alpha_meanci[1] <- mean(envy_alpha_1$ci[1],envy_alpha_2$ci[1])

envy_alpha_meanci[2] <- mean(envy_alpha_1$ci[2],envy_alpha_2$ci[2])


overall_envy_alpha <-ltm::cronbach.alpha(data
                                         [,c(round_1_envy_vars,round_2_envy_vars)],CI=TRUE)

bt_ci95 <- paste("[", round(envy_alpha_meanci[1],2), ", ", 
                 round(envy_alpha_meanci[2],2),"]",  sep="")

alpha95 <- c("",
             paste("[", round(envy_alpha_meanci[1],2), ", ", 
                   round(envy_alpha_meanci[2],2),"]",  sep="")
             ,"",
             "")


s1b_desc <- data.frame(Variable, Mean, SD
                        ,alpha95
)

# ---- s1b-make-long-data ----
data_long <- data %>% pivot_longer(consumed_vars, names_to = "round_num", values_to = "security_consumed")

data_long$security_spending <- data_long$security_consumed*2

data_long$round_num <- data_long$round_num %>%
  # sub("round_", "", .) %>%
  sub("_security_consumed", "", .) %>%
  sub("ineq_", "", .) %>%
  as.numeric()


data_long <- data_long %>%
  dplyr::mutate(page = ifelse(
    round_num == 1,
    ineq_1_page_in_round,
    ifelse(
      round_num == 2,
      ineq_2_page_in_round,NA
    )
  )
  )


data_long$page <- data_long$page %>%
  sub("Sec_20_", "", .)


data_long <- data_long %>%
  dplyr::mutate(ineq = ifelse(
    page == "ineq",
    "1",
    ifelse(
      page == "eq",
      "0", NA)
  )
  )

data_long <- data_long %>%
  dplyr::mutate(inequality = ifelse(
    page == "ineq",
    "1.yes",
    ifelse(
      page == "eq",
      "2.no", NA)
  )
  )

data_long$inequality <- as.factor(data_long$inequality)

data_long$ineq <- as.factor(data_long$ineq)


envy_vars <- c("p_partner_jealous","p_partner_bitter","p_partner_envy")




data_long <- data_long %>%
  dplyr::mutate(p_partner_attempt = ifelse(
    round_num == 1,
    ineq_1_pre_partner_attempt,
    ifelse(
      round_num == 2,
      ineq_2_pre_partner_attempt,NA)
  ))


data_long <- data_long %>%
  dplyr::mutate(p_partner_jealous = ifelse(
    round_num == 1,
    ineq_1_p_partner_jealous,
    ifelse(
      round_num == 2,
      ineq_2_p_partner_jealous,NA)
  ))

data_long <- data_long %>%
  dplyr::mutate(p_partner_bitter = ifelse(
    round_num == 1,
    ineq_1_p_partner_bitter,
    ifelse(
      round_num == 2,
      ineq_2_p_partner_bitter,NA)
  ))

data_long <- data_long %>%
  dplyr::mutate(p_partner_envy = ifelse(
    round_num == 1,
    ineq_1_p_partner_envy,
    ifelse(
      round_num == 2,
      ineq_2_p_partner_envy,NA)
  ))


data_long <- data_long %>%
  dplyr::mutate(p_partner_attempt = ifelse(
    round_num == 1,
    ineq_1_pre_partner_attempt,
    ifelse(
      round_num == 2,
      ineq_2_pre_partner_attempt,NA)
  ))

data_long <- data_long %>%
  dplyr::mutate(p_inequality = ifelse(
    round_num == 1,
    ineq_1_p_inequality,
    ifelse(
      round_num == 2,
      ineq_2_p_inequality,NA)
  ))


data_long %>% 
  # group_by(stake_cond) %>%
  filter(inequality=="1.yes") %>%
  select(envy_vars) %>%
  psych::alpha()

data_long %>% 
  # group_by(stake_cond) %>%
  filter(inequality=="2.no") %>%
  select(envy_vars) %>%
  psych::alpha()


data_long <- data_long %>%
  dplyr::mutate(likely_envy = ifelse(
    round_num == 1,
    round_1_likely_envy,
    ifelse(
      round_num == 2,
      round_2_likely_envy,NA)
  ))

data_long$likely_envy_cent <- data_long$likely_envy - mean(data_long$likely_envy)

data_long$round_num <- as.factor(data_long$round_num)


data_long$security_bin <- ifelse(data_long$security_consumed == 0, 0,
                                 ifelse(data_long$security_consumed > 0 , 1,NA))


# summarize major variables
summary(data_long$likely_envy)
summary(data_long$security_consumed)
summary(data_long$security_bin)
summary(data_long$inequality)



#t-tests ----


s1b_pineq_t_tidy <- tidy_ttest(data_long, iv = "inequality", dv = "p_inequality", paired = TRUE)

s1b_envy_t_tidy <- tidy_ttest(data_long, iv = "inequality", dv = "likely_envy", paired = TRUE)

s1b_likelihood_t_tidy <- tidy_ttest(data_long, iv = "inequality", dv = "p_partner_attempt", paired = TRUE)

s1b_sec_t_tidy <- tidy_ttest(data_long, iv = "inequality", dv = "security_consumed", paired = TRUE)

s1b_spend_t_tidy <- tidy_ttest(data_long, iv = "inequality", dv = "security_spending", paired = TRUE)



# s1b-vizualize-security ----

security_summary_data <- data_long %>%
  group_by(inequality) %>%
  dplyr::summarise(
    mean = round(mean(security_consumed),1),
    min = round(mean(security_consumed),1) - qnorm(0.975) * round(sd(security_consumed),1) /
      sqrt(n()),
    max = round(mean(security_consumed),1) + qnorm(0.975) * round(sd(security_consumed),1) /
      sqrt(n())
  )

ineq_order <- c("2.no","1.yes")

security_s1b_jittolin <- security_summary_data %>%
  ggplot(aes(
    y = mean,
    x = factor(inequality,level=ineq_order)
  )) + ggbeeswarm::geom_quasirandom(data = data_long,
                                    aes(y = security_consumed),
                                    bandwidth = .2,  # Smaller numbers (< 1) produce a tighter density "fit"
                                    size = 3,
                                    alpha = .04,
                                    width = .4,  # adjusts maximum width of where points can be found
                                    dodge.width = 0)  +
  geom_pointrange(
    aes(inequality, mean, ymin = min
        , ymax = max),
    fatten = 3,
    size = 1,
    shape = 20 ) + 
  geom_line(aes(x=inequality,y=mean,group=1)) +
  # scale_y_continuous(
  #   # don't expand y scale at the lower end
  #   expand = expansion(mult = c(0, 0.05))) + 
  scale_x_discrete(labels=c("2.no" = "No", "1.yes" = "Yes")) +
  ylab("Units of security consumed") +
  xlab("Inequality") +    
  geom_label( color = "black",
              aes(label = paste(round(mean,2)),vjust=-1), fill="white") + 
  theme_half_open() + 
  theme(axis.title = element_text(face="bold")) + 
  theme(plot.subtitle = element_markdown()) + 
  labs(subtitle = report_tidy_t_dci(s1b_sec_t_tidy))

security_s1b_jittolin

ggsave(here::here("figures", "s1b_security_jittolin.png"),
       width=4, height = 3)


# ----s1b-security-spending----

spending_summary_data <- data_long %>%
  group_by(inequality) %>%
  dplyr::summarise(
    mean = round(mean(security_spending),1),
    min = round(mean(security_spending),1) - qnorm(0.975) * round(sd(security_spending),1) /
      sqrt(n()),
    max = round(mean(security_spending),1) + qnorm(0.975) * round(sd(security_spending),1) /
      sqrt(n())
  )

spending_s1b_jittolin <- spending_summary_data %>%
  ggplot(aes(
    y = mean,
    x = factor(inequality,level=ineq_order)
  )) + ggbeeswarm::geom_quasirandom(data = data_long,
                                    aes(y = security_spending),
                                    bandwidth = .2,  # Smaller numbers (< 1) produce a tighter density "fit"
                                    size = 3,
                                    alpha = .04,
                                    width = .4,  # adjusts maximum width of where points can be found
                                    dodge.width = 0)  +
  geom_pointrange(
    aes(inequality, mean, ymin = min
        , ymax = max),
    fatten = 3,
    size = 1,
    shape = 20 ) + 
  geom_line(aes(x=inequality,y=mean,group=1)) +
  # scale_y_continuous(
  #   # don't expand y scale at the lower end
  #   expand = expansion(mult = c(0, 0.05))) + 
  scale_x_discrete(labels=c("2.no" = "No", "1.yes" = "Yes")) +
  ylab("$ spent on security") +
  xlab("Inequality") +    
  geom_label( color = "black",
              aes(label = paste(round(mean,2)),vjust=-1), fill="white") + 
  theme_half_open() + 
  theme(axis.title = element_text(face="bold")) + 
  theme(plot.subtitle = element_markdown()) + 
  labs(subtitle = report_tidy_t_dci(s1b_spend_t_tidy))

spending_s1b_jittolin

ggsave(here::here("figures", "s1b_spending_jittolin.png"),
       width=4, height = 3)


# s1b-vizualize-envy ----

envy_summary_data <- data_long %>%
  group_by(inequality) %>%
  dplyr::summarise(
    mean = round(mean(likely_envy),1),
    min = round(mean(likely_envy),1) - qnorm(0.975) * round(sd(likely_envy),1) /
      sqrt(n()),
    max = round(mean(likely_envy),1) + qnorm(0.975) * round(sd(likely_envy),1) /
      sqrt(n())
  )

envy_1b_jittolin <- envy_summary_data %>%
  ggplot(aes(
    y = mean,
    x = factor(inequality,level=ineq_order)
  )) + ggbeeswarm::geom_quasirandom(data = data_long,
                                    aes(y = likely_envy),
                                    bandwidth = .7,
                                    size = 3,
                                    alpha = .04,
                                    width = .4,
                                    dodge.width = .7)  +
  geom_pointrange(
    aes(inequality, mean, ymin = min
        , ymax = max),
    fatten = 3,
    size = 1,
    shape = 20 ) + 
  geom_line(aes(x=inequality,y=mean,group=1)) +
  # scale_y_continuous(
  #   # don't expand y scale at the lower end
  #   expand = expansion(mult = c(0, 0.05))) + 
  scale_x_discrete(labels=c("2.no" = "No", "1.yes" = "Yes")) +
  ylab("Perceived partner envy") +
  xlab("Inequality") +    
  geom_label( color = "black",
              aes(label = paste(round(mean,2)),vjust=-1), fill="white") + 
  theme_half_open() + 
  theme(axis.title = element_text(face="bold"))+ 
  theme(plot.subtitle = element_markdown()) + 
  labs(subtitle = report_tidy_t_dci(s1b_envy_t_tidy))

envy_1b_jittolin

ggsave(here::here("figures", "s1b_envy_jittolin.png"),
       width=4, height = 3)



# s1b-vizualize-likelihood ----

likelihood_summary_data <- data_long %>%
  group_by(inequality) %>%
  dplyr::summarise(
    mean = round(mean(p_partner_attempt),1),
    min = round(mean(p_partner_attempt),1) - qnorm(0.975) * round(sd(p_partner_attempt),1) /
      sqrt(n()),
    max = round(mean(p_partner_attempt),1) + qnorm(0.975) * round(sd(p_partner_attempt),1) /
      sqrt(n())
  )


likelihood_1b_jittolin <- likelihood_summary_data %>%
  ggplot(aes(
    y = mean,
    x = factor(inequality,level=ineq_order)
  )) + ggbeeswarm::geom_quasirandom(data = data_long,
                                    aes(y = p_partner_attempt),
                                    bandwidth = .7,
                                    size = 3,
                                    alpha = .02,
                                    width = .4,
                                    dodge.width = .7)  +
  geom_pointrange(
    aes(inequality, mean, ymin = min
        , ymax = max),
    fatten = 3,
    size = 1,
    shape = 20 ) + 
  geom_line(aes(x=inequality,y=mean,group=1)) +
  # scale_y_continuous(
  #   # don't expand y scale at the lower end
  #   expand = expansion(mult = c(0, 0.05))) + 
  scale_x_discrete(labels=c("2.no" = "No", "1.yes" = "Yes")) +
  ylab("Perceived attack likelihood") +
  xlab("Inequality") +    
  geom_label( color = "black",
              aes(label = paste(round(mean,2)),vjust=-1), fill="white") + 
  theme_half_open() + 
  theme(axis.title = element_text(face="bold"))+ 
  theme(plot.subtitle = element_markdown()) + 
  labs(subtitle = report_tidy_t_dci(s1b_likelihood_t_tidy))

likelihood_1b_jittolin

ggsave(here::here("figures", "s1b_likelihood_jittolin.png"),
       width=4, height = 3)


# security-paired-t-test----
# 
# s1b_sec_ttest <- stats::t.test(security_consumed ~ inequality, data=data_long, paired = TRUE) 
# 
# s1b_sec_ttest <- broom::tidy(s1b_sec_ttest)
# 
# # stat.test <- data_long  %>% 
# #   rstatix::t_test(security_consumed ~ inequality,  paired=TRUE) %>%
# #   rstatix::add_significance()
# # 
# # stat.test
# 
# s1b_sec_d <- effectsize::cohens_d(security_consumed ~ inequality, data = data_long, paired=TRUE)
# 
# s1b_sec_t_tidy <- data.frame(cbind(s1b_sec_ttest,s1b_sec_d))


s1b_sec_m_mixed_binary <- lme4::glmer(data = data_long, security_bin ~ as.factor(ineq) + (1| participant_code),  
                                       family = binomial, control = glmerControl(optimizer = "bobyqa"),
                                       nAGQ = 10)


# --- mediation-analysis----
s1b_m_mixed_med1 <- lme4::lmer(security_consumed~ ineq + (1| participant_code), 
                                data = data_long)

s1b_m_mixed_med2 <- lme4::lmer(likely_envy_cent ~ ineq +
                                  (1| participant_code), data = data_long)

s1b_m_mixed_med_total <- lme4::lmer(security_consumed~ ineq + likely_envy_cent  + 
                                       (1| participant_code), data = data_long)

s1b_m_mixed_med_full <- mediation::mediate(s1b_m_mixed_med2, s1b_m_mixed_med_total,sims = 5000,
                                            treat="ineq",mediator="likely_envy_cent")


s1b_med_1 <- summary(s1b_m_mixed_med1)
s1b_med_2 <- summary(s1b_m_mixed_med2)
s1b_med_total <- summary(s1b_m_mixed_med_total)
s1b_med_full <- summary(s1b_m_mixed_med_full)

# s1b-tidy-mediation-analyses ----
s1b_med_1_tidy <- tidy_lmer(s1b_med_1)

s1b_med_2_tidy <- tidy_lmer(s1b_med_2)

s1b_med_total_tidy <- tidy_lmer(s1b_med_total)

s1b_med_full_tidy <- tidy_mixed_med(s1b_med_full)


# --- mediation-analysis-spending----
s1b_m_mixed_med1 <- lme4::lmer(security_spending~ ineq + (1| participant_code), 
                               data = data_long)

s1b_m_mixed_med2 <- lme4::lmer(likely_envy_cent ~ ineq +
                                 (1| participant_code), data = data_long)

s1b_m_mixed_med_total <- lme4::lmer(security_spending~ ineq + likely_envy_cent  + 
                                      (1| participant_code), data = data_long)

s1b_m_mixed_med_full <- mediation::mediate(s1b_m_mixed_med2, s1b_m_mixed_med_total,sims = 5000,
                                           treat="ineq",mediator="likely_envy_cent")


s1b_med_1 <- summary(s1b_m_mixed_med1)
s1b_med_2 <- summary(s1b_m_mixed_med2)
s1b_med_total <- summary(s1b_m_mixed_med_total)
s1b_med_full <- summary(s1b_m_mixed_med_full)

# s1b-tidy-mediation-analyses-spending----
s1b_med_1_tidy <- tidy_lmer(s1b_med_1)

s1b_med_2_tidy <- tidy_lmer(s1b_med_2)

s1b_med_total_tidy <- tidy_lmer(s1b_med_total)

s1b_med_full_tidy <- tidy_mixed_med(s1b_med_full)




# s1b-save-objects ----

save(s1b_n_collected,s1b_n_consented,s1b_n_attended,  #number of participants collected and retained
     s1b_desc,s1b_female,  # descriptive statistics
     s1b_sec_t_tidy,s1b_envy_t_tidy,s1b_likelihood_t_tidy, s1b_spend_t_tidy, #t-test
     s1b_med_1,s1b_med_2,s1b_m_mixed_med_total,s1b_med_full, # meditiation
     s1b_med_1_tidy,s1b_med_2_tidy,s1b_med_total_tidy, s1b_med_full_tidy,
     file = here::here("output", "security_s1b_output.RData"))
