# Study 1a - 2b*inequality/2w*stakes

library(readr) #read_csv
library(tidyr) #pivot_wider
library(tidyverse) #select
library(afex) # anovas
library(cowplot) # nice ggtheme
library(gtools) 

set.seed(1234)

source(file=here::here("scripts","manuscript_funs.R"))
source(file="scripts/tidy_mixed_med.r")

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

pilot_data <- read_csv("C:/Users/dalla/Google Drive/offline_data_files/security_game/ineq_security_1a_2022_05_31.csv")

page_times <-
  read_csv(
    "C:/Users/dalla/Google Drive/offline_data_files/security_game/PageTimes_ineq_security_1a_pilot_2022-05-31.csv"
  )


pilot_data <- pilot_data %>%
  select(-contains(c("participant.label","completionlink","prolific")))

save(pilot_data, file = here::here("data", "ineq_security_1a_2022_05_31_anon.csv"))


names(page_times) <-
  gsub(x = names(page_times),
       pattern = "\\._",
       replacement = "_")

names(page_times) <-
  gsub(x = names(page_times),
       pattern = "\\.",
       replacement = "_")

names(pilot_data) <-
  gsub(
    x = names(pilot_data),
    pattern = "._",
    replacement = "_",
    fixed = TRUE
  )
names(pilot_data) <-
  gsub(
    x = names(pilot_data),
    pattern = ".",
    replacement = "_",
    fixed = TRUE
  )


pilot_data <- pilot_data %>%
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




names(pilot_data) <-
  gsub(
    x = names(pilot_data),
    pattern = "security_game_mixed",
    replacement = "round",
    fixed = TRUE
  )

names(pilot_data) <-
  gsub(
    x = names(pilot_data),
    pattern = "consent_1_player_",
    replacement = "",
    fixed = TRUE
  )

names(pilot_data) <-
  gsub(
    x = names(pilot_data),
    pattern = "attention_check_1_",
    replacement = "",
    fixed = TRUE
  )

names(pilot_data) <-
  gsub(
    x = names(pilot_data),
    pattern = "player_",
    replacement = "",
    fixed = TRUE
  )

names(pilot_data) <-
  gsub(
    x = names(pilot_data),
    pattern = "survey_1_",
    replacement = "",
    fixed = TRUE
  )

names(pilot_data) <-
  gsub(
    x = names(pilot_data),
    pattern = "consent_1_",
    replacement = "",
    fixed = TRUE
  )

names(pilot_data) <-
  gsub(
    x = names(pilot_data),
    pattern = "round_1_inequality",
    replacement = "inequality",
    fixed = TRUE
  )

pilot_data$inequality <- as.factor(pilot_data$inequality)

# out_wide <-
#   pivot_wider(
#     page_times,
#     id_cols = participant_code,
#     names_from = page_name,
#     names_prefix = "time_",
#     values_from = seconds_on_page
#   )

s1a_n_collected <- nrow(pilot_data)

pilot_data <- pilot_data %>%
  # left_join(out_wide, by = c("participant_code")) %>%
  select(-contains(c("prolific", "completion", "mturk"))) %>%
  filter(!is.na(atn_boost))
pilot_data <- pilot_data %>%
  janitor::clean_names()      # clean names to be all lowercase, replacing spaces with "_"

s1a_n_consented <- nrow(pilot_data)


# number of people not consenting
non_consent <- s1a_n_collected - pilot_data
# pilot_data1 <-  pilot_data

which(pilot_data$study_end_1_comments == 
        "I meant to type \"yes\" in the security check but misclicked and advanced the screen instead. Apologies!")

pilot_data$atn_other[184] <- "yes"


pilot_data <-  pilot_data %>%
  filter(!is.na(age) & !is.na(gender)) %>%
  filter(
    comp_check == "Deciding whether to purchase a security product") %>%
  filter(atn_other == "yes" | atn_other == "Yes"| atn_other == "&quot;yes&quot;" | atn_other == "\"yes\"" | atn_other == "YES") %>%
  filter(atn_boost > 3)

#number of participants after comprehension and attention
s1a_n_attended <- nrow(pilot_data)

s1a_female <- sum(pilot_data$gender == "Female")/nrow(pilot_data)


consumed_vars <- colnames(pilot_data[grepl('security_consumed', colnames(pilot_data))])

page_vars <- colnames(pilot_data[grepl('page_in_round', colnames(pilot_data))])

pilot_data$consumed_total <-
  rowMeans(pilot_data[, consumed_vars], na.rm = TRUE) * NA ^ (rowMeans(!is.na(pilot_data[, consumed_vars])) == 0)

# adjust for unused 60_02 condition in this study, move conditions 'back' a round where 60_02 is not the third round

pilot_data <- pilot_data %>%
  mutate(
    new_round_1_page_in_round = if_else(round_1_page_in_round == "Sec_60_02", round_2_page_in_round, round_1_page_in_round),
    round_1_security_consumed = if_else(round_1_page_in_round == "Sec_60_02",round_2_security_consumed,round_1_security_consumed),
    round_1_pre_partner_attempt = if_else(round_1_page_in_round == "Sec_60_02",round_2_pre_partner_attempt,round_1_pre_partner_attempt),
    round_1_p_partner_jealous = if_else(round_1_page_in_round == "Sec_60_02",round_2_p_partner_jealous,round_1_p_partner_jealous),
    round_1_p_partner_bitter = if_else(round_1_page_in_round == "Sec_60_02",round_2_p_partner_bitter,round_1_p_partner_bitter),
    round_1_p_partner_envy = if_else(round_1_page_in_round == "Sec_60_02",round_2_p_partner_envy,round_1_p_partner_envy),
  )

round_1_envy_items <- c("round_1_p_partner_jealous","round_1_p_partner_bitter","round_1_p_partner_envy")

pilot_data <- pilot_data %>%
  mutate(
    new_round_2_page_in_round = if_else(round_1_page_in_round == "Sec_60_02" | 
                                          round_2_page_in_round == "Sec_60_02", round_3_page_in_round, round_2_page_in_round),
    round_2_security_consumed = if_else(round_1_page_in_round == "Sec_60_02" | 
                                          round_2_page_in_round == "Sec_60_02", round_3_security_consumed, round_2_security_consumed),
    round_2_pre_partner_attempt = if_else(round_1_page_in_round == "Sec_60_02" | 
                                          round_2_page_in_round == "Sec_60_02", round_3_pre_partner_attempt, round_2_pre_partner_attempt),
    round_2_p_partner_jealous = if_else(round_1_page_in_round == "Sec_60_02" | 
                                          round_2_page_in_round == "Sec_60_02", round_3_p_partner_jealous, round_2_p_partner_jealous),
    round_2_p_partner_bitter = if_else(round_1_page_in_round == "Sec_60_02" | 
                                          round_2_page_in_round == "Sec_60_02", round_3_p_partner_bitter, round_2_p_partner_bitter),
    round_2_p_partner_envy = if_else(round_1_page_in_round == "Sec_60_02" | 
                                          round_2_page_in_round == "Sec_60_02", round_3_p_partner_envy, round_2_p_partner_envy),
  )

round_2_envy_items <- c("round_2_p_partner_jealous","round_2_p_partner_bitter","round_2_p_partner_envy")




# pilot_data <- pilot_data %>%
#   mutate(
#     new_round_2_page_in_round = if_else(round_2_page_in_round == "Sec_60_02", round_3_page_in_round, round_2_page_in_round)
#   )

pilot_data$round_1_page_in_round <-pilot_data$new_round_1_page_in_round
pilot_data$round_2_page_in_round <-pilot_data$new_round_2_page_in_round

pilot_data <- pilot_data %>%
  # left_join(out_wide, by = c("participant_code")) %>%
  select(-contains(c("round_3", "round_2_inequality", "mturk")))


round_1_envy_vars <- colnames(pilot_data[grepl('round_1_p_partner_', colnames(pilot_data))])

round_2_envy_vars <- colnames(pilot_data[grepl('round_2_p_partner_', colnames(pilot_data))])

# ltm::cronbach.alpha(pilot_data
                    # [,round_1_envy_items],CI=TRUE)
# 
# pilot_data %>%
#   tm::cronbach.alpha(.
#                      [,round_2_envy_items],CI=TRUE)

pilot_data$round_1_likely_envy <- rowMeans(pilot_data[, round_1_envy_vars], na.rm = TRUE) * NA ^ (rowMeans(!is.na(pilot_data[, round_1_envy_vars])) == 0)

pilot_data$round_2_likely_envy <- rowMeans(pilot_data[, round_2_envy_vars], na.rm = TRUE) * NA ^ (rowMeans(!is.na(pilot_data[, round_2_envy_vars])) == 0)

pilot_data$av_likely_envy <- rowMeans(pilot_data[, c("round_2_likely_envy","round_1_likely_envy")], na.rm = TRUE) * NA ^ (rowMeans(!is.na(pilot_data[, c("round_2_likely_envy","round_1_likely_envy")])) == 0)

pilot_data$av_p_likelihood <- rowMeans(pilot_data[, c("round_2_pre_partner_attempt","round_1_pre_partner_attempt")], na.rm = TRUE) * NA ^ (rowMeans(!is.na(pilot_data[, c("round_2_pre_partner_attempt","round_1_pre_partner_attempt")])) == 0)


s1adesc_columns <- c("Variable",	"Mean",	"SD")

Variable <- c("Age",
              "Perceived envy",
              "Perceived attack likelihood",
              "Security consumed")


Mean <-
  c(round(mean(pilot_data$age),1),
    round(mean(pilot_data$av_likely_envy),1),
    round(mean(pilot_data$av_p_likelihood),1),
    round(mean(pilot_data$consumed_total),1)
  )

SD <-
  c(round(sd(pilot_data$age),1),
    round(sd(pilot_data$av_likely_envy),1),
    round(sd(pilot_data$av_p_likelihood),1),
    round(sd(pilot_data$consumed_total),1)
  )



envy_alpha_1 <- ltm::cronbach.alpha(pilot_data
                                    [,round_1_envy_items],CI=TRUE)

envy_alpha_2 <-ltm::cronbach.alpha(pilot_data
                                   [,c(round_2_envy_items)],CI=TRUE)

envy_alpha_meanci <- NA

envy_alpha_meanci[1] <- mean(envy_alpha_1$ci[1],envy_alpha_2$ci[1])

envy_alpha_meanci[2] <- mean(envy_alpha_1$ci[2],envy_alpha_2$ci[2])

bt_ci95 <- paste("[", round(envy_alpha_meanci[1],2), ", ", 
                 round(envy_alpha_meanci[2],2),"]",  sep="")

alpha95 <- c("",
             paste("[", round(envy_alpha_meanci[1],2), ", ", 
                   round(envy_alpha_meanci[2],2),"]",  sep="")
             ,"",
             "")

# paste(round(overall_envy_alpha$alpha,2),


s1a_desc <- data.frame(Variable, Mean, SD,alpha95)

consumed_vars <- c("round_1_security_consumed", "round_2_security_consumed")

data_long <- pilot_data %>% pivot_longer(consumed_vars, names_to = "round_num", values_to = "security_consumed")

data_long$round_num <- data_long$round_num %>%
  # sub("round_", "", .) %>%
  sub("_security_consumed", "", .) %>%
  sub("round_", "", .) %>%
  as.numeric()


# create variable to log page displayed page for each observation
data_long <- data_long %>%
  dplyr::mutate(page = ifelse(
    round_num == 1,
    round_1_page_in_round,
    ifelse(
      round_num == 2,
      round_2_page_in_round,NA
      )
    )
    )
  

data_long$page <- data_long$page %>%
  sub("Sec_", "", .)


data_long <- data_long %>%
  dplyr::mutate(stake_cond = ifelse(
    page == "50_02",
    "lo",
    ifelse(
      page == "50_02_histak",
      "hi", NA)
    )
  )


data_long$stake_cond <- as.factor(data_long$stake_cond)

envy_vars <- c("p_partner_jealous","p_partner_bitter","p_partner_envy")

pre_partner_attempt


data_long <- data_long %>%
  dplyr::mutate(p_partner_attempt = ifelse(
    round_num == 1,
    round_1_pre_partner_attempt,
    ifelse(
      round_num == 2,
      round_2_pre_partner_attempt,NA)
  ))


data_long <- data_long %>%
  dplyr::mutate(p_partner_jealous = ifelse(
    round_num == 1,
    round_1_p_partner_jealous,
    ifelse(
      round_num == 2,
      round_2_p_partner_jealous,NA)
  ))

data_long <- data_long %>%
  dplyr::mutate(p_partner_bitter = ifelse(
    round_num == 1,
    round_1_p_partner_bitter,
    ifelse(
      round_num == 2,
      round_2_p_partner_bitter,NA)
  ))

data_long <- data_long %>%
  dplyr::mutate(p_partner_envy = ifelse(
    round_num == 1,
    round_1_p_partner_envy,
    ifelse(
      round_num == 2,
      round_2_p_partner_envy,NA)
  ))

data_long %>% 
  # group_by(stake_cond) %>%
  filter(stake_cond=="hi") %>%
  select(envy_vars) %>%
  psych::alpha()

data_long %>% 
  # group_by(stake_cond) %>%
  filter(stake_cond=="lo") %>%
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
summary(data_long$stake_cond)


# data_long %>%
#   group_by(stake_cond) %>%
#   psych::alpha()


security_summary_data <- data_long %>%
  group_by(inequality, stake_cond) %>%
  dplyr::summarise(
    mean = round(mean(security_consumed),1),
    min = round(mean(security_consumed),1) - qnorm(0.975) * round(sd(security_consumed),1) /
      sqrt(n()),
    max = round(mean(security_consumed),1) + qnorm(0.975) * round(sd(security_consumed),1) /
      sqrt(n())
  )

# raincloud_theme = theme(
#   text = element_text(size = 10),
#   axis.title.x = element_text(size = 16),
#   axis.title.y = element_text(size = 16),
#   axis.text = element_text(size = 14),
#   axis.text.x = element_text(
#     # angle = 45, 
#     vjust = 0.5),
#   legend.title = element_text(size = 16),
#   legend.text = element_text(size = 16),
#   legend.position = "right",
#   plot.title = element_text(
#     lineheight = .8,
#     face = "bold",
#     size = 16
#   ),
#   panel.border = element_blank(),
#   panel.grid.minor = element_blank(),
#   panel.grid.major = element_blank(),
#   axis.line.x = element_line(
#     colour = 'black',
#     size = 0.5,
#     linetype = 'solid'
#   ),
#   axis.line.y = element_line(
#     colour = 'black',
#     size = 0.5,
#     linetype = 'solid'
#   )
# )

security_1a_jittolin <- security_summary_data %>%
  ggplot(aes(
    y = mean,
    x = inequality
    ,group = stake_cond
    # ,fill = as.factor(cost)
    # ,shape = as.factor(cost)
    # ,color = as.factor(cost)
  )) + ggbeeswarm::geom_quasirandom(data = data_long,
                                    aes(y = security_consumed, shape = stake_cond, color = stake_cond),
  bandwidth = .2,
  show.legend = T,
  size = 3,
  alpha = .02,
  width = .15,
  dodge.width = .7) + scale_colour_manual(
  values = cbPalette,
  aesthetics = c(
    "colour"
  ))+labs(colour="Stake size",shape="Stake size") +
  geom_line(size = .7,position = position_dodge(width = .7),show.legend = FALSE) +
  geom_pointrange(
    aes(inequality, mean, ymin = min
        # ,color = cost
        , ymax = max),
    fatten = 1.5,
    # alpha = .7,
    size = 1,
    shape = 20,
    # 95,
    position = position_dodge(width = .7)
    ,show.legend = FALSE
  )+
  ylab("Units of security consumed") +    
  geom_label( color = "black",
    aes(label = paste(round(mean,2)), group =stake_cond,vjust=-1), fill="white", position = position_dodge(width = .7)
    # , alpha = 1
  ) + guides(
    color = 
      guide_legend(
        # title = "ay",  #duplicates and mucks color/shape
        override.aes = list(size=5, alpha = 1))) + 
  theme_half_open() + theme(axis.title = element_text(face="bold")) +
  # scale_y_continuous(
  #   # don't expand y scale at the lower end
  #   expand = expansion(mult = c(0, 0.05))) + 
  scale_x_discrete(labels=c("0" = "No", "1" = "Yes")) +
  xlab("Inequality")
    
security_1a_jittolin

ggsave(here::here("figures", "s1a_security_jittolin.png"),width=5, height = 3)


envy_summary_data <- data_long %>%
  group_by(inequality, stake_cond) %>%
  dplyr::summarise(
    mean = round(mean(likely_envy),1),
    min = round(mean(likely_envy),1) - qnorm(0.975) * round(sd(likely_envy),1) /
      sqrt(n()),
    max = round(mean(likely_envy),1) + qnorm(0.975) * round(sd(likely_envy),1) /
      sqrt(n())
  )



likely_envy_1a_jittolin <- envy_summary_data %>%
  ggplot(aes(
    y = mean,
    x = inequality
    ,group = stake_cond
    # ,fill = as.factor(cost)
    # ,shape = as.factor(cost)
    # ,color = as.factor(cost)
  )) + ggbeeswarm::geom_quasirandom(data = data_long,
                                    aes(y = likely_envy, shape = stake_cond, color = stake_cond),
                                    bandwidth = .2,
                                    show.legend = T,
                                    size = 3,
                                    alpha = .02,
                                    width = .16,
                                    dodge.width = .7) + scale_colour_manual(
                                      values = cbPalette,
                                      aesthetics = c("colour"
                                      ))+labs(colour="Stake size",shape="Stake size") +
  geom_line(size = .7,position = position_dodge(width = .7),show.legend = FALSE) +
  geom_pointrange(
    aes(inequality, mean, ymin = min
        # ,color = cost
        , ymax = max),
    fatten = 1.5,
    # alpha = .7,
    size = 1,
    shape = 20,
    # 95,
    position = position_dodge(width = .7)
    ,show.legend = FALSE
  )+
  ylab("Perceived partner envy") +  geom_label(
    color = "black",
    aes(label = paste(round(mean,2)), group =stake_cond,vjust=-1), fill="white", position = position_dodge(width = .7)
    # , alpha = 1
  ) + guides(
    color = 
      guide_legend(
        # title = "ay",  #duplicates and mucks color/shape
        override.aes = list(size=5, alpha = 1))) +
  theme_half_open() + theme(axis.title = element_text(face="bold")) +
  # scale_y_continuous(
  #   # don't expand y scale at the lower end
  #   expand = expansion(mult = c(0, 0.05))) + 
  scale_x_discrete(labels=c("0" = "No", "1" = "Yes")) +
  xlab("Inequality")

likely_envy_1a_jittolin


ggsave(here::here("figures", "s1a_envy_jittolin.png"),
       width=5, height = 3)


likelihood_summary_data <- data_long %>%
  group_by(inequality, stake_cond) %>%
  dplyr::summarise(
    mean = round(mean(p_partner_attempt),1),
    min = round(mean(p_partner_attempt),1) - qnorm(0.975) * round(sd(p_partner_attempt),1) /
      sqrt(n()),
    max = round(mean(p_partner_attempt),1) + qnorm(0.975) * round(sd(p_partner_attempt),1) /
      sqrt(n())
  )


likelihood_1a_jittolin <- likelihood_summary_data %>%
  ggplot(aes(
    y = mean,
    x = inequality
    ,group = stake_cond
    # ,fill = as.factor(cost)
    # ,shape = as.factor(cost)
    # ,color = as.factor(cost)
  )) + ggbeeswarm::geom_quasirandom(data = data_long,
                                    aes(y = p_partner_attempt, shape = stake_cond, color = stake_cond),
                                    bandwidth = .2,
                                    show.legend = T,
                                    size = 3,
                                    alpha = .01,
                                    width = .16,
                                    dodge.width = .7) + scale_colour_manual(
                                      values = cbPalette,
                                      aesthetics = c("colour"
                                      ))+labs(colour="Stake size",shape="Stake size") + 
  geom_line(size=.7,position = position_dodge(width = .7),show.legend = FALSE) +
  geom_pointrange(
    aes(inequality, mean, ymin = min
        # ,color = cost
        , ymax = max),
    fatten = 1.5,
    # alpha = .7,
    size = 1,
    shape = 20,
    # 95,
    position = position_dodge(width = .7)
    ,show.legend = FALSE
  )+
  ylab("Perceived attack likelihood") +  geom_label(
    color = "black",
    aes(label = paste(round(mean,2)), group =stake_cond,vjust=-1), fill="white", position = position_dodge(width = .7)
    # , alpha = .7
  ) + guides(
    color = 
      guide_legend(
        # title = "ay",  #duplicates and mucks color/shape
        override.aes = list(size=5, alpha = 1))) +
  theme_half_open() + theme(axis.title = element_text(face="bold")) +
  # scale_y_continuous(
  #   # don't expand y scale at the lower end
  #   expand = expansion(mult = c(0, 0.05))) + 
  scale_x_discrete(labels=c("0" = "No", "1" = "Yes")) +
  xlab("Inequality")

likelihood_1a_jittolin

ggsave(here::here("figures", "s1a_likelihood_jittolin.png"),
       width=5, height = 3)



afex_options(es_aov         = 'pes',
             correction_aov = 'GG',
             emmeans_model  = 'univariate')

ineq_anova_mixed <- aov_ez(id='participant_code',
                           dv='security_consumed',
                           data=data_long,
                           within = c('stake_cond'),
                           between = c('inequality'))  

ineq_anova_mixed_cohensf <- effectsize::cohens_f(ineq_anova_mixed, alternative="two.sided",verbose = FALSE,generalized = FALSE)

ineq_anova_mixed <- ineq_anova_mixed$anova_table
ineq_anova_mixed$cohens_f <- round(ineq_anova_mixed_cohensf$Cohens_f_partial,3)
ineq_anova_mixed$ci95_lo <- round(ineq_anova_mixed_cohensf$CI_low,3)
ineq_anova_mixed$ci95_hi <- round(ineq_anova_mixed_cohensf$CI_high,3)
ineq_anova_mixed <- rename(ineq_anova_mixed, DenDF = "den Df"
                           , NumDF = "num Df")

ineq_anova_mixed_envy <- aov_ez(id='participant_code',
                                dv='likely_envy',
                                data=data_long,
                                within = c('stake_cond'),
                                between = c('inequality'))

ineq_anova_mixed_envy_cohensf <- effectsize::cohens_f(ineq_anova_mixed_envy, alternative="two.sided",verbose = FALSE,generalized = FALSE)

ineq_anova_mixed_envy <- ineq_anova_mixed_envy$anova_table
ineq_anova_mixed_envy$cohens_f <- round(ineq_anova_mixed_envy_cohensf$Cohens_f_partial,3)
ineq_anova_mixed_envy$ci95_lo <- round(ineq_anova_mixed_envy_cohensf$CI_low,3)
ineq_anova_mixed_envy$ci95_hi <- round(ineq_anova_mixed_envy_cohensf$CI_high,3)
ineq_anova_mixed_envy <- rename(ineq_anova_mixed_envy, DenDF = "den Df"
                           , NumDF = "num Df")
ineq_anova_mixed_envy


ineq_anova_mixed_likelihood <- aov_ez(id='participant_code',
                                      dv='p_partner_attempt',
                                      data=data_long,
                                      within = c('stake_cond'),
                                      between = c('inequality'))  

ineq_anova_mixed_likelihood_cohensf <- effectsize::cohens_f(ineq_anova_mixed_likelihood, alternative="two.sided",verbose = FALSE,generalized = FALSE)


ineq_anova_mixed_likelihood <- ineq_anova_mixed_likelihood$anova_table
ineq_anova_mixed_likelihood$cohens_f <- round(ineq_anova_mixed_likelihood_cohensf$Cohens_f_partial,3)
ineq_anova_mixed_likelihood$ci95_lo <- round(ineq_anova_mixed_likelihood_cohensf$CI_low,3)
ineq_anova_mixed_likelihood$ci95_hi <- round(ineq_anova_mixed_likelihood_cohensf$CI_high,3)
ineq_anova_mixed_likelihood <- rename(ineq_anova_mixed_likelihood, DenDF = "den Df"
                           , NumDF = "num Df")
ineq_anova_mixed_likelihood



# bxp <- ggpubr::ggboxplot(
#   data_long, x = "inequality", y = "security_consumed",
#   color = "stake_cond", palette = "jco"
# )
# bxp
# 
# bxp_env <- ggpubr::ggboxplot(
#   data_long, x = "inequality", y = "likely_envy",
#   color = "stake_cond", palette = "jco"
# )
# bxp_env




data_long$hi_stake <- ifelse(data_long$stake_cond == "hi",
                             1,
                             0)



m_mixed_binary <- lme4::glmer(data = data_long, security_bin ~ as.factor(inequality) +
                                as.factor(hi_stake) + (1| participant_code),  
                              family = binomial, control = glmerControl(optimizer = "bobyqa"),
                              nAGQ = 10)




# H2) mixed mediation conducted using r packages lme4 and mediate

# m_mixed_med_new1 <- lme4::lmer(security_consumed ~ inequality + (1| participant_code), data = data_long)
# 
# m_mixed_med_new2 <- lme4::lmer(likely_envy ~ inequality  + 
#                              (1| participant_code), data = data_long)
# 
# m_mixed_med_new_total <- lme4::lmer(security_consumed ~ inequality + likely_envy + (1| participant_code), data = data_long)
# 
# m_mixed_med_new_full <- mediation::mediate(m_mixed_med_new1, m_mixed_med_new2,sims = 5000,
#                                        treat="inequality",mediator="likely_envy")

# m_mixed_med1 <- lme4::lmer(likely_envy ~ inequality + (1| participant_code), data = data_long)
# 
# m_mixed_med2 <- lme4::lmer(security_consumed ~ inequality + likely_envy + 
#                              (1| participant_code), data = data_long)
# 
# m_mixed_total <- lme4::lmer(security_consumed ~ inequality + (1| participant_code), data = data_long)
# 
# m_mixed_med_full <- mediation::mediate(m_mixed_med1, m_mixed_med2,sims = 5000,
#                                        treat="inequality",mediator="likely_envy")


# summary(m_mixed_med_full)

# 
# m_mixed_med_large1 <- lme4::lmer(likely_envy ~ inequality + (1| participant_code) +
#                              hi_stake, data = data_long)
# 
# m_mixed_med_large2 <- lmer(security_consumed ~ inequality + hi_stake + likely_envy +
#                        (1| participant_code), data = data_long)
# 
# m_mixed_large_total <- lmer(security_consumed ~ inequality +
#                         hi_stake + (1| participant_code) , data = data_long)
# 
# m_mixed_med_large_full <- mediation::mediate(m_mixed_med1, m_mixed_med2,sims = 5000,
#                                      treat="inequality",mediator="likely_envy")
# 
# 
# m_mixed_med_large_new1 <- lme4::lmer(security_consumed ~ inequality +
#                                        hi_stake + (1| participant_code), data = data_long)
# 
# m_mixed_med_large_new2 <- lme4::lmer(likely_envy ~ inequality + hi_stake  + 
#                                  (1| participant_code), data = data_long)
# 
# m_mixed_med_large_new_total <- lme4::lmer(security_consumed ~ inequality + hi_stake + likely_envy + 
#                                             (1| participant_code), data = data_long)
# 
# m_mixed_med_large_new_full <- mediation::mediate(m_mixed_med_large_new1, m_mixed_med_large_new2,sims = 5000,
#                                            treat="inequality",mediator="likely_envy")
# 


m_mixed_med_large_newnew1 <- lme4::lmer(security_consumed ~ inequality +
                                       hi_stake + (1| participant_code), data = data_long)
# predicting mediator with treatment and covariates
m_mixed_med_large_newnew2 <- lme4::lmer(likely_envy_cent ~ inequality + hi_stake  + 
                                       (1| participant_code), data = data_long)
# predicting outcome with treatment, mediator, and covariates
m_mixed_med_large_newnew_total <- lme4::lmer(security_consumed ~ inequality + hi_stake + likely_envy_cent + 
                                            (1| participant_code), data = data_long)

m_mixed_med_large_newnew_full <- mediation::mediate(m_mixed_med_large_newnew2, m_mixed_med_large_newnew_total,sims = 5000,
                                                 treat="inequality",mediator="likely_envy_cent")



s1a_med_1 <- summary(m_mixed_med_large_newnew1)
# Inequality 
s1a_med_2 <- summary(m_mixed_med_large_newnew2)
s1a_med_total <- summary(m_mixed_med_large_newnew_total)
s1a_med_full <- summary(m_mixed_med_large_newnew_full)

broom::tidy(s1a_med_full, conf.int = TRUE, conf.level = 0.95)

s1a_med_1_tidy <- tidy_lmer(s1a_med_1)
  s1a_med_1_tidy$df <- NA
  s1a_med_1_tidy$df[c(1,2)] <- format(s1a_med_1$ngrps - nrow(s1a_med_1_tidy) - 1,big.mark = ",", scientific = FALSE)
  s1a_med_1_tidy[3,"df"] <- format(s1a_med_1$devcomp$dims[["n"]] - 1 - 1,
                                   big.mark = ",", scientific = FALSE)
  
  s1a_med_1_tidy <- s1a_med_1_tidy %>% relocate(df, .before = estimate)
  
  # s1a_med_1$logLik
  
s1a_med_2_tidy <- tidy_lmer(s1a_med_2)
  s1a_med_2_tidy$df <- NA
  s1a_med_2_tidy$df[c(1,2)] <- format(s1a_med_2$ngrps - nrow(s1a_med_2_tidy) - 1,big.mark = ",", scientific = FALSE)
  s1a_med_2_tidy[3,"df"] <- format(s1a_med_2$devcomp$dims[["n"]] - 1 - 1,
                                   big.mark = ",", scientific = FALSE)
  s1a_med_2_tidy <- s1a_med_2_tidy %>% relocate(df, .before = estimate)

s1a_med_total_tidy <- tidy_lmer(s1a_med_total)
  s1a_med_total_tidy$df <- NA
  s1a_med_total_tidy$df[c(1,2)] <- format(s1a_med_2$ngrps - nrow(s1a_med_total_tidy) - 1,big.mark = ",", scientific = FALSE)
  s1a_med_total_tidy[c(3,4),"df"] <- format(s1a_med_total$devcomp$dims[["n"]] - 1 - 1,
                                   big.mark = ",", scientific = FALSE)
  s1a_med_total_tidy <- s1a_med_total_tidy %>% relocate(df, .before = estimate)

  
  tidied_med_int_total<- m_mixed_med_large_newint_total %>% broom.mixed::tidy()  

#format(m_ord_mod1$dims$df.residual,big.mark = ",", scientific = FALSE)

  
  s1a_med_full_tidy <-  tidy_mixed_med(s1a_med_full)
  

# H2.1), mediation step 1: In the model m_mixed_med1, the coefficient for income inequality
# will have a significant positive effect on participants’ belief that their partner is envious.
# H2.2), mediation step 2: In the model m_mixed_med2, participants’ belief that their
# partner is envious will have a significantly positive effect on security spending.
# H2.3), mediation step 3: In the model m_mixed_med_full, the indirect effect/ACME
# (Average Causal Mediation Effect [total effect - direct effect]) will be significantly different
# from 0.



m_mixed_med_large_newint1 <- lme4::lmer(security_consumed ~ inequality*hi_stake +
                                          (1| participant_code), data = data_long)
# predicting mediator with treatment and covariates
m_mixed_med_large_newint2 <- lme4::lmer(likely_envy_cent ~ inequality*hi_stake  + 
                                          (1| participant_code), data = data_long)
# predicting outcome with treatment, mediator, and covariates
m_mixed_med_large_newint_total <- lme4::lmer(security_consumed ~ inequality*hi_stake + likely_envy_cent + 
                                               (1| participant_code), data = data_long)

m_mixed_med_large_newint_full <- mediation::mediate(m_mixed_med_large_newint2, m_mixed_med_large_newint_total,sims = 5000,
                                                    treat="inequality",mediator="likely_envy_cent")


s1a_med_int_1 <- summary(m_mixed_med_large_newint1)
s1a_med_int_2 <- summary(m_mixed_med_large_newint2)
s1a_med_int_total <- summary(m_mixed_med_large_newint_total)
s1a_med_int_full <- summary(m_mixed_med_large_newint_full)


tidied_med_int_total<- m_mixed_med_large_newint_total %>% broom.mixed::tidy()

save(s1a_n_collected,s1a_n_consented,s1a_n_attended,s1a_desc,s1a_female,
     ineq_anova_mixed,ineq_anova_mixed_envy,ineq_anova_mixed_likelihood, 
     s1a_med_1,s1a_med_2,s1a_med_total,s1a_med_full,
     s1a_med_1_tidy,s1a_med_2_tidy,s1a_med_total_tidy, s1a_med_full_tidy,
     file = here::here("output", "security_s1a_output.RData"))
