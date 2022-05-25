
# In hypotheses: decimals indicate testable, model-specific predictions to test the higher-order hypothesis. 
#Letters indicate tests of the hypotheses' mechanisms, but are tested distinctly from the main hypothesis. 
#'m' or 'min' indicate a shortened phrasing of the hypothesis, 
#'used for presenting the hypothesis in tables or other short forms.



#Hypothesis 1 ____________________________________
h1 <- "Economic inequality increases security consumption"

h1p <- "The presence of relatively disadvantaged peers increases security consumption"

h1_min <- "Positive main effect of inequality"

h1.1 <- "(mixed anova) In the model `ineq_anova_mixed`, security spending will be larger in the when income inequality is present (1) vs. absent (0)"

h1.t <- "(t-test) In the model `ineq_t_test`, security spending will be larger in the when income inequality is present (1) vs. absent (0)"

h1a <- "Economic inequality's effect on security consumption is mediated by the expectation of peers' envy"

h1am <- "The indirect effect (ACME) will be significantly different from 0"

h1a.1  <- "In the model `m_mixed_med1`, the coefficient for income inequality will have a significant positive effect on participants' belief that their partner is envious."

h1a.2 <- "In the model `m_mixed_med2`, participants' belief that their partner is envious will have a significantly positive effect on security spending."

h1a.3 <- "In the model `m_mixed_med_full`, the indirect effect/ACME (Average Causal Mediation Effect [total effect - direct effect]) will be significantly different from 0"

hyp1 <- c(h1,h1_min,h1.1,h1a,h1am,h1a.1,h1a.2,h1a.3)




#Hypothesis 2 ____________________________________

h2 <- "The effect of inequality on security consumption will be lower when incomes appear to be earned"

h2_min <- "Negative inequality*merit interaction"

h2.1 <- "(linear/anova): In the 2b*2b model `anova_merit`, 
the inequality-merit interaction will have a significantly negative effect on security spending."

h2.2 <- "(Planned pairwise comparisons), in the model `merit_pc`, the hi-inequality/random income condition will have significantly higher rates of security consumption than all other conditions 
(i.e., **.a)** hi-inequality/merit income, **.b)** no-inequality/random income, & **.c)** no-inequality/merit income)."

h2.2m <- "Pairwise: Hi-inequality/random > all other conditions."

h2a <- "The deservingness-mediated effect of inequality on security consumption will be smaller when incomes are earned"

h2am <- "ACME will be significantly larger when roles are random vs. based on merit"

h2a.1 <- "(Moderated mediation) The indirect effects of inequality on security consumption, through perceived income deservingness, will be largest when positions are allocated randomly (versus when roles are allocated based on merit). In the model `merit_modmed_test`, the indirect effect/ACME (Average Causal Mediation Effect [total effect - direct effect]) will be significantly larger when roles are random (merit=0) than when allocated based on merit (merit=1)"


hyp2 <- c(h2,h2_min,h2.1,h2.2,h2.2m,h2a,h2am,h2a.1)


# **H1e.3:** (Mediation step 1) In the model `m_vis_med1`, coefficients for **a)** income inequality and **b)** the inequality\*merit interaction each will have a significant positive effect on participants' perceived likelihood of their partner attacking.
# 
# **H1e.4:** (Mediation step 2) In the model `m_merit_likely_security`, participants' perceived likelihood of their partner attacking will have a significantly positive effect on security spending.


#Hypothesis 3 ____________________________________

h3 <- "The effect of partner's outgroup membership on security consumption will be higher when inequality is produced by the choice of an ingroup member"

h3_min <- "Positive outgroup partner*agentic interaction"

h3.1 <- "(lm/anova) In the 2b*2b model `anova_group`, the interaction between group membership and agentic wealth distribution will be significant."

h3.2 <- "(Planned pairwise comparisons) In the model `group_pc`, the agentic wealth distribution/outgroup condition will have higher rates of security consumption than all other conditions (i.e., **.a)** agentic/ingroup, **.b)** random/outgroup, & **.c)** random/ingroup."

h3.2m <- "Pairwise: agentic/outgroup condition > all other conditions."

# "Perceived fairness will mediate the effect of agent-chosen wealth distribution on security consumption"

h3a <- "The fairness-mediated effect of group membership on security consumption will be larger when inequality appears to be the result of an ingroup member's choice agent-chosen wealth distribution"

h3am <- "Indirect effect of group membership will be significantly larger when wealth allocations are chosen by an agent"

h3a.1 <- "(Moderated mediation) The indirect effects of partner's group membership on security consumption, through perceived likelihood of partner attack, will be largest when positions are allocated randomly (versus when roles are allocated by another participant). In the model `group_modmed_test`, the indirect effect/ACME (Average Causal Mediation Effect [total effect - direct effect]) will be significantly larger when roles are random (outgroup=0) than when allocated based on a participant's choice (outgroup=1)"

hyp3 <- c(h3,h3_min,h3.1,h3.2,h3a,h3am,h3a.1,h3.2m)



# (Mediation step 1) ) In the model `m_group_med1`, coefficients for **b)** partner's group membership and **c)** the interaction between partner's group membership and agentic wealth distribution will have a significant positive effect on participants' perceived likelihood of their partner attacking.
# 
# (Mediation step 2) In the model `m_group_likely_security`,participants' perceived likelihood of their partner attacking will have a significantly positive effect on security spending.



#Hypothesis 4 ____________________________________

h4 <- "Nation-level inequality will be positively associated with consumption of security products"

h4_min <- "Positive main effect of nation gini on security consumption"



#Just for table___________________
hyp2_min1 <- "The indirect effect (ACME) will be sig. dif. from 0"

# hyp2_min[1]

h2_min1 <- "Negative ineq *merit interaction"

h2.2m1 <- "Pairwise: Hi-ineq /random > other conds."

h3.2m1 <- "Pairwise: agentic/ outgroup > other conds."



# Hypothesis lists _________________________________

hypotheses <- c(h1,h2,h3,h4)

hyp_min <- c(h1_min,h2_min,h3_min,h4_min,h2.2m,h3.2m)

hypoth2 <- c(h1a,h2a,h3a)

hyp2_min <- c(h1am,h2am,h3am)

cgwtools::resave(hyp1,hyp2,hyp3,hypotheses,hyp_min, hypoth2,hyp2_min, file = "C:/Users/dalla/Google Drive/project_files/inequality_security/output/manuscript_objects.RData")
