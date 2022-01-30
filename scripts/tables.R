source(file=here::here("scripts","hypotheses.r"))
load(file = here::here("output", "pretest_output.RData"))


#First design table ____________________________________

design_columns <- c("Question",	"Hypothesis",	"Sampling plan", "Analysis Plan",	"Interpretation given to different outcomes")


Question <- c("Does inequality increase security consumption?",
              "Does envy anticipation mediate the effect of inequality on security consumption?",
              "Does deservingness reduce inequality's effect on security consumption?", 
              "Under inequality, does group-based favoritism increase security consumption?", 
              "Is country-level inequality associated with individual-level security consumption?")
Hypothesis <- c(h1_min,hyp2_min[1],h2_min, h3_min, h4_min)
Sampling <- c(paste("Studies 1a, b, & c; n=",format(round(power_ttest_security$n*2,0),big.mark = ",", scientific = FALSE)," 
               crowdsourced. Target: cohen's d = .2. Exclusion: missing data, failed attention/comprehension checks"),
              "Study 1a, same sample as above",
              paste("Study 1d; n = ",format(cell_n_01*4,big.mark = ",", scientific = FALSE), "crowdsourced. Power target: parial eta^2 = .01. Excluded if completes < 3 sliders; missing data, failed atn/comp checks"), 
              paste("Study 1e; n = ",format(cell_n_01*4,big.mark = ",", scientific = FALSE), "crowdsourced, following from power analysis in Study 1e. Exclusion: missing data, failed atn/comp checks"), 
              "Study 2; k = 28 countries, and n = 52,908 participants; Exclusion: missing data")
Analysis<- c("Between-subjects t-tests, r package `rstatix`",
             "Mediation analysis; r package `mediate`",
             "2bx2b ANOVA and Pairwise comparisons, r package `car`",
             "2bx2b ANOVA and Pairwise comparisons, r package `car`", 
             "Multi-level ordinal regression model, r package `ordinal`")
Interpretation <- c("Task complexity (low comprehension of partner's income)",
                    "Low comprehension",
                    "1) Task complexity, 2)increased desire to keep funds (and thus purchase security)",
                    "Task complexity (comprehending group membership, and allocation process)", 
                    "Country is too broad (stronger effects at province, city, neighborhood)")

design <- data.frame(Question, Hypothesis, Sampling, Analysis, Interpretation)

cgwtools::resave(design_columns,design, file = here::here("output", "manuscript_objects.RData"))


#Secondary design table ____________________________________

Question2 <- c("Does envy anticipation mediate the effect of inequality on security consumption?", 
               "Does deservingness mediate the inequality*merit effect on security consumption?", 
               "Does fairness mediate the outgroup*agentic effect on security consumption?")
Hypothesis2 <- c(hyp2_min)
Sampling2 <- c(paste(format(round(power_ttest_security$n*2,0),big.mark = ",", scientific = FALSE),"
               n=680 (Studies 1a, b, & c), crowdsourced. Power: partial eta2 = .01."),
               "Study 1d; n = 1360, crowdsourced", "Study 1e; n = 1360, crowdsourced, following from previous power analysis")
Analysis2 <- c("mediation::mediate","moderated mediaiotn using mediation::test.modmed", 
               "moderated mediaiotn using mediation::test.modmed")
Interpretation2 <- c("Task complexity (low comprehension of partner's income)",
                     "low perceived deservingness in hi inequality, meritocracy condition",
                     "Task complexity (comprehending group membership, and allocation process)")

design2 <- data.frame(Question2, Hypothesis2, Sampling2, Analysis2, Interpretation2)


cgwtools::resave(design2, file = here::here("output", "manuscript_objects.RData"))
