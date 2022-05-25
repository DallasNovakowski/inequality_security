source(file=here::here("scripts","hypotheses.r"))
load(file = here::here("output", "pretest_output.RData"))
load("C:/Users/dalla/Google Drive/offline_data_files/icvs_pwt_swiid/data/icvs_solo_datafiles.RData")  # loads indidividual dataframes for different icvs configurations

#First design table ____________________________________

design_columns <- c("Question",	"Hypothesis",	"Sampling plan", "Analysis Plan",	"Interpretation given to different outcomes")


Question <- c("Does inequality increase security consumption?",
              "Does envy anticipation mediate the effect of inequality on security consumption?",
              "Does deservingness reduce inequality's effect on security consumption?", 
              "Under inequality, does group-based favoritism increase security consumption?", 
              "Is country-level inequality associated with individual-level security consumption?")
Hypothesis <- c(paste("H1)",h1_min),paste("H2)", hyp2_min1),paste("H3) ",h2_min,". ",h2.2m1,sep=""), paste("H4) ",h3_min,". ",h3.2m1,sep=""), paste("H5)",h4_min))
Sampling <- c(paste("Study 1a; n = ",format(round(cell_n_5*2,0),big.mark = ",", scientific = FALSE)," crowdsourced. Power: cohen's f = ", round(security_exact_5$main_results$cohen_f[1],2),". Study 1b: n determined by 1a effect. Exclusion: missing data, failed atn/comp checks", sep = ""),
              "Study 1a & 1b, same samples as above",
              paste("Study 1c; n = ",format(cell_n_01*4,big.mark = ",", scientific = FALSE), "crowdsourced. Power: cohen's f = .1. Exclusion < 3 sliders; missing data, failed atn/comp checks"), 
              paste("Study 1d; n = ",format(cell_n_01*4,big.mark = ",", scientific = FALSE), "crowdsourced, following from power analysis in Study 1c. Exclusion: missing data, failed atn/comp checks"), 
              paste("Study 2; k = 32 countries, and n = ", format(nrow(iv_2005),big.mark = ",", scientific = FALSE), "participants; Exclusion: missing data"))
Analysis<- c("2b*2w ANOVA and estimated marginal means, r package `afex`",
             "Mediation analysis; r package `mediate`",
             "2b*2b ANOVA and Pairwise comparisons, r package `afex`",
             "2b*2b ANOVA and Pairwise comparisons, r package `afex`", 
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
               n = 680 (Studies 1a, &b), crowdsourced. Power: partial eta2 = .01."),
               "Study 1c; n = 1360, crowdsourced", "Study 1d; n = 1360, crowdsourced, following from previous power analysis")
Analysis2 <- c("mediation::mediate","moderated mediation using mediation::test.modmed", 
               "moderated mediaiotn using mediation::test.modmed")
Interpretation2 <- c("Task complexity (low comprehension of partner's income)",
                     "low perceived deservingness in hi inequality, meritocracy condition",
                     "Task complexity (comprehending group membership, and allocation process)")

design2 <- data.frame(Question2, Hypothesis2, Sampling2, Analysis2, Interpretation2)


cgwtools::resave(design2, file = here::here("output", "manuscript_objects.RData"))
