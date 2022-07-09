#designed for in-text reporting of anova results (generated from r parckage afex), 
# according to APA standards

anova_extract <- function(model,coef_name,is.first = FALSE){
  test_stat <- round(as.numeric(model[coef_name,"F"]),2)
  df <- paste(round(model[coef_name,"NumDF"],2), round(model[coef_name,"DenDF"],2),sep = ", ")
  
  pval <- ifelse(model[coef_name,"Pr(>F)"] < .001,
                 "< .001",ifelse(model[coef_name,"Pr(>F)"] >= .01,
                                 paste("=", round(model[coef_name,"Pr(>F)"],2)), paste("=", round(model[coef_name,"Pr(>F)"],3))))
  
  estimate <- model[coef_name,"cohens_f"]
  # se <- model[coef_name,"Std. Error"]
  ci_lo <- model[coef_name,"ci95_lo"]
  ci_hi <- model[coef_name,"ci95_hi"]
  
  # print(c(test_stat, df, pval, estimate, se))
  
  paste0("*F*","(",df,")"," = ", round(test_stat,2), ", *p* ", 
        pval,
        # ", *$\\eta^{2}_p$* = ", 
        ", Cohen's *f* = ",
        round(estimate,2),  
        ", ",
        if (is.first==TRUE) {
          paste0("CI(95%) = ")
        },
        "[", round(ci_lo,2), ", ", round(ci_hi,2),"]"
        
        )
}