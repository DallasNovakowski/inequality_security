inline_extract <- function(model,coef_name){
  test_stat <- model$coefficients[coef_name,"t value"]
  df <- model$coefficients[coef_name,"df"]
  
  pval <- ifelse(model$coefficients[coef_name,"Pr(>|t|)"] < .001,
                 "< .001",
                 paste("=", model$coefficients[coef_name,"Pr(>|t|)"] %>% round(3)))
  
  estimate <- model$coefficients[coef_name,"Estimate"]
  se <- model$coefficients[coef_name,"Std. Error"]
  ci_lo <- estimate-se*1.96
  ci_hi <- estimate+se*1.96
  
  # print(c(test_stat, df, pval, estimate, se))
  
  paste("*t*","(",round(df,0),")"," = ", round(test_stat,2), ", *p* ", 
        pval, ", *b* = ", round(estimate,3),  
        ", CI(95%) = [", round(ci_lo,3), ", ", round(ci_hi,3),"]",  sep="")
}


percent <- function(x, digits = 0, format = "f", ...) {      # Create user-defined function
  paste0(formatC(x * 100, format = format, digits = digits, ...), "%")
}


round_df <- function(x, digits) {
  numeric_columns <- sapply(x, mode) == 'numeric'
  x[numeric_columns] <-  round(x[numeric_columns], digits)
  x
}


tidy_extract <- function(data,varname){
  this_var <- data[data$term  == varname, ]
  
  
  test_stat <- this_var$z_score
  df <- this_var$df 
  
  pval <- ifelse(grepl("<",this_var$p_value, fixed = TRUE),
                 this_var$p_value,
                 paste("=", as.numeric(this_var$p_value) %>% round(3)))
  
  estimate <- this_var$estimate
  se <- this_var$std.error
  # ci_lo <- estimate-se*1.96
  # ci_hi <- estimate+se*1.96
  
  # print(c(test_stat, df, pval, estimate, se))
  
  paste("*z*", "(", df, ")", "=", round(test_stat,2),
        ", *p* ", pval,", *b* = ", round(estimate,3), 
        ", CI(95%) = ",this_var$CI95,  sep="")
}
