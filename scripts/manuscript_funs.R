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
        ", CI(95%) = [", round(ci_lo,2), ", ", round(ci_hi,2),"]",  sep="")
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
                 this_var$p_value, ifelse(as.numeric(this_var$p_value) >=.01,
                                          paste("=",round(as.numeric(this_var$p_value),2),
                                                paste("=", round(as.numeric(this_var$p_value),3)))
                                          )
                 )
                 
  
  estimate <- this_var$estimate
  se <- this_var$std.error
  # ci_lo <- estimate-se*1.96
  # ci_hi <- estimate+se*1.96
  
  # print(c(test_stat, df, pval, estimate, se))
  
  paste("*z*", "(", df, ")", "=", round(test_stat,2),
        ", *p* ", pval,", *b* = ", ifelse(estimate >= .01, round(estimate,2),round(estimate,3)), 
        ", CI(95%) = ",this_var$CI95,  sep="")
}

##DF for level-1 predictors is M-r-1, where M is number of level-1 units and r is the total number of explanatory variables
#DF for  level-2 predictors is N-q-1, where N is the number of clusters and q is the number of level-2 predictors


make_df <- function(tidy_df,m_sum){
  tidy_df$df <- NA
  
  tidy_df[tidy_df$obs_lvl %in% 1,"df"] <- format(as.numeric(m_sum$devcomp$dims["n"]) - 
                                                   as.numeric(m_sum$devcomp$dims["p"]) - 1,big.mark = ",", scientific = FALSE)
  
  tidy_df[tidy_df$obs_lvl %in% 2,"df"] <- format(as.numeric(m_sum$devcomp$dims["q"]) - sum(tidy_df$obs_lvl==2) - 1,
                                                 big.mark = ",", scientific = FALSE)
  
  tidy_df <- tidy_df %>% relocate(df, .before = estimate)
  tidy_df <- tidy_df %>% relocate(obs_lvl, .before = df)
  
  tidy_df
}

make_ci <- function(tidied){
  tidied$ci95_lo <- tidied$estimate - 1.96*tidied$std.error 
  tidied$ci95_hi <- tidied$estimate + 1.96*tidied$std.error 
  
  tidied$z_score <- tidied$estimate/tidied$std.error #Wald tests
  tidied$p_value <-  as.numeric(format(2*pnorm(abs(tidied$z_score), lower.tail=FALSE), 
                                       scientific = FALSE))
  
  tidied$p_value <- ifelse(tidied$p_value < .001,
         "< .001",ifelse(tidied$p_value >= .01,
                         round(tidied$p_value,2), round(tidied$p_value,3)))
  
  tidied$CI95 <- paste("[", round(tidied$ci95_lo,2), ", ", 
                       round(tidied$ci95_hi,2),"]",  sep="")
  
  tidied <- round_df(tidied, 2)
  
  tidied <- subset(tidied, select = -c(ci95_lo,ci95_hi))
  
  tidied <- tidied %>% relocate(CI95, .before = z_score)
}


tidy_lmer <- function(sum_data){
  w_data <- data.frame(sum_data$coefficients)
  w_data <- rename(w_data, estimate = "Estimate"
                   , SE  = "Std..Error")
  w_data$ci95_hi <- w_data$estimate + 1.96*w_data$SE # = 0.4539815 
  
  # w_data$z_score <- w_data$estimate/w_data$SE #Wald tests
  
  w_data$ci95_lo <- w_data$estimate - 1.96*w_data[,"SE"] # = 0.4539815
  
  w_data$estimate <- round(w_data$estimate,2)
  w_data$t.value <- round(w_data$t.value,2)
  
  w_data$p_value <-  as.numeric(2*pnorm(abs(w_data$t.value), lower.tail=FALSE))
  
  # w_data$p_value <-  ifelse(w_data$p_value < .01,
  #                           paste(w_data$p_value %>% round(3)), ifelse(w_data$p_value >= .01,round(w_data$p_value,2),NA
  #                           ))
  
  w_data$CI95 <- paste("[", round(w_data$ci95_lo,2), ", ", 
                       round(w_data$ci95_hi,2),"]",  sep="")
  w_data <- round_df(w_data, 3)
  w_data <- subset(w_data, select = -c(ci95_lo,ci95_hi))
  w_data <- w_data %>% relocate(CI95, .before = t.value)
  w_data <- w_data %>%
    select(-contains(c("Pr...t..")))
  
  return(w_data)
}

firstup <- function(x) {
  substr(x, 1, 1) <- toupper(substr(x, 1, 1))
  x
}