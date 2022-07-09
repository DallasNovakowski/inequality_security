tidy_mixed_med <- function(med_model){
  bt_effect <- c("Indirect Effect (ACME)", "Direct Effect (ADE)", "Total Effect", 
                 "Prop. Mediated")
  bt_est <- c(med_model$d1, med_model$z1, med_model$tau.coef, med_model$n1)
  bt_cilo <- unname(c(med_model$d1.ci[1], med_model$z1.ci[1], med_model$tau.ci[1], med_model$n1.ci[1]))
  bt_cihi <- unname(c(med_model$d1.ci[2], med_model$z1.ci[2], med_model$tau.ci[2], med_model$n1.ci[2]))
  
  bt_ci95 <- paste("[", round(bt_cilo,2), ", ", 
                   round(bt_cihi,2),"]",  sep="")
  
  #bt_p <- format.pval(c(med_model$d1.p, med_model$z1.p, med_model$tau.p, med_model$n1.p))
  bt_p <- c(med_model$d1.p, med_model$z1.p, med_model$tau.p, med_model$n1.p)
  bt_stars <- c(stars.pval(med_model$d1.p), stars.pval(med_model$z1.p),
                stars.pval(med_model$tau.p), stars.pval(med_model$n1.p))
  
  # bt_p <- paste(bt_p,bt_stars)
  
  bt_DF <- data.frame(row.names = bt_effect, round(bt_est,2),bt_ci95,
                      bt_p
                      # , bt_stars
  )
  colnames(bt_DF) <- c("estimate", "ci95", "p_value")

    return(bt_DF)
}

