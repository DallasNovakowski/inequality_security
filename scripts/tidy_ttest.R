# https://www.matthewvanaman.com/post/2021-02-05-tidy-t-test-in-r-with-cohen-s-d/

tidy_ttest <-
  function(data, iv, dv, long = FALSE, ...) {
    require(dplyr)
    require(effectsize)
    require(broom)
    require(stringr)
    means.SDs <- suppressMessages(
      data %>%
        group_by_at(iv) %>%
        summarise(
          mean = mean(eval(parse(text = dv)), na.rm = TRUE),
          sd = sd(eval(parse(text = dv)), na.rm = TRUE)) %>%
        pivot_wider(names_from = iv, values_from =  c("mean", "sd"), names_sep = ".")
    )
    iv.2 <- rlang::sym(iv)
    dv.2 <- rlang::sym(dv)
    form <- expr(!! dv.2 ~ !! iv.2)
    t.tests <- t.test(eval(form), data, var.equal = FALSE, ...) 
    Ds <- effectsize::cohens_d(eval(form), data = data, ...) %>% as.tibble
    stats <- t.tests %>% tidy
    std.err <- t.tests$stderr
    cols <- c("conf.low", "conf.high", "statistic", "parameter", "p.value")
    means.table <-
      cbind(means.SDs, stats[, "estimate"], std.err, stats[, cols], select(Ds, -CI))
    means.table <- means.table %>%
      rename(
        mean.difference = estimate,
        t = statistic,
        df = parameter,
        ci.dif.lo = conf.low,
        ci_dif_hi = conf.high,
        cohens.d = Cohens_d,
        ci.d.lo = CI_low,
        ci.d.hi = CI_high
      ) %>%
      mutate_if(is.numeric, round, 3)
    rownames(means.table) <- NULL
    if (long == TRUE) {
      means.table.long <-
        means.table %>% t %>% data.frame("Value" = .) %>% rownames_to_column("Statistic")
      return(means.table.long)
    } else{
      return(means.table)
    }
  }

report_tidy_t <- function(tidy_frame, is.first = FALSE){
  paste("*t*","(",tidy_frame$df,")"," = ", round(tidy_frame$t,2), ", *p* ", 
        
        ifelse(tidy_frame$p.value < .001, "< .001",
               ifelse(tidy_frame$p.value > .01,paste("=", tidy_frame$p.value %>% round(2)),
                      paste("=", tidy_frame$p.value %>% round(3)))
               )
        ,
        ", *d* = ",
        round(tidy_frame$cohens.d,2), ifelse(is.first == TRUE,
                                             paste(", CI(95%) = [", round(tidy_frame$ci.d.lo,2), ", ", 
                                                   round(tidy_frame$ci.d.hi,2),"]",  sep=""),
                                             paste(" [", round(tidy_frame$ci.d.lo,2), ", ", round(tidy_frame$ci.d.hi,2),"]",  sep="")),  sep="") 
        
        
        
  }
