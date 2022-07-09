library(weights)

report_pval <- function(pval) {
  ifelse(pval < .001, "<.001",
         ifelse(pval >= .01,weights::rd(pval,2),
                weights::rd(pval,3)
                )
         )
}

