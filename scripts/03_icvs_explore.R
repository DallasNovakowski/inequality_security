

responders_2005[ , prevention_min2] <- sapply(responders_2005[ , prevention_min2],
                                             function(x) as.numeric(as.character(x)))

responders_2005$total_security <- rowSums(responders_2005[ , prevention_min2], na.rm = TRUE) * NA ^ (rowSums(!is.na(responders_2005[ , prevention_min2])) == 0)

responders_2005$total_securitprev_summaryy <- responders_2005$total_security

responders_2005$total_security[is.na(responders_2005$total_security)] = 0

# Data Viz
library(rlang)

hist_plot <- function(mydf, myycol, mytitle) {
  ggplot2::ggplot(data= mydf,aes({{myycol}})) + geom_histogram(fill="#00C0AFAA", colour='grey') + 
    geom_vline(aes(xintercept=mean({{myycol}})),color="#D55E00", linetype="dashed", size=1 )+  geom_density(alpha=.2, fill="#56B4E9AA", adjust = 4)+
    ggtitle(mytitle)
}

library(moments)

total_skew <- skewness(responders_2005$total_security)
total_kurtosis <-kurtosis(responders_2005$total_security)
total_jarque <- jarque.test(responders_2005$total_security)


responders_2005$sqrt_security <- sqrt(responders_2005$total_security) # square root transformation seems to provide something closer, so just use that?




hist_plot(responders_2005, sqrt_security, "Histogram of Total Security (Square root)")

sqrt_plot <- ggplot(responders_2005,aes(sqrt_security)) + geom_histogram(fill="#00C0AFAA", colour='grey') + 
  geom_vline(aes(xintercept=mean(sqrt_security)),color="#D55E00", linetype="dashed", size=1 )+  geom_density(alpha=.2, fill="#56B4E9AA", adjust = 4)


sqrt_skew <- skewness(responders_2005$sqrt_security)
sqrt_kurtosis <-kurtosis(responders_2005$sqrt_security)
sqrt_jarque <- jarque.test(responders_2005$sqrt_security)

ggsave("C:/Users/dalla/Google Drive/R Coding/icvs_inequality/figures/security_sqrt_plot.png", plot = sqrt_plot)

responders_2005$ordinal_security <- NA

responders_2005$ordinal_security <- ifelse(between(responders_2005$total_security,0,1), 1, NA)
responders_2005$ordinal_security[responders_2005$total_security == 0] <- 0 
responders_2005$ordinal_security[responders_2005$total_security > 1] <- 2



ordinal_plot <- ggplot(responders_2005,aes(ordinal_security)) + geom_histogram(fill="#00C0AFAA", colour='grey') + 
  geom_vline(aes(xintercept=mean(ordinal_security)),color="#D55E00", linetype="dashed", size=1 )+  geom_density(alpha=.2, fill="#56B4E9AA", adjust = 4)


ordinal_skew <- skewness(responders_2005$ordinal_security)
ordinal_kurtosis <-kurtosis(responders_2005$ordinal_security)
ordinal_jarque <- jarque.test(responders_2005$ordinal_security)