library(kableExtra)
library(na.tools) # for all_na function

kable_summary <- function(x){
  kable(summary(x)) %>%
    kable_styling(full_width = F)}


na_tally <- function(data,groupvar,selection){
  tally_1 <-  {{data}} %>%   group_by({{groupvar}})%>% 
    dplyr::select({{selection}}) %>% 
    summarise_all(list(all_na))
  
  tally_2 <- tally_1 %>%   
    mutate(total_missing = rowSums(.[2:ncol(tally_1)])) 
  
  tally_3 <- subset(tally_2, select=c(1,ncol(tally_2),2:(ncol(tally_2)-1)))
  tally_3
}

var_na <- function(data,selection){
  {{data}} %>%    
    dplyr::select({{selection}}) %>% 
    summarise_all(colSums(is.na))
}

# Data exploration

## plots
library(rlang)
hist_plot <- function(mydf, myycol, mytitle) {
  ggplot2::ggplot(data= mydf,aes({{myycol}})) + geom_histogram(fill="#00C0AFAA", colour='grey') + 
    geom_vline(aes(xintercept=mean({{myycol}})),color="#40B0A6", linetype="dashed", size=1 ) +
    ggtitle(mytitle) + theme_minimal()
}

# stats
normality_stats <- function(data){
  print("skewness is")
  print(skewness(data))
  print("kurtosis is")
  print(kurtosis(data))
  jarque.test(data)
}


# center with 'colMeans()'
center_colmeans <- function(x) {
  xcenter = colMeans(x)
  x - rep(xcenter, rep.int(nrow(x), ncol(x)))
}

