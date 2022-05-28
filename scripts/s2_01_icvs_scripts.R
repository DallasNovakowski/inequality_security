library(kableExtra)
library(na.tools) # for all_na function
library(cowplot)

## ---- data-cleaning  --------

sav_clean <- function(data){
  data <- data %>% 
    mutate_all(as_factor) %>%   # convert all variables from chr+lbl to fct type          
    set_names(data %>%       # convert variable names to informative labels
                sjlabelled::get_label() %>% # pull labels from original tibble
                enframe() %>%               # convert list of column names to a new tibble
                na_if("") %>%               # if variable label is empty string, convert to NA
                dplyr::mutate(value = coalesce(value, name)) %>%  # fill in NA with original if missing
                pull(value)) %>%            # extract new variable name into a character vector
    janitor::clean_names()      # clean names to be all lowercase, replacing spaces with "_"     
  return(data)
}


## ---- making-tables --------
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

## ---- plots --------
# The palette with grey:
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

library(rlang)
library(scales)
hist_plot <- function(mydf, myycol
                      # , mytitle
                      ) {
  ggplot2::ggplot(data= mydf,aes({{myycol}})) + geom_bar(position='dodge', width = .60, fill=cbPalette[[1]]) +
    geom_vline(aes(xintercept=mean({{myycol}})),color=cbPalette[[7]], linetype="dashed", size=1.5 ) +
    theme_half_open() +
    scale_y_continuous(
      # don't expand y scale at the lower end
      expand = expansion(mult = c(0, 0.05)), labels=comma_format(accuracy=1)
    ) + ylab("Count")
}
#  + theme_minimal()

big_hist_plot <- function(mydf, myycol
                      # , mytitle
) { ggplot2::ggplot(data= mydf,aes({{myycol}})) + geom_histogram(fill=cbPalette[[1]]) +
    geom_vline(aes(xintercept=mean({{myycol}})), color=cbPalette[[7]], linetype="dashed", size=1.5 ) +
    theme_half_open() +
    scale_y_continuous(
      # don't expand y scale at the lower end
      expand = expansion(mult = c(0, 0.05)), labels=comma_format(accuracy=1)
    ) + ylab("Count")
}


## ---- descriptive-stats --------
normality_stats <- function(data){
  print("skewness is")
  print(moments::skewness(data))
  print("kurtosis is")
  print(moments::kurtosis(data))
  moments::jarque.test(data)
}

## ---- variable-transform --------
# center with 'colMeans()'
center_colmeans <- function(x) {
  xcenter = colMeans(x)
  x - rep(xcenter, rep.int(nrow(x), ncol(x)))
}

