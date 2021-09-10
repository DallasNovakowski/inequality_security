# Import and sav_clean data with janitor()

library(haven) # haven for reading .sav file
library(janitor)
library(stats) # for stats package
library(tidyverse)
library(dplyr) # for glimpse and filter functions
library(data.table)
library(knitr)

rawest_data <- read_sav("C:/Users/dalla/Google Drive/R Coding/icvs_inequality/data/ICVS2005_3.sav")  # Reading data


raw_classes <- lapply(as.data.frame(lapply(rawest_data, class)),tail,1)

jan_data <- rawest_data %>% 
  mutate_all(as_factor) %>%   # convert all variables from chr+lbl to fct type          
  set_names(rawest_data %>%       # convert variable names to informative labels
              sjlabelled::get_label() %>% # pull labels from original tibble
              enframe() %>%               # convert list of column names to a new tibble
              na_if("") %>%               # if variable label is empty string, convert to NA
              mutate(value = coalesce(value, name)) %>%  # fill in NA with original if missing
              pull(value)) %>%            # extract new variable name into a character vector
  janitor::clean_names()      # clean names to be all lowercase, replacing spaces with "_"     


setnames(jan_data, old=c("i002a","i002b"), new=c("sweep_year", "sweep_num"), skip_absent=TRUE)

jan_names <- names(jan_data)



jan_data1 <- rawest_data %>% 
  mutate_all(as.character) %>%   # convert all variables from chr+lbl to fct type   
  set_names(rawest_data %>%       # convert variable names to informative labels
              sjlabelled::get_label() %>% # pull labels from original tibble
              enframe() %>%               # convert list of column names to a new tibble
              na_if("") %>%               # if variable label is empty string, convert to NA
              mutate(value = coalesce(value, name)) %>%  # fill in NA with original if missing
              pull(value)) %>%            # extract new variable name into a character vector
  janitor::clean_names()      # clean names to be all lowercase, replacing spaces with "_"     

#attach names and classes to cleaned dataset
class(jan_data1) <- raw_classes
names(jan_data1) <-jan_names

jan_data2 <- jan_data

# substituting specific british isles regions with UK
jan_data2$country[jan_data2$country %in% c("England & Wales", "Scotland", "Northern Ireland")] <- "United Kingdom"

jan_data2$country<- recode(jan_data2$country, "Hong Kong (SAR China)"= "Hong Kong",
                           "USA" = "United States")


# check on classes
jan_classes <- lapply(as.data.frame(lapply(jan_data2, class)),tail,1)

raw_data <- jan_data2