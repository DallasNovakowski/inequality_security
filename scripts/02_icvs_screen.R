# after loading raw_data

library(kableExtra)

all_countries <- unique(raw_data$country)
num_countries <- length(unique(raw_data$country))

library(data.table) # for setnames function

kable_summary <- function(x){
  kable(summary(x)) %>%
    kable_styling(full_width = F)}


library(reshape) # cast function


# library(tidyselect)
library(na.tools) # for all_na function

prevention_min <- c("prev_burglar_alarm", "prev_special_door_locks", "prev_special_grills", "prev_high_fence", "motion_detector", "prev_caretaker_security", "gun_ownership")
prevention_mod <- c("prev_burglar_alarm", "prev_special_door_locks", "prev_special_grills", "prev_a_watch_dog", "prev_high_fence", "motion_detector", "prev_caretaker_security", "gun_ownership")
prevention_max <- c("prev_burglar_alarm", "prev_special_door_locks", "prev_special_grills", "prev_a_watch_dog", "prev_high_fence", "prev_caretaker_security", "prev_caretaker_security", "prev_watch_scheme","firearm_incl_airrifle" )

prevention_others <- c("prev_other", "prev_insurance" , "arrangement_with_neighbours", "prev_do_not_know", "prev_keep_lights_on")

library(pander)

data_2005 = filter(raw_data,sweep_num == 5 |sweep_num == "5*") 
responders_2005 = filter(data_2005, is.na(prev_refusal))

prevention_min1 <- c("prev_burglar_alarm", "prev_special_door_locks", "prev_special_grills", "prev_high_fence", "prev_caretaker_security", "gun_ownership")

na_tally <- function(data,groupvar,selection){
tally_1 <-  {{data}} %>%   group_by({{groupvar}})%>% 
    dplyr::select({{selection}}) %>% 
    summarise_all(list(all_na))

    tally_2 <- tally_1 %>%   
      mutate(total_missing = rowSums(.[2:ncol(tally_1)])) 
    
    tally_3 <- subset(tally_2, select=c(1,ncol(tally_2),2:(ncol(tally_2)-1)))
    tally_3
    }



responders_cati <- filter(responders_2005, questionnaire_based_on == "cati 2000") 
responders_country_spec <- filter(responders_2005, questionnaire_used == "country specific") 
responders_hk <- filter(responders_2005, country == "Hong Kong")



responder_countries_2005 <- names(summary(responders_2005$country)[0!= summary(responders_2005$country)]) 

prevention_min2 <- c("prev_burglar_alarm", "prev_special_door_locks", "prev_special_grills", "prev_high_fence", "prev_caretaker_security", "gun_ownership")


# importing national data
library(purrr)
load("C:/Users/dalla/Google Drive/R Coding/swiid9_0/swiid9_0.rda")

swidd_plot <- swiid_summary %>%
  filter(country == responder_countries_2005) %>%
  ggplot(aes(x=year, y=gini_disp, colour = country)) +
  geom_line() +
  geom_ribbon(aes(ymin = gini_disp-1.96*gini_disp_se,
                  ymax = gini_disp+1.96*gini_disp_se,
                  linetype=NA), alpha = .25) +
  scale_x_continuous(breaks=seq(1960, 2015, 5)) +
theme_bw() +
  labs(x = "Year",
       y = "SWIID Gini Index, Disposable Income",
       title = "Income Inequality over countries")

unique(swiid[[1]]$country)

swiid_responders <-  swiid %>%
  map(. %>% filter(.,country%in% responder_countries_2005)) %>%
  map(. %>% filter(.,year == 2003 | year == 2004 | year == 2005 |year == 2006|year == 2007)) %>%
  map(. %>% select(country,year,gini_disp))
        
unique(swiid_responders[[1]]$country)
  
swiid_spread <- swiid_responders %>%
  map(.%>% spread(., year,gini_disp))


swiid_spread <- swiid_responders %>%
  map(.%>% pivot_wider(names_from = year, names_prefix ="gini_", values_from =gini_disp))

swiid_2004_6 <- swiid_spread %>%
  map(.%>% rowwise() %>%
  mutate(gini_2004_6 = mean(c(gini_2004, gini_2005, gini_2006))))

#Below we first load the PWT dataset
#and use it to generate a dataset of GDP per capita (in thousands of dollars). Then we load the WVS
#data, generate our variables of interest, and merge in our PWT data. Finally, we use purrr::map()
#to merge these data into each of the 100 SWIID dataframes.

# Get GDP per capita data from the Penn World
# Tables, Version 10 (Feenstra et al. 2015)

library(readxl)

pwt <- read_excel("C:/Users/dalla/Google Drive/R Coding/pwt100.xlsx", sheet = "Data")

pwt100_gdppc <- pwt %>% 
  transmute(country = country,
 year = year,
 gdppc = rgdpe/pop) %>%
  filter(!is.na(gdppc))

pwt_responders <-  pwt100_gdppc %>%
  filter(country%in% responder_countries_2005) %>% 
  filter(year == 2003 | year == 2004 | year == 2005 |year == 2006|year == 2007)

pwt_spread <- spread(pwt_responders, year,gdppc) 

colnames(pwt_spread)[2:6] = c("gdppc_2003", "gdppc_2004", "gdppc_2005", "gdppc_2006", "gdppc_2007") 

pwt_2004_6 <- pwt_spread %>%
  rowwise() %>%
  mutate(gdppc_2004_6 = mean(c(gdppc_2004, gdppc_2005, gdppc_2006)))

#merge

pwt_swiid <- swiid_2004_6 %>%
map(. %>% left_join(pwt_2004_6, by = c("country")))


#Again, we use purrr::map(), this time to estimate our model on
#each of the 100 different dataframes. Continuing with our example, we estimate a three-level
#linear mixed-effects model of individual responses nested in country-years nested in countries using
#lme4::lmer() (Bates et al. 2015).
#5

#nation_data$gini <- nation_data$lis_gini_2004_6

#nation_data[,c("country","gini", "wb_gini_2004_6")]

# add missing gini values from WB and 2007 LIS
#nation_data$gini <- ifelse(nation_data$country == "Argentina"| nation_data$country== "Bulgaria"| nation_data$country == "Portugal", 
#          nation_data$wb_gini_2004_6 -.0233, nation_data$gini)

#nation_data$gini <- ifelse(nation_data$country == "Lithuania",.362, ifelse(nation_data$country == "New Zealand",.31, nation_data$gini))

#nation_agg <- nation_data  %>%dplyr::select("country", ends_with("2004_6"), ends_with("gini"))

#nation_agg_1 = filter(nation_agg, !is.na(lis_gini_2004_6))

#nation_agg_2 = filter(nation_agg_1, !is.na(wb_gini_2004_6))

#nation_agg_short <- nation_agg[,c("country","pop_2004_6","gni_cap_currppp_2004_6","gdp_cap_currppp_2004_6","gini")]

# summary(jan1$country)




#jan_data2 <- merge(jan_data2, nation_agg_short, by="country", all = TRUE)

#nation_sum <- summary(jan_data2[,c("country","pop_2004_6","gni_cap_currppp_2004_6","gdp_cap_currppp_2004_6","gini")])



