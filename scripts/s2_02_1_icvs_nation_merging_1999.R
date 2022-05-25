
library(tidyverse)
library(purrr)

load(file = "C:/Users/dalla/Google Drive/offline_data_files/icvs_pwt_swiid/data/data_1999.RData")

load("C:/Users/dalla/Google Drive/offline_data_files/icvs_pwt_swiid/data/swiid9_0.rda")

#republic of korea, rumania, ukrain

swiid_1999 <-  swiid %>%
  map(. %>% filter(.,country %in% responders_1999_countries )) %>%
  map(. %>% filter(.,year == 1999 | year == 2000 | year == 2001 |year == 2002 |year == 2003)) %>%
  map(. %>% select(country,year,gini_disp)) %>%
  map(.%>% pivot_wider(names_from = year, names_prefix ="gini_", values_from =gini_disp))%>%
  map(.%>% rowwise() %>%
        dplyr::mutate(gini_1999_03 = mean(c(gini_1999, gini_2000, gini_2001,gini_2002,gini_2003))))

for (i in 1:length(swiid_1999)) {
  swiid_1999[[i]][["gini_1999_03_cent"]] <-  swiid_1999[[i]][["gini_1999_03"]] - mean(swiid_1999[[i]][["gini_1999_03"]])
  swiid_1999[[i]][["gini_1999_03_winz"]] <-  DescTools::Winsorize(swiid_1999[[i]][["gini_1999_03"]])
  swiid_1999[[i]][["gini_1999_03_wc"]] <-  swiid_1999[[i]][["gini_1999_03_winz"]] - mean(swiid_1999[[i]][["gini_1999_03_winz"]])
  swiid_1999[[i]][["gini_1999_03_ws"]] <- scale(swiid_1999[[i]][["gini_1999_03_winz"]])
}

pwt <- readxl::read_excel("C:/Users/dalla/Google Drive/offline_data_files/icvs_pwt_swiid/data/pwt100.xlsx", sheet = "Data")


# changed from rgdpe
pwt100_gdppc <- pwt %>% 
  transmute(country = country,
            year = year,
            gdppc = cgdpe/pop) %>%
  filter(!is.na(gdppc))

pwt100_gdppc$country<- dplyr::recode(pwt100_gdppc$country, "Eswatini"= "Swaziland",
                                  "Republic of Korea" = "Korea", "Russian Federation" = "Russia")


pwt_1999 <-  pwt100_gdppc %>%
  filter(country %in% names(summary(responders_1999$country)[0!= summary(responders_1999$country)]) ) %>%
  filter(year == 1999 | year == 2000 | year == 2001 |year == 2002 |year == 2003)

rm(pwt100_gdppc, pwt)

pwt_spread <- tidyr::spread(pwt_1999, year,gdppc) 


pwt_1999 <- pwt_spread %>%
  dplyr::rowwise() %>%
  dplyr::mutate(gdppc_1999_03 = mean(c(`1999`, `2000`, `2001`,`2002`,`2003`)))


pwt_1999 <- pwt_1999 %>%
  filter(country %in% responders_1999_countries )


# standardize and centre GDP
pwt_1999[,c("gdppc_1999_03_scale")] <- scale(pwt_1999[,c("gdppc_1999_03")])
# mean centering
pwt_1999[["gdppc_1999_03_cent"]] <-  pwt_1999[["gdppc_1999_03"]] - mean(pwt_1999[["gdppc_1999_03"]])
# Winzorizing
#GDP
pwt_1999[["gdppc_1999_03_winz"]] <-  DescTools::Winsorize(pwt_1999[["gdppc_1999_03"]])
pwt_1999[["gdppc_1999_03_wc"]] <-  DescTools::Winsorize(pwt_1999[["gdppc_1999_03_cent"]])
pwt_1999[["gdppc_1999_03_ws"]] <-  DescTools::Winsorize(pwt_1999[["gdppc_1999_03_scale"]])


data_list <- swiid_1999 %>% reduce(inner_join, by = "country")
# create a column with mean of all gini_2004 values
data_list$av_gini <- data_list %>%   
  dplyr::select(starts_with("gini_1999_03.")) %>%
  rowMeans()


data_list <- data_list[,c("country", "av_gini")]


data_list <- data_list %>%
  left_join(pwt_1999[,c("country", "gdppc_1999_03")], by = c("country"))

data_list$country <- as.factor(data_list$country)

# standardize and centre GDP
data_list[,c("gdppc_1999_03_scale")] <- scale(data_list[,c("gdppc_1999_03")])

# mean centering
#GDP
data_list[["gdppc_1999_03_cent"]] <-  data_list[["gdppc_1999_03"]] - mean(data_list[["gdppc_1999_03"]])
# gini
data_list[["gini_1999_03_cent"]] <-  data_list[["av_gini"]] - mean(data_list[["av_gini"]])

# Winzorizing
#GDP
data_list[["gdppc_1999_03_winz"]] <-  DescTools::Winsorize(data_list[["gdppc_1999_03"]])
data_list[["gdppc_1999_03_wc"]] <-  DescTools::Winsorize(data_list[["gdppc_1999_03_cent"]])
data_list[["gdppc_1999_03_ws"]] <-  DescTools::Winsorize(data_list[["gdppc_1999_03_scale"]])
#gini
data_list[["gini_1999_03_winz"]] <-  DescTools::Winsorize(data_list[["av_gini"]])
data_list[["gini_1999_03_wc"]] <-  DescTools::Winsorize(data_list[["gini_1999_03_cent"]])


responders_1999_2 <- data_list %>%
  left_join(responders_1999, by = c("country"))

nation_vars_1999 <- responders_1999_2 %>% 
  filter(!is.na(num_victim_5yr)) %>%
  group_by(country) %>% 
  summarise(gdppc_1999_03 = mean(gdppc_1999_03),
            gdppc_1999_03_winz = mean(gdppc_1999_03_winz),
            gdppc_1999_03_cent = mean(gdppc_1999_03_cent),
            num_victim_5yr  = mean(num_victim_5yr),
            # victim_nation_winz = mean(victim_nation_winz),
            num_victim_5yr_winz = mean(num_victim_5yr_winz),
            num_victim_5yr_wc = mean(num_victim_5yr_wc),
            total_security = mean(total_security),
            security_winz = mean(security_winz),
            gini_1999_03  = mean(av_gini),
            gini_1999_03_cent = mean(gini_1999_03_cent),
            gini_1999_03_winz = mean(gini_1999_03_winz)
  )


cgwtools::resave(nation_vars_1999, file = "C:/Users/dalla/Google Drive/offline_data_files/icvs_pwt_swiid/data/data_1999.RData")

rm(responders_1999_2)

responders_1999_joined <- swiid_1999 %>%
  map(. %>% left_join(pwt_1999, by = c("country"))) %>%
  map(. %>% left_join(responders_1999, by = c("country")))

save(responders_1999_joined, 
     file = "C:/Users/dalla/Google Drive/offline_data_files/icvs_pwt_swiid/data/responders_1999_joined.RData")


responders_1999_joined1 <- responders_1999_joined[[1]]

rm(responders_1999_joined,swiid_1999)

save(responders_1999_joined1, file = "C:/Users/dalla/Google Drive/offline_data_files/icvs_pwt_swiid/data/icvs_joined_samples_1999.RData")

rm(responders_1999_joined1)
