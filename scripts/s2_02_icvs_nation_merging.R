load("C:/Users/dalla/Google Drive/offline_data_files/icvs_pwt_swiid/data/icvs_solo_datafiles.RData")

load("C:/Users/dalla/Google Drive/offline_data_files/icvs_pwt_swiid/data/iv_2005.RData")

library(tidyverse)

source(here::here("scripts", "s2_01_icvs_scripts.r"), local = knitr::knit_global())


# Creating four ICVS datasets with different inclusion criteria



# Summarizing the SWIID
library(purrr)
load("C:/Users/dalla/Google Drive/offline_data_files/icvs_pwt_swiid/data/swiid9_0.rda")

# swiid_iv <-  swiid %>%
#   map(. %>% filter(.,country %in% names(summary(iv_2005$country)[0!= summary(iv_2005$country)]) )) %>%
#   map(. %>% filter(.,year == 2003 | year == 2004 | year == 2005 |year == 2006|year == 2007)) %>%
#   map(. %>% select(country,year,gini_disp)) %>%
#   map(.%>% pivot_wider(names_from = year, names_prefix ="gini_", values_from =gini_disp))%>%
#   map(.%>% rowwise() %>%
#         dplyr::mutate(gini_2004_6 = mean(c(gini_2004, gini_2005, gini_2006))))
# 
# for (i in 1:length(swiid_iv)) {
#   swiid_iv[[i]][["gini_2004_6_cent"]] <-  swiid_iv[[i]][["gini_2004_6"]] - mean(swiid_iv[[i]][["gini_2004_6"]])
#   swiid_iv[[i]][["gini_2004_6_winz"]] <-  DescTools::Winsorize(swiid_iv[[i]][["gini_2004_6"]])
#   swiid_iv[[i]][["gini_2004_6_wc"]] <-  swiid_iv[[i]][["gini_2004_6_winz"]] - mean(swiid_iv[[i]][["gini_2004_6_winz"]])
#   swiid_iv[[i]][["gini_2004_6_ws"]] <- scale(swiid_iv[[i]][["gini_2004_6_winz"]])
# }



swiid_iv <-  swiid %>%
  map(. %>% filter(.,country %in% iv_2005_countries )) %>%
  map(. %>% filter(.,year == 2003 | year == 2004 | year == 2005 |year == 2006|year == 2007)) %>%
  map(. %>% select(country,year,gini_disp)) %>%
  map(.%>% pivot_wider(names_from = year, names_prefix ="gini_", values_from =gini_disp))%>%
  map(.%>% rowwise() %>%
        dplyr::mutate(gini_2004_6 = mean(c(gini_2004, gini_2005, gini_2006))))

for (i in 1:length(swiid_iv)) {
  swiid_iv[[i]][["gini_2004_6_cent"]] <-  swiid_iv[[i]][["gini_2004_6"]] - mean(swiid_iv[[i]][["gini_2004_6"]])
  swiid_iv[[i]][["gini_2004_6_winz"]] <-  DescTools::Winsorize(swiid_iv[[i]][["gini_2004_6"]])
  swiid_iv[[i]][["gini_2004_6_wc"]] <-  swiid_iv[[i]][["gini_2004_6_winz"]] - mean(swiid_iv[[i]][["gini_2004_6_winz"]])
  swiid_iv[[i]][["gini_2004_6_ws"]] <- scale(swiid_iv[[i]][["gini_2004_6_winz"]])
}


swiid_mod <-  swiid %>%
  map(. %>% filter(.,country %in% iv_2005_mod_countries )) %>%
  map(. %>% filter(.,year == 2003 | year == 2004 | year == 2005 |year == 2006|year == 2007)) %>%
  map(. %>% select(country,year,gini_disp)) %>%
  map(.%>% pivot_wider(names_from = year, names_prefix ="gini_", values_from =gini_disp))%>%
  map(.%>% rowwise() %>%
        dplyr::mutate(gini_2004_6 = mean(c(gini_2004, gini_2005, gini_2006))))

for (i in 1:length(swiid_mod)) {
  swiid_mod[[i]][["gini_2004_6_cent"]] <-  swiid_mod[[i]][["gini_2004_6"]] - mean(swiid_mod[[i]][["gini_2004_6"]])
  swiid_mod[[i]][["gini_2004_6_winz"]] <-  DescTools::Winsorize(swiid_mod[[i]][["gini_2004_6"]])
  swiid_mod[[i]][["gini_2004_6_wc"]] <-  swiid_mod[[i]][["gini_2004_6_winz"]] - mean(swiid_mod[[i]][["gini_2004_6_winz"]])
  swiid_mod[[i]][["gini_2004_6_ws"]] <- scale(swiid_mod[[i]][["gini_2004_6_winz"]])
}


swiid_lib <-  swiid %>%
  map(. %>% filter(.,country %in% iv_2005_lib_countries )) %>%
  map(. %>% filter(.,year == 2003 | year == 2004 | year == 2005 |year == 2006|year == 2007)) %>%
  map(. %>% select(country,year,gini_disp)) %>%
  map(.%>% pivot_wider(names_from = year, names_prefix ="gini_", values_from =gini_disp))%>%
  map(.%>% rowwise() %>%
        dplyr::mutate(gini_2004_6 = mean(c(gini_2004, gini_2005, gini_2006))))

for (i in 1:length(swiid_lib)) {
  swiid_lib[[i]][["gini_2004_6_cent"]] <-  swiid_lib[[i]][["gini_2004_6"]] - mean(swiid_lib[[i]][["gini_2004_6"]])
  swiid_lib[[i]][["gini_2004_6_winz"]] <-  DescTools::Winsorize(swiid_lib[[i]][["gini_2004_6"]])
  swiid_lib[[i]][["gini_2004_6_wc"]] <-  swiid_lib[[i]][["gini_2004_6_winz"]] - mean(swiid_lib[[i]][["gini_2004_6_winz"]])
  swiid_lib[[i]][["gini_2004_6_ws"]] <- scale(swiid_lib[[i]][["gini_2004_6_winz"]])
}


swiid_lib1 <-  swiid %>%
  map(. %>% filter(.,country %in% iv_2005_lib1_countries )) %>%
  map(. %>% filter(.,year == 2003 | year == 2004 | year == 2005 |year == 2006|year == 2007)) %>%
  map(. %>% select(country,year,gini_disp)) %>%
  map(.%>% pivot_wider(names_from = year, names_prefix ="gini_", values_from =gini_disp))%>%
  map(.%>% rowwise() %>%
        dplyr::mutate(gini_2004_6 = mean(c(gini_2004, gini_2005, gini_2006))))

for (i in 1:length(swiid_lib1)) {
  swiid_lib1[[i]][["gini_2004_6_cent"]] <-  swiid_lib1[[i]][["gini_2004_6"]] - mean(swiid_lib1[[i]][["gini_2004_6"]])
  swiid_lib1[[i]][["gini_2004_6_winz"]] <-  DescTools::Winsorize(swiid_lib1[[i]][["gini_2004_6"]])
  swiid_lib1[[i]][["gini_2004_6_wc"]] <-  swiid_lib1[[i]][["gini_2004_6_winz"]] - mean(swiid_lib1[[i]][["gini_2004_6_winz"]])
  swiid_lib1[[i]][["gini_2004_6_ws"]] <- scale(swiid_lib1[[i]][["gini_2004_6_winz"]])
}

rm(swiid)

#swiid_spread <- swiid_iv %>%
# map(.%>% spread(., year,gini_disp))



# 
# swiid_spread <- swiid_iv %>%
#   map(.%>% pivot_wider(names_from = year, names_prefix ="gini_", values_from =gini_disp))
# 
# 
# swiid_2004_6 <- swiid_spread %>%
#   map(.%>% rowwise() %>%
#         dplyr::mutate(gini_2004_6 = mean(c(gini_2004, gini_2005, gini_2006))))


# swiid_2004_6 <- swiid_2004_6 %>%
#   map(~mutate(., gini_2004_6_cent = gini_2004_6 - mean(gini_2004_6)))
#   # a = mean(gini_2004_6)d
# 

# 
# for (i in 1:length(swiid_2004_6)) {
#     swiid_2004_6[[i]][["gini_2004_6_cent"]] <-  swiid_2004_6[[i]][["gini_2004_6"]] - mean(swiid_2004_6[[i]][["gini_2004_6"]])
#     swiid_2004_6[[i]][["gini_2004_6_winz"]] <-  DescTools::Winsorize(swiid_2004_6[[i]][["gini_2004_6"]])
#     swiid_2004_6[[i]][["gini_2004_6_wc"]] <-  swiid_2004_6[[i]][["gini_2004_6_winz"]] - mean(swiid_2004_6[[i]][["gini_2004_6_winz"]])
#     swiid_2004_6[[i]][["gini_2004_6_ws"]] <- scale(swiid_2004_6[[i]][["gini_2004_6_winz"]])
# }


# 
# df2 <- df %>% mutate_at(c('var1'), ~(scale(.) %>% as.vector))
# df2



# swiid_2004_6[[1]][["gini_2004_6_cent"]] <-  swiid_2004_6[[1]][["gini_2004_6"]] - mean(swiid_2004_6[[1]][["gini_2004_6"]])

#plot swiid
swiid_summary %>%
  filter(country == names(summary(iv_2005$country)[0!= summary(iv_2005$country)]) ) %>%
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


rm(swiid_summary)

## Living standards (PWT)


pwt <- readxl::read_excel("C:/Users/dalla/Google Drive/offline_data_files/icvs_pwt_swiid/data/pwt100.xlsx", sheet = "Data")

# changed from rgdpe
pwt100_gdppc <- pwt %>% 
  transmute(country = country,
            year = year,
            gdppc = cgdpe/pop) %>%
  filter(!is.na(gdppc))

pwt_iv <-  pwt100_gdppc %>%
  filter(country%in% names(summary(iv_2005$country)[0!= summary(iv_2005$country)]) ) %>% 
  filter(year == 2003 | year == 2004 | year == 2005 |year == 2006|year == 2007)

rm(pwt100_gdppc, pwt)

pwt_spread <- spread(pwt_iv, year,gdppc) 


pwt_2004_6 <- pwt_spread %>%
  dplyr::rowwise() %>%
  dplyr::mutate(gdppc_2004_6 = mean(c(`2004`, `2005`, `2006`)))


pwt_iv <- pwt_2004_6 %>%
  filter(country %in% iv_2005_countries )

# standardize and centre GDP
pwt_iv[,c("gdppc_2004_6_scale")] <- scale(pwt_iv[,c("gdppc_2004_6")])
# mean centering
pwt_iv[["gdppc_2004_6_cent"]] <-  pwt_iv[["gdppc_2004_6"]] - mean(pwt_iv[["gdppc_2004_6"]])
# Winzorizing
#GDP
pwt_iv[["gdppc_2004_6_winz"]] <-  DescTools::Winsorize(pwt_iv[["gdppc_2004_6"]])
pwt_iv[["gdppc_2004_6_wc"]] <-  DescTools::Winsorize(pwt_iv[["gdppc_2004_6_cent"]])
pwt_iv[["gdppc_2004_6_ws"]] <-  DescTools::Winsorize(pwt_iv[["gdppc_2004_6_scale"]])

pwt_mod <- pwt_2004_6 %>%
  filter(country %in% iv_2005_mod_countries )

# standardize and centre GDP
pwt_mod[,c("gdppc_2004_6_scale")] <- scale(pwt_mod[,c("gdppc_2004_6")])
# mean centering
pwt_mod[["gdppc_2004_6_cent"]] <-  pwt_mod[["gdppc_2004_6"]] - mean(pwt_mod[["gdppc_2004_6"]])
# Winzorizing
#GDP
pwt_mod[["gdppc_2004_6_winz"]] <-  DescTools::Winsorize(pwt_mod[["gdppc_2004_6"]])
pwt_mod[["gdppc_2004_6_wc"]] <-  DescTools::Winsorize(pwt_mod[["gdppc_2004_6_cent"]])
pwt_mod[["gdppc_2004_6_ws"]] <-  DescTools::Winsorize(pwt_mod[["gdppc_2004_6_scale"]])


pwt_lib <- pwt_2004_6 %>%
  filter(country %in% iv_2005_lib_countries )


# standardize and centre GDP
pwt_lib[,c("gdppc_2004_6_scale")] <- scale(pwt_lib[,c("gdppc_2004_6")])
# mean centering
pwt_lib[["gdppc_2004_6_cent"]] <-  pwt_lib[["gdppc_2004_6"]] - mean(pwt_lib[["gdppc_2004_6"]])
# Winzorizing
#GDP
pwt_lib[["gdppc_2004_6_winz"]] <-  DescTools::Winsorize(pwt_lib[["gdppc_2004_6"]])
pwt_lib[["gdppc_2004_6_wc"]] <-  DescTools::Winsorize(pwt_lib[["gdppc_2004_6_cent"]])
pwt_lib[["gdppc_2004_6_ws"]] <-  DescTools::Winsorize(pwt_lib[["gdppc_2004_6_scale"]])


pwt_lib1 <- pwt_2004_6 %>%
  filter(country %in% iv_2005_lib1_countries )


# standardize and centre GDP
pwt_lib1[,c("gdppc_2004_6_scale")] <- scale(pwt_lib1[,c("gdppc_2004_6")])
# mean centering
pwt_lib1[["gdppc_2004_6_cent"]] <-  pwt_lib1[["gdppc_2004_6"]] - mean(pwt_lib1[["gdppc_2004_6"]])
# Winzorizing
#GDP
pwt_lib1[["gdppc_2004_6_winz"]] <-  DescTools::Winsorize(pwt_lib1[["gdppc_2004_6"]])
pwt_lib1[["gdppc_2004_6_wc"]] <-  DescTools::Winsorize(pwt_lib1[["gdppc_2004_6_cent"]])
pwt_lib1[["gdppc_2004_6_ws"]] <-  DescTools::Winsorize(pwt_lib1[["gdppc_2004_6_scale"]])


# pwt_2004_6 %>%
#   mutate(gdppc_2004_6_cent = mean(gdppc_2004_6))


# ggplot(pwt_2004_6, aes(x = gdppc_2004_6)) + geom_density()

# ggplot(pwt_2004_6, aes(x = gdppc_2004_6))  +  geom_histogram(alpha=.7, fill="#40B0A6", colour='grey') + 
#   # + geom_density( aes(y=..count..)) + theme_minimal() 
#   geom_vline(aes(xintercept=mean(gdppc_2004_6)),color="#B03F49", linetype="dashed", size=1 ) + ggtitle("Histogram of average GDP per Capita over 2004-2006")


# Merging PWT and SWIID - create national dataframes



# create joined dataframe from all list of 100 dataframes for independent use
data_list <- swiid_iv %>% reduce(inner_join, by = "country")
# create a column with mean of all gini_2004 values
data_list$av_gini <- data_list %>%   
  dplyr::select(starts_with("gini_2004_6.")) %>%
  rowMeans()

# data_list$av_gini_cent <- data_list %>%   
#   dplyr::select(starts_with("gini_2004_6_cent.")) %>%
#   rowMeans()
# 
# 
# data_list$av_gini_winz <- data_list %>%   
#   dplyr::select(starts_with("gini_2004_6_winz.")) %>%
#   rowMeans()
# 
# data_list$av_gini_wc <- data_list %>%   
#   dplyr::select(starts_with("gini_2004_6_wc.")) %>%
#   rowMeans()
# 
# data_list$av_gini_ws <- data_list %>%   
#   dplyr::select(starts_with("gini_2004_6_ws.")) %>%
#   rowMeans()

# "av_gini_cent","av_gini_winz","av_gini_wc","av_gini_ws"

data_list <- data_list[,c("country", "av_gini")]

data_list <- data_list %>%
  filter(country != "Hong Kong") 

data_list <- data_list %>%
  left_join(pwt_2004_6[,c("country", "gdppc_2004_6")], by = c("country"))

data_list$country <- as.factor(data_list$country)

# standardize and centre GDP
data_list[,c("gdppc_2004_6_scale")] <- scale(data_list[,c("gdppc_2004_6")])

# mean centering
#GDP
data_list[["gdppc_2004_6_cent"]] <-  data_list[["gdppc_2004_6"]] - mean(data_list[["gdppc_2004_6"]])
# gini
data_list[["gini_2004_6_cent"]] <-  data_list[["av_gini"]] - mean(data_list[["av_gini"]])

# Winzorizing
#GDP
data_list[["gdppc_2004_6_winz"]] <-  DescTools::Winsorize(data_list[["gdppc_2004_6"]])
data_list[["gdppc_2004_6_wc"]] <-  DescTools::Winsorize(data_list[["gdppc_2004_6_cent"]])
data_list[["gdppc_2004_6_ws"]] <-  DescTools::Winsorize(data_list[["gdppc_2004_6_scale"]])
#gini
data_list[["gini_2004_6_winz"]] <-  DescTools::Winsorize(data_list[["av_gini"]])
data_list[["gini_2004_6_wc"]] <-  DescTools::Winsorize(data_list[["gini_2004_6_cent"]])



# creating nation-level data for variable-moderate countries
data_list_mod <- data_list %>%
  filter(country != "Japan" & country != "Peru"  & country != "Iceland") 

# standardize and centre GDP
data_list_mod[,c("gdppc_2004_6_scale")] <- scale(data_list_mod[,c("gdppc_2004_6")])

# mean centering
#GDP
data_list_mod[["gdppc_2004_6_cent"]] <-  data_list_mod[["gdppc_2004_6"]] - mean(data_list_mod[["gdppc_2004_6"]])
# gini
data_list_mod[["gini_2004_6_cent"]] <-  data_list_mod[["av_gini"]] - mean(data_list_mod[["av_gini"]])

# Winzorizing
#GDP
data_list_mod[["gdppc_2004_6_winz"]] <-  DescTools::Winsorize(data_list_mod[["gdppc_2004_6"]])
data_list_mod[["gdppc_2004_6_wc"]] <-  DescTools::Winsorize(data_list_mod[["gdppc_2004_6_cent"]])
data_list_mod[["gdppc_2004_6_ws"]] <-  DescTools::Winsorize(data_list_mod[["gdppc_2004_6_scale"]])
#gini
data_list_mod[["gini_2004_6_winz"]] <-  DescTools::Winsorize(data_list_mod[["av_gini"]])
data_list_mod[["gini_2004_6_wc"]] <-  DescTools::Winsorize(data_list_mod[["gini_2004_6_cent"]])


# creating nation-level data for variable-maximizing countries
data_list_lib <- data_list_mod %>%
  filter(country != "New Zealand") 

# standardize and centre GDP
data_list_lib[,c("gdppc_2004_6_scale")] <- scale(data_list_lib[,c("gdppc_2004_6")])

# mean centering
#GDP
data_list_lib[["gdppc_2004_6_cent"]] <-  data_list_lib[["gdppc_2004_6"]] - mean(data_list_lib[["gdppc_2004_6"]])
# gini
data_list_lib[["gini_2004_6_cent"]] <-  data_list_lib[["av_gini"]] - mean(data_list_lib[["av_gini"]])

# Winzorizing
#GDP
data_list_lib[["gdppc_2004_6_winz"]] <-  DescTools::Winsorize(data_list_lib[["gdppc_2004_6"]])
data_list_lib[["gdppc_2004_6_wc"]] <-  DescTools::Winsorize(data_list_lib[["gdppc_2004_6_cent"]])
data_list_lib[["gdppc_2004_6_ws"]] <-  DescTools::Winsorize(data_list_lib[["gdppc_2004_6_scale"]])
#gini
data_list_lib[["gini_2004_6_winz"]] <-  DescTools::Winsorize(data_list_lib[["av_gini"]])
data_list_lib[["gini_2004_6_wc"]] <-  DescTools::Winsorize(data_list_lib[["gini_2004_6_cent"]])


data_list_lib1 <- data_list %>%
  filter(country != "Iceland" & country != "New Zealand") 

# standardize and centre GDP
data_list_lib1[,c("gdppc_2004_6_scale")] <- scale(data_list_lib1[,c("gdppc_2004_6")])

# mean centering
#GDP
data_list_lib1[["gdppc_2004_6_cent"]] <-  data_list_lib1[["gdppc_2004_6"]] - mean(data_list_lib1[["gdppc_2004_6"]])
# gini
data_list_lib1[["gini_2004_6_cent"]] <-  data_list_lib1[["av_gini"]] - mean(data_list_lib1[["av_gini"]])

# Winzorizing
#GDP
data_list_lib1[["gdppc_2004_6_winz"]] <-  DescTools::Winsorize(data_list_lib1[["gdppc_2004_6"]])
data_list_lib1[["gdppc_2004_6_wc"]] <-  DescTools::Winsorize(data_list_lib1[["gdppc_2004_6_cent"]])
data_list_lib1[["gdppc_2004_6_ws"]] <-  DescTools::Winsorize(data_list_lib1[["gdppc_2004_6_scale"]])
#gini
data_list_lib1[["gini_2004_6_winz"]] <-  DescTools::Winsorize(data_list_lib1[["av_gini"]])
data_list_lib1[["gini_2004_6_wc"]] <-  DescTools::Winsorize(data_list_lib1[["gini_2004_6_cent"]])


# append nation-level data for cluster-maximizing data
iv_2005_2 <- data_list %>%
  left_join(iv_2005, by = c("country"))

gd <- iv_2005_2 %>% 
  filter(!is.na(num_victim_5yr)) %>%
  group_by(country) %>% 
  summarise(gdppc_2004_6 = mean(gdppc_2004_6),
            gdppc_2004_6_winz = mean(gdppc_2004_6_winz),
            gdppc_2004_6_cent = mean(gdppc_2004_6_cent),
            num_victim_5yr  = mean(num_victim_5yr),
            num_victim_5yr_winz = mean(num_victim_5yr_winz),
            gini_2004_6  = mean(av_gini),
            gini_2004_6_cent = mean(gini_2004_6_cent),
            gini_2004_6_winz = mean(gini_2004_6_winz)
  )


# append nation-level data for variable-maximizing data
iv_2005_mod2 <- data_list_mod %>%
  left_join(iv_2005_mod, by = c("country"))


gd_mod <- iv_2005_mod2 %>% 
  filter(!is.na(num_victim_5yr)) %>%
  group_by(country) %>% 
  summarise(gdppc_2004_6 = mean(gdppc_2004_6),
            gdppc_2004_6_winz = mean(gdppc_2004_6_winz),
            gdppc_2004_6_cent = mean(gdppc_2004_6_cent),
            num_victim_5yr  = mean(num_victim_5yr),
            num_victim_5yr_winz = mean(num_victim_5yr_winz),
            num_victim_5yr_assault = mean(num_victim_5yr_assault),
            num_victim_5yr_assault_winz = mean(num_victim_5yr_assault_winz),
            gini_2004_6  = mean(av_gini),
            gini_2004_6_cent = mean(gini_2004_6_cent),
            gini_2004_6_winz = mean(gini_2004_6_winz)
  )


cgwtools::resave(gd,gd_mod, file = "C:/Users/dalla/Google Drive/offline_data_files/icvs_pwt_swiid/data/iv_2005.RData")


# iv_2005_lib <- data_list_lib %>%
#   left_join(iv_2005_lib, by = c("country"))
# 
# iv_2005_lib1 <- data_list_lib1 %>%
#   left_join(iv_2005_lib1, by = c("country"))


rm(iv_2005_mod2,iv_2005_2)


# colnames(pwt_spread)[2:6] = c("gdppc_2003", "gdppc_2004", "gdppc_2005", "gdppc_2006", "gdppc_2007") 


# save(sweep5_nrow, sweep5_countries, vic_var_assault, vic_var_drop, prevention_min2, gd, gd_mod, pre_weightscreen_n, pre_weightscreen_k, post_weightscreen_n, post_weightscreen_k, iv_2005_countries, iv_2005_countries, iv_2005_mod_countries, iv_2005_lib_countries, iv_2005_lib1_countries, cleaned_countries, file = "C:/Users/dalla/Google Drive/offline_data_files/icvs_pwt_swiid/data/iv_2005.RData")


# icvs_joined <- swiid_2004_6 %>%
#   map(. %>% left_join(pwt_2004_6, by = c("country"))) %>%
#   map(. %>% left_join(iv_2005, by = c("country")))

# save(icvs_joined, file = "C:/Users/dalla/Google Drive/offline_data_files/icvs_pwt_swiid/data/icvs_swiid_pwt_joined.RData")

iv_joined <- swiid_iv %>%
  map(. %>% left_join(pwt_iv, by = c("country"))) %>%
  map(. %>% left_join(iv_2005, by = c("country")))

save(iv_joined, file = "C:/Users/dalla/Google Drive/offline_data_files/icvs_pwt_swiid/data/iv_joined.RData")

iv_joined1 <- iv_joined[[1]]

rm(iv_joined,swiid_iv)

save(iv_joined1, file = "C:/Users/dalla/Google Drive/offline_data_files/icvs_pwt_swiid/data/icvs_joined_samples.RData")

rm(iv_joined1)

mod_joined <- swiid_mod %>%
  map(. %>% left_join(pwt_mod, by = c("country"))) %>%
  map(. %>% left_join(iv_2005_mod, by = c("country")))

save(mod_joined, file = "C:/Users/dalla/Google Drive/offline_data_files/icvs_pwt_swiid/data/mod_joined.RData")

mod_joined1 <- mod_joined[[1]]
# save(mod_joined1, file = "C:/Users/dalla/Google Drive/offline_data_files/icvs_pwt_swiid/data/mod_joined1.RData")

rm(mod_joined,swiid_mod)

cgwtools::resave(mod_joined1, file = "C:/Users/dalla/Google Drive/offline_data_files/icvs_pwt_swiid/data/icvs_joined_samples.RData")

rm(mod_joined1)

lib_joined <- swiid_lib %>%
  map(. %>% left_join(pwt_lib, by = c("country"))) %>%
  map(. %>% left_join(iv_2005_lib, by = c("country")))

save(lib_joined, file = "C:/Users/dalla/Google Drive/offline_data_files/icvs_pwt_swiid/data/lib_joined.RData")

lib_joined1 <- lib_joined[[1]]

rm(lib_joined,swiid_lib)

cgwtools::resave(lib_joined1, file = "C:/Users/dalla/Google Drive/offline_data_files/icvs_pwt_swiid/data/icvs_joined_samples.RData")

rm(lib_joined1)

lib1_joined <- swiid_lib1 %>%
  map(. %>% left_join(pwt_lib1, by = c("country"))) %>%
  map(. %>% left_join(iv_2005_lib1, by = c("country")))

save(lib1_joined, file = "C:/Users/dalla/Google Drive/offline_data_files/icvs_pwt_swiid/data/lib1_joined.RData")

lib1_joined1 <- lib1_joined[[1]]

rm(lib1_joined, swiid_lib1)

cgwtools::resave(lib1_joined1, file = "C:/Users/dalla/Google Drive/offline_data_files/icvs_pwt_swiid/data/icvs_joined_samples.RData")

rm(lib1_joined1)



gc()
# rm(list = ls())
