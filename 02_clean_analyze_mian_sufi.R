#Get mian and sufi to the CBSA level:
library(readxl)
library(tidycensus)
library(tidyverse)
library(haven)


#Read data:
m <- read_dta("data/raw/miansufieconometrica_countylevel.dta")
x <- read_excel("data/raw/list1.xls", skip=2)

#Process xwalk:
xwalk <- x %>%
  janitor::clean_names() %>%
  mutate(fips = str_c(fips_state_code, fips_county_code)) %>%
  select(fips, cbsa_code, cbsa_title, metropolitan_micropolitan_statistical_area)

#Process mian sufi:
ms <- m %>%
  mutate(fips = str_pad(fips, width = 5, side = "left", pad = "0")) %>%
  select(fips, netwp_h)

write.csv(ms, "data/processed/ms.csv")


#Get 2007-ish housing count by county via tidycensus:

#SET CENSUS API KEY: https://walker-data.com/tidycensus/reference/census_api_key.html
#census_api_key()
housing_county_07 <- get_acs(geography = "county", 
                         variables = "B25008_002",
                         year = 2009, 
                         survey = "acs5")
housing_county_07 <- housing_county_07 %>%
  select(fips = GEOID, count_housing = estimate)

#Get names of cities:
rent_home_prices <- read.csv("data/processed/rent_home_prices.csv")
cities <- unique(rent_home_prices$geography)
cbsa_codes_18_cities <- c("31080",  #Los Angeles
                          "16980", #Chicago
                          "38060", #Phoenix
                          "41740", #San Diego
                          "35620",  #New York
                          "19100", #Dallas
                          "19820", #Detroit
                          "42660", #Seattle
                          "33100", #Miami 
                          "17460", #Cleveland
                          "33460", #Minneapolis
                          "41180", #St. Louis
                          "45300", #Tampa 
                          "47900", #Washington DC. 
                          "17460", #Cleveland
                          "12060", #Atlanta
                          "14460", #Boston 
                          "38900", #Portland
                          "19740",  #Denver
                          "41860" #San Francisco
                          )

#Get the weighted sum of mian and sufi counties where weight is number of 
#owner-occupied housing units from acs 2005- 2009 
ms_weighted <- ms %>%
  left_join(housing_county_07) %>% #Join housing data
  left_join(xwalk) %>% #Join crosswalk
  #Use only mian and sufi data for the 18 cities
  filter(cbsa_code %in% cbsa_codes_18_cities, 
         !is.na(netwp_h)) %>% #And for where it exists
  
  #get weighted sum of netwp_h where weight is number of housing units
  group_by(cbsa_code, cbsa_title) %>%
  summarize(netwp_h_weighted_sum = sum(netwp_h * count_housing) / sum(count_housing)) %>%
  
  #Get geography column for future join
  mutate(geography = sub("-.*", "", cbsa_title), 
         geography = ifelse(geography == "St. Louis, MO", 
                            "St. Louis",
                            geography), 
         geography = ifelse(geography == "Washington",
                            "Washington DC",
                            geography)) %>%
  select(-cbsa_title)

write.csv(ms_weighted, "data/processed/ms_weighted.csv")

rent_home_ms <- left_join(rent_home_prices, ms_weighted)

#######

#Base Regression Set-Up:
base_reg <- rent_home_ms %>%
  filter(geography != "20 City" & geography != "USA") %>%
  filter(year == 2006) %>%
  mutate(home_rent_div = cs_home/real_rent, 
         home_rent_reindex_div = cs_home_reindexed / real_rent_reindexed)

base_model <- lm(netwp_h_weighted_sum ~ home_rent_div, data = base_reg)
reindex_model <- lm(netwp_h_weighted_sum ~ home_rent_reindex_div, data = base_reg)


#Results using 2006 to predict Mian and Sufi 2007 - 2009 drop 
summary(base_model)

summary(reindex_model)
stargazer::stargazer(reindex_model, type="html")
