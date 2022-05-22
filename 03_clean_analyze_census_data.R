#Census Data For housing change over time:
library(tidycensus)
library(tidyverse)
library(fredr)

#SET CENSUS API KEY: https://walker-data.com/tidycensus/reference/census_api_key.html
#census_api_key()



v17 <- load_variables(2015, "acs1", cache = TRUE)

#Median housing valueB25077_001
years = c(2005:2019)
acs <- map_dfr(years, ~ get_acs(
                          geography = "county",
                          variables = c("B25077_001", "B25064_001", "B01001_001"),
                          year = .x,
                          survey = "acs1"),
               .id = "year"
               ) 

acs_clean <- acs %>%
  mutate(year = as.numeric(year) + 2004) %>%
  select(-moe)%>%
  pivot_wider(names_from = variable, values_from= estimate) %>%
  rename(median_gross_rent = "B25064_001", 
         median_val_housing_units = "B25077_001", 
         total_pop = "B01001_001")
  #rename(est_housing_value = estimate) %>%
  

#GET INFLATION SINCE 2005:
inflation  <- fredr(
  series_id = "FPCPITOTLZGUSA",
  observation_start = as.Date("2005-01-01"),
  observation_end = as.Date("2019-01-01"), 
  frequency = "a"
)

#Get inflation value
inf <- pull(inflation, value)

#Convert to 1.0X format
inf = 1 + inf/100

#Set up and run loop to calculate inf since 05
inf[1] = 1
inf_since_05 <- rep(1, length(inf))

prev_year_inf = 1
for (i in (1:length(inf) )) {
  current_year_inf = inf[i]
  tot_inf_since_05 = prev_year_inf * current_year_inf
  inf_since_05[i] = tot_inf_since_05
  prev_year_inf = tot_inf_since_05 
}

year <- seq(2005:2019) + 2004
inf_since_05 <- data.frame(year, inf_since_05)

acs_clean_w_i <- left_join(acs_clean, inf_since_05) %>%
  mutate(median_gross_rent_05 = median_gross_rent / inf_since_05, 
         median_val_housing_units_05 = median_val_housing_units / inf_since_05) %>%
  select(year, fips = GEOID, name = NAME, median_gross_rent_05, median_val_housing_units_05, total_pop)

vals_05 <- acs_clean_w_i %>%
  filter(year == 2005) %>%
  select(fips, median_gross_rent_05_at_05 = median_gross_rent_05, 
         median_val_housing_units_05_at_05 = median_val_housing_units_05, 
         total_pop_05 = total_pop)

acs_clean_w_i_at_05 <- left_join(acs_clean_w_i, vals_05) %>%
  mutate(rent_index = median_gross_rent_05 / median_gross_rent_05_at_05 * 100, 
         housing_index = median_val_housing_units_05 /
           median_val_housing_units_05_at_05 * 100) %>%
  select(year, fips, name, rent_index, housing_index, total_pop_05)

a <- acs_clean_w_i_at_05

#Get names of top 18 cities:
rh <- read_csv("data/processed/rent_home_prices.csv")

cities <- unique(rh$geography)

cities <- cities[-1]
cities <- cities[-19]
names <- c("Fulton County, Georgia", 
           "Suffolk County, Massachusetts", 
           "Cook County, Illinois", 
           "Cuyahoga County, Ohio",
           "Dallas County, Texas", 
           "Denver County, Colorado", 
           "Wayne County, Michigan", 
           "Los Angeles County, California",
           "Miami-Dade County, Florida", 
           "Hennepin County, Minnesota", 
           "Bronx County, New York", 
           "New York County, New York", 
           "Suffolk County, New York", 
           "Nassau County, New York", 
           "Queens County, New York", 
           "Kings County, New York", 
           "Maricopa County, Arizona", 
           "Multnomah County, Oregon",
           "San Diego County, California",
           "San Francisco County, California", 
           "King County, Washington", 
           "Hillsborough County, Florida", 
           "District of Columbia, District of Columbia"
)
big_counties <- a %>%
  filter(name %in% names) %>%
  mutate(city = case_when(
    name == "Fulton County, Georgia" ~ "Atlanta", 
    name ==       "Suffolk County, Massachusetts" ~ "Boston", 
    name ==       "Cook County, Illinois" ~ "Chicago", 
    name ==       "Cuyahoga County, Ohio" ~"Cleveland",
    name ==      "Dallas County, Texas" ~ "Dallas", 
    name ==       "Denver County, Colorado" ~ "Denver", 
    name ==      "Wayne County, Michigan" ~"Detroit", 
    name ==      "Los Angeles County, California" ~"Los Angeles",
    name ==     "Miami-Dade County, Florida" ~"Miami", 
    name ==     "Hennepin County, Minnesota" ~ "Minneapolis", 
    name ==    "Bronx County, New York" ~ "New York", 
    name ==    "New York County, New York" ~ "New York", 
    name ==    "Suffolk County, New York" ~ "New York", 
    name ==    "Nassau County, New York" ~ "New York", 
    name ==    "Queens County, New York" ~ "New York", 
    name ==   "Kings County, New York" ~ "New York", 
    name ==   "Maricopa County, Arizona" ~ "Phoenix", 
    name ==   "Multnomah County, Oregon" ~ "Portland",
    name ==   "San Diego County, California" ~ "San Diego",
    name ==   "San Francisco County, California" ~"San Francisco", 
    name ==    "King County, Washington" ~"Seattle", 
    name ==    "Hillsborough County, Florida" ~"Tampa", 
    name ==    "District of Columbia, District of Columbia" ~"Washington DC"
  )) %>%
  pivot_longer(!c("year", "fips", "name", "city", "total_pop_05"),
               names_to = "indices", values_to = "value") %>%
  separate(name, sep = ",", c("county", "state")) %>%
  mutate(city_county = str_c(city, " (", county, ")"))



ggplot(data = big_counties, aes(x = year)) + 
  geom_line(aes(y = value, color = indices)) +  
  facet_wrap(city_county ~ .)

  
  