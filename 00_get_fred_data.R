#Download Data from FRED:

library(tidyverse)
library(janitor)
library(fredr)
library(haven)
library(ipumsr)
library(tidycensus)

#Access FRED API: https://fred.stlouisfed.org/docs/api/api_key.html
Sys.setenv("FRED_API_KEY" = api_key)
api_key <- Sys.getenv("FRED_API_KEY")


#Funtion to get FRED Housing data for full usa, 20 city composite, 
# and 20 cities individually: 
get_fred_housing <- function(series_id_code){
  #Function to get FRED housing data in a standard way
  #Input: "series_id_code" (string) - the FRED code 
  #Output: FRED data in a dataframe
  
  print(series_id_code)

  fred_data <- fredr(
    series_id = series_id_code,
    observation_start = as.Date("1987-01-01"),
    observation_end = as.Date("2022-02-01"), 
    frequency = "a"
  )
  
  
  return(fred_data)
}

#Full usa:
housing_usa_cs <- get_fred_housing("CSUSHPINSA")

#20 city composite:
housing_20_city_cs <- get_fred_housing("SPCS20RSA")


#20 cities individually
fred_codes_housing <- c("SFXRSA", #San Francisco 
                        "LXXRSA", #Los Angeles    - potentially (CMSA through Jan 2018)
                        "CHXRSA", #Chicago
                        "NYXRSA", #New York
                        "SDXRSA", #San Diego  - potentially (only by year through 2021)
                        "SEXRSA", #Seattle
                        "PHXRSA", #Phoenix    - potentially (only 2002 - 2021 and by year)
                        "MIXRSA", #Miami 
                        "DNXRSA", #Denver     - potentially (only by year through 2021)
                        "BOXRSA", #Boston 
                        "DAXRSA", #Dallas
                        "ATXRSA", #Atlanta
                        "POXRSA", #Portland   - potentially (only by year, discontinued in 2017)
                        "WDXRSA", #Washington DC - potentially (discontinued 2017)
                        "TPXRSA", #Tampa      - potentially (only year)
                        #"LVXRSA", #Las Vegas   - NOPE
                        "MNXRSA", #Minneapolis - by year
                        #"CRXRSA", #Charlotte   - NOPE
                        "DEXRSA", #Detroit
                        "CEXRSA"  #Cleveland   -yes discontinued in 2017 
                        ) 
#Cities here are: San Francisco, Los Angeles, Chicago, New York, 
#                 San Diego, Seattle, Phoenix, Miami,
#                 Denver, Boston, Dallas, Atlanta, 
#                 Portland, Washington DC, Tampa, Las Vegas
#                 Minnesota, Charlotte, Detroit, Cleveland

housing_city_cs <- lapply(fred_codes_housing, get_fred_housing)
housing_city_cs <- bind_rows(housing_city_cs)


#Get Rental Data: Rent of Primary residence: 

#CBSA RENT CODES:
#cbsa_rent_codes <- lapply(cbsa_home_codes, convert_home_to_rent_code)
cbsa_rent_codes <- c("CUURA101SEHA",  #New York 
                     "CUURA319SEHA", #Atlanta
                     "CUURA422SEHA", #San Francisco 
                     "CUURA103SEHA", #Boston
                     "CUURA208SEHA", #Detroit
                     "CUURA423SEHA", #Seattle
                     "CUURA320SEHA", #Miami 
                     "CUURA207SEHA", #Chicago 
                     "CUURA316SEHA",  #Dallas-Ft Worth
                     
                     "CUURA421SEHA", #Los Angeles
                     
                     "CUUSA424SEHA", #San Diego -a
                     "CUUSA429SEHA", #Phoenix -a
                     "CUUSA433SEHA", #Denver - a
                     "CUUSA425SEHA", #Portland -a 
                     "CUURA311SEHA", #Washington DC
                     "CUUSA321SEHA", #Tampa -a
                     "CUUSA211SEHC01", #Minneapolis - a
                     "CUUSA210SEHA" #Cleveland- # - a
)  
rent_city_bls <- lapply(cbsa_rent_codes, get_fred_housing)

rent_city_bls <- bind_rows(rent_city_bls)

rent_all_urban_bls <- get_fred_housing("CUSR0000SEHA")


##Clean the acquired data:
rent <- bind_rows(rent_city_bls, rent_all_urban_bls) %>%
  rename(real_rent= value) %>%
  mutate(
    data = "real_rent", 
    year = str_sub(date, 1, 4), 
    geography  = case_when( 
               str_detect(series_id, "CUURA101") ~ "New York",
               str_detect(series_id, "CUURA319") ~ "Atlanta",
               str_detect(series_id, "CUURA422") ~ "San Francisco",
               str_detect(series_id, "CUURA103") ~ "Boston",
               str_detect(series_id, "CUURA208") ~ "Detroit",
               str_detect(series_id, "CUURA423") ~ "Seattle",
               str_detect(series_id, "CUURA320") ~ "Miami",
               str_detect(series_id, "CUURA207") ~ "Chicago",
               str_detect(series_id, "CUURA316") ~ "Dallas",
               str_detect(series_id, "CUURA421") ~ "Los Angeles", 
               str_detect(series_id, "CUUSA424") ~ "San Diego", 
               str_detect(series_id, "CUUSA429") ~ "Phoenix", 
               str_detect(series_id, "CUUSA433") ~ "Denver", 
               str_detect(series_id, "CUUSA425") ~ "Portland", 
               str_detect(series_id, "CUURA311") ~ "Washington DC", 
               str_detect(series_id, "CUUSA211") ~ "Minneapolis", 
               str_detect(series_id, "CUUSA210") ~ "Cleveland",
               str_detect(series_id, "CUUSA321") ~ "Tampa", 
               str_detect(series_id, "CUSR0000") ~ "USA")) %>%
  select(year, geography, real_rent) %>%
  mutate(geog_join = geography)
  

home <- bind_rows(housing_20_city_cs, housing_city_cs, housing_usa_cs) %>%
  select(-c(realtime_start, realtime_end)) %>%
  rename(cs_home = value) %>%
  mutate(
    year = str_sub(date, 1, 4), 
    geography_to_keep = case_when(
      str_detect(series_id, "NYXRSA") ~ "New York",
      str_detect(series_id, "ATXRSA") ~ "Atlanta",
      str_detect(series_id, "SFXRSA") ~ "San Francisco",
      str_detect(series_id, "BOXRSA") ~ "Boston",
      str_detect(series_id, "DEXRSA") ~ "Detroit",
      str_detect(series_id, "SEXRSA") ~ "Seattle",
      str_detect(series_id, "MIXRSA") ~ "Miami",
      str_detect(series_id, "CHXRSA") ~ "Chicago",
      str_detect(series_id, "DAXRSA") ~ "Dallas",
      str_detect(series_id, "LXXRSA") ~ "Los Angeles", 
      str_detect(series_id, "SDXRSA") ~ "San Diego", 
      str_detect(series_id, "PHXRSA") ~ "Phoenix", 
      str_detect(series_id, "DNXRSA") ~ "Denver", 
      str_detect(series_id, "POXRSA") ~ "Portland", 
      str_detect(series_id, "WDXRSA") ~ "Washington DC", 
      str_detect(series_id, "MNXRSA") ~ "Minneapolis", 
      str_detect(series_id, "CUUSA210") ~ "Cleveland",
      str_detect(series_id, "TPXRSA") ~ "Tampa", 
      str_detect(series_id, "CEXRSA") ~ "Cleveland", 
      str_detect(series_id, "SPCS20RSA") ~ "20 City",
      str_detect(series_id, "CSUSHPINSA") ~ "USA")) %>%
  select(year, geography_to_keep, cs_home) %>%
  mutate(geog_join = ifelse(geography_to_keep == "20 City",
                            "USA",
                            geography_to_keep))
     

  
full <- left_join(home, rent, by = c("year", "geog_join")) %>%
  select(year, geography = geography_to_keep, cs_home, real_rent)

#Goal now is to reindex both cs_home and real_rent to the first year in 
# which they both exist in the dataset:
year_first_both <- full %>%
  mutate(both_have_data = ifelse(!is.na(cs_home) & !is.na(real_rent), TRUE, FALSE)) %>%
  filter(both_have_data ==TRUE) %>%
  group_by(geography) %>%
  slice_min(order_by = year) %>%
  select(geography, year_first_both = year)

full <- full %>%
  left_join(year_first_both)

reindex_value <- full %>%
  filter(year == year_first_both) %>%
  select(geography, cs_home_reindex_val = cs_home, real_rent_reindex_val = real_rent)

full <- full %>%
  left_join(reindex_value) %>%
  mutate(cs_home_reindexed = cs_home/cs_home_reindex_val, 
         real_rent_reindexed = real_rent/real_rent_reindex_val) %>%
  select(year, geography, real_rent, cs_home, real_rent_reindexed, cs_home_reindexed)




#Description:
# The table above join two sets of disparate datasets.
#cs_home is the Case Shiller Home Price Index for the MSA or CBSA or the 
#city listed listed. For every
# one of the 18 cities, the Case Shiller Index refers specifically to that city. 
# The "20 City" refers to the 20 City Case Shiller Index. The real-rent data
# comes from the BLS real rent index. The all USA Case Shiller AND the 20 city
# case Shiller are paired with the BLS data for all Urban consumers (neither 
# match is perfect, but both offer reasonable comparisons. )

#The reindexed values are the initial values acquired divided by the first year
# in which the data has values both for the real rent data from BLS and for the
#Case Shiller index. 


write_csv(full, "data/processed/rent_home_prices.csv")

