#Download Data from FRED:

library(tidyverse)
library(janitor)
library(fredr)

#Access FRED API: https://fred.stlouisfed.org/docs/api/api_key.html
#Sys.setenv("FRED_API_KEY" = your_fred_api_key)
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
    frequency = "m"
  )
  
  
  return(fred_data)
}

#Full usa:
housing_usa <- get_fred_housing("CSUSHPINSA")

#20 city composite:
housing_20_city <- get_fred_housing("SPCS20RSA")


#20 cities individually
fred_codes_housing <- c("SFXRSA", "LXXRSA", "CHXRSA", "NYXRSA",
                        "SDXRSA", "SEXRSA", "PHXRSA", "MIXRSA", 
                        "DNXRSA", "BOXRSA", "DAXRSA", "ATXRSA", 
                        "POXRSA", "WDXRSA", "TPXRSA", "LVXRSA", 
                        "MNXRSA", "CRXRSA", "DEXRSA", "CEXRSA")
#Cities here are: San Francisco, Los Angeles, Chicago, New York, 
#                 San Diego, Seattle, Phoenix, Miami,
#                 Denver, Boston, Dallas, Atlanta, 
#                 Portland, Washington DC, Tampa, Las Vegas
#                 Minnesota, Charlotte, Detroit, Cleveland

city_data <- lapply(fred_codes_housing, get_fred_housing)
housing_by_city <- bind_rows(city_data)


#Get Rental Data: CPI Owner Equivalent Rent of Primary Residence:

#GET CODES: 
#City:
cbsa_home_codes <- c("CUURA101SEHC01", 
                     "CUURA319SEHC01",
                     "CUURA422SEHC01",
                     "CUURA103SEHC01",
                     "CUURA208SEHC01",
                     "CUURA423SEHC01",
                     "CUURA320SEHC01",
                     "CUURA207SEHC01",
                     "CUURA316SEHC01", 
                     "CUURA102SEHC01",
                     "CUURA318SEHC01")

region_home_codes <- c("CUUR0300SEHC01", 
                       "CUUR0100SEHC01", 
                       "CUUR0200SEHC01", 
                       "CUUR0400SEHC01")

us_city_home_average_code <- "CUSR0000SEHC"




convert_home_to_rent_code <- function(rent_code){
  #Function to make use of the fact that for region and cbsa, 
  #the code is the same for home equivalent rent of primary residence
  # and rent of primary residence for each geography except for
  # home, the last digits are: SEHC01 and for rent they are SEHA
  home_code <- str_c(substring(rent_code, 1, nchar(rent_code) - 3), "A")
}

cbsa_rent_codes <- lapply(cbsa_home_codes, convert_home_to_rent_code)
region_rent_codes <- lapply(region_home_codes, convert_home_to_rent_code)
us_city_rent_average_code <- "CUSR0000SEHA"

#GET DATA FROM CODES: 
cbsa_home <- lapply(cbsa_home_codes, get_fred_housing)
region_home <- lapply(region_home_codes, get_fred_housing)
us_city_home_average <- lapply(us_city_home_average_code, get_fred_housing)
cbsa_rent <- lapply(cbsa_rent_codes, get_fred_housing)
region_rent <- lapply(region_rent_codes, get_fred_housing)
us_city_rent_average <- lapply(us_city_rent_average_code, get_fred_housing)


all_data <- bind_rows(cbsa_home, region_home, us_city_home_average, cbsa_rent, 
                      region_rent, us_city_rent_average) %>%
  mutate(type = ifelse(str_detect(series_id, "SEHC"), "owner_equiv_rent_pr", "rent_pr"), 
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
           str_detect(series_id, "CUURA102") ~ "Philadelphia", 
           str_detect(series_id, "CUURA318") ~ "Houston",
           str_detect(series_id, "CUUR0300") ~ "South", 
           str_detect(series_id, "CUUR0100") ~ "Northeast", 
           str_detect(series_id, "CUUR0200") ~ "Midwest", 
           str_detect(series_id, "CUUR0400") ~ "West",
           str_detect(series_id, "CUSR0000") ~ "USA")
         ) %>%
  separate(date, sep ="-", into = c("year", "month", "day")) %>%
  select(year, month, geography, type, value)
    

         