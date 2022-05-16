#Plot Change Over Time of Rent and Home Indices

library(tidyverse)

rh <- read_csv("data/processed/rent_home_prices.csv") #rh for rent and house


#Filter to appropriate Geography:
us_rh <- rh %>%
  filter(geography == "USA")

top_20 <- rh %>%
  filter(geography == "20 City")

cbsa_rh <- rh %>%
  filter(!(geography %in% c("20 City", "USA")))
         

ggplot(data = us_rh, aes(x = year)) + 
  geom_line(aes(y = cs_home_reindexed), color = "red") + 
  geom_line(aes(y = real_rent_reindexed), color = "blue") 


ggplot(data = top_20, aes(x = year)) + 
  geom_line(aes(y = cs_home_reindexed), color = "red") + 
  geom_line(aes(y = real_rent_reindexed), color = "blue") 

ggplot(data = cbsa_rh, aes(x = year)) + 
  geom_line(aes(y = cs_home_reindexed), color = "blue") + 
  geom_line(aes(y = real_rent_reindexed), color = "red") + 
  facet_wrap(geography ~ .)

#Thoughts on the charts:
#1 slightly misleading comparing central city to sub-urban area
#One would expect central cities would be more expensive than the surrounding metro areas
#BUT this charts show data normalized to the same start year, so they tend to 
#suggest regions are seeing their urban housing increase more quickly than rent
# in the equivalent metropolitan area

