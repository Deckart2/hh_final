#Plot Change Over Time of Rent and Home Indices

library(tidyverse)
library(gtable)
library(lemon)
library(ggthemes)

rh <- read_csv("data/processed/rent_home_prices.csv") #rh for rent and house


#Filter to appropriate Geography:
us_rh <- rh %>%
  filter(geography == "USA") %>%
  pivot_longer(!c("year", "geography"), names_to = "index", values_to = "value") %>%
  mutate(value = value * 100) %>%
  mutate(Index = case_when(
    index == "cs_home" ~ "Case-Shiller Housing Index", 
    index == "cs_home_reindexed" ~ "Case-Shiller Housing Index ", 
    index == "real_rent" ~ "BLS Real Rent of Primary Residence", 
    index == "real_rent_reindexed" ~ "BLS Real Rent of Primary Residence")) %>%
  filter(index == "cs_home_reindexed" | index == "real_rent_reindexed")

top_20 <- rh %>%
  filter(geography == "20 City")

cbsa_rh <- rh %>%
  filter(!(geography %in% c("20 City", "USA"))) %>%
  pivot_longer(!c("year", "geography"), names_to = "index", values_to = "value") %>%
  mutate(value = value * 100) %>%
  mutate(Index = case_when(
    index == "cs_home" ~ "Case-Shiller Housing Index", 
    index == "cs_home_reindexed" ~ "Case-Shiller Housing Index ", 
    index == "real_rent" ~ "BLS Real Rent of Primary Residence", 
    index == "real_rent_reindexed" ~ "BLS Real Rent (Primary Residence)"))
         
cbsa_rh_index <- cbsa_rh %>%
  filter(index %in% c("cs_home_reindexed", "real_rent_reindexed"))

cbsa_rh_no_index <- cbsa_rh %>%
  filter(!(index %in% c("cs_home_reindexed", "real_rent_reindexed")))

ggplot(data = us_rh, aes(x = year, y = value, color = Index)) + 
  geom_line() +
  labs(x = "Year",
       y = "Housing/Rent Index", 
       color = "Legend",
       title = "Housing prices have outpaced rent since 2011", 
       caption = "Index = 100 in 1987. Data From FRED. Rent for U.S. City Average. Housing Index for all of the US.") + 
  scale_y_continuous(limits = c(0, 400), breaks = scales::pretty_breaks(n = 5)) + 
  theme(axis.text.y = element_text(angle = 0), 
        panel.grid.major = element_line(colour = "lightgrey", size = .15), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        strip.text.x = element_text(size = 11),
        strip.background =element_rect(fill="#EAEAEA"),
        legend.key = element_rect(colour = NA, fill = NA),
        legend.title = element_text(size=12), #change legend title font size
        legend.text = element_text(size=10),
        plot.title = element_text(size=16), 
        plot.caption =  element_text(size = 9, hjust = 0),
        legend.position = c(0.8, 0.2)
  )




shift_legend2 <- function(p) {
  #CODE COPIED FROM: https://stackoverflow.com/questions/54438495/shift-legend-into-empty-facets-of-a-faceted-plot-in-ggplot2
  # ...
  # to grob
  gp <- ggplotGrob(p)
  facet.panels <- grep("^panel", gp[["layout"]][["name"]])
  empty.facet.panels <- sapply(facet.panels, function(i) "zeroGrob" %in% class(gp[["grobs"]][[i]]))
  empty.facet.panels <- facet.panels[empty.facet.panels]
  
  # establish name of empty panels
  empty.facet.panels <- gp[["layout"]][empty.facet.panels, ]
  names <- empty.facet.panels$name
  # example of names:
  #[1] "panel-3-2" "panel-3-3"
  
  # now we just need a simple call to reposition the legend
  reposition_legend(p, 'center', panel=names)
}

p <-ggplot(data = cbsa_rh_index) + 
  geom_line(aes(x = year, y = value, color = Index)) +
  facet_wrap(geography ~ ., scales='free') + 
  labs(x = "Year",
       y = "Housing/Rent Index", 
       color = "Legend",
       title = "Heterogeneous Growth in Housing and Rent Prices by City", 
       subtitle = "Housing price increases in many western cities continue to outpace increases in real rent.", 
       caption = "Index = 100 in the first year of data for both cities. Data From FRED. Rent for U.S. City Average. Housing Index for all of the US.") + 
  scale_y_continuous(limits = c(0, 550), breaks = scales::pretty_breaks(n = 5)) + 
  theme(axis.text.y = element_text(angle = 0), 
        panel.grid.major = element_line(colour = "lightgrey", size = .15), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        strip.text.x = element_text(size = 11),
        strip.background =element_rect(fill="#EAEAEA"),
        legend.key = element_rect(colour = NA, fill = NA),
        legend.title = element_text(size=15), #change legend title font size
        legend.text = element_text(size=12.5),
        plot.title = element_text(size=20), 
        plot.caption =  element_text(size = 9, hjust = 0)
        )
p <- shift_legend2(p)




#Thoughts on the charts:
#1 slightly misleading comparing central city to sub-urban area
#One would expect central cities would be more expensive than the surrounding metro areas
#BUT this charts show data normalized to the same start year, so they tend to 
#suggest regions are seeing their urban housing increase more quickly than rent
# in the equivalent metropolitan area

