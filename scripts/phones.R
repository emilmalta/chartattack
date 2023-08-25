# Load libraries ---------------------------------------------------------------

library(tidyverse)
library(countrycode)
library(gganimate)
library(WDI)

# Import data ------------------------------------------------------------------

phones_raw <- c("IT.CEL.SETS.P2", "IT.MLT.MAIN.P2", "SP.POP.TOTL") %>% 
  map(WDI, country = "all") %>% 
  reduce(left_join)

# Tidy/transform ---------------------------------------------------------------

phones <- phones_raw %>% 
  mutate(continent = countrycode(iso3c, "iso3c", "continent")) %>% 
  suppressWarnings() %>%
  drop_na(continent) %>% 
  arrange(country, year) %>% 
  group_by(country) %>% 
  fill(IT.CEL.SETS.P2, IT.MLT.MAIN.P2) %>% 
  ungroup()

# Visualise --------------------------------------------------------------------

phones %>% 
  filter(year >= 1982, year <= 2020) %>% 
  arrange(desc(SP.POP.TOTL)) %>% 
  ggplot(aes(x = IT.MLT.MAIN.P2, IT.CEL.SETS.P2)) +
  geom_point(
    aes(size = SP.POP.TOTL, fill = continent, group = 1), pch = 21, color = "white") +
  scale_size_area(max_size = 50) +
  scale_x_continuous(limits = c(0, 80)) +
  scale_y_continuous(limits = c(0, 200)) +
  guides(size = "none") +
  theme(legend.position = "bottom") +
  labs(color = NULL, fill = NULL,
       x = "Landline subscriptions\n(per 100 people)",
       y = "Cell phone subscriptions\n(per 100 people)"
  ) +
  labs(title = "{frame_time}") + 
  transition_time(year) +
  shadow_wake(.05)


