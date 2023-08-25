# Load libraries ---------------------------------------------------------------

library(tidyverse)
library(countrycode)
library(shadowtext)library(ggrepel)
library(WDI)

# Import ddata -----------------------------------------------------------------

gap_raw <- c("SP.DYN.LE00.IN", "NY.GDP.PCAP.CD", "SP.POP.TOTL") %>% 
  map(WDI, country = "all") %>% 
  reduce(left_join)

# Tidy/transform ---------------------------------------------------------------

gap <- gap_raw %>% 
  as_tibble() %>%
  mutate(continent = countrycode(iso3c, "iso3c", "continent")) %>% 
  suppressWarnings() %>% 
  drop_na(continent)

gap %>% 
  filter(year <= 2019) %>% 
  arrange(country, year) %>% 
  ggplot(aes(x = NY.GDP.PCAP.CD, y = SP.DYN.LE00.IN)) +
  geom_path(aes(group = country, color = continent), alpha = .2) +
  geom_point(data = . %>% filter(year == 2019) %>% arrange(desc(SP.POP.TOTL)),
             aes(fill = continent, size = SP.POP.TOTL), pch = 21, color = "white") +
  geom_shadowtext(data = . %>% filter(year == 2019) %>% arrange(desc(SP.POP.TOTL)),
            aes(label = country,), size = 4, check_overlap = TRUE, bg.color = "white", color = "black", nudge_y = -.5, alpha = .75) +
  scale_x_log10(labels = scales::dollar) +
  scale_size_area(max_size = 30) +
  coord_cartesian(xlim = c(100, 200000), ylim = c(50, 85)) +
  theme_minimal() +
  theme(
    panel.grid.minor.x = element_blank(), legend.position = "none"
  ) 
  

