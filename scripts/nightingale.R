# Load libraries ---------------------------------------------------------------

library(tidyverse)
library(HistData)

# Import data ------------------------------------------------------------------

data("Nightingale", package = "HistData")

# Tidy/transform ---------------------------------------------------------------

nightingale <- Nightingale %>% 
  select(Date, Month, ends_with("rate")) %>% 
  pivot_longer(ends_with("rate")) %>% 
  mutate(period = ifelse(Date < "1855-04-01", "Before", "After"))

# Visualise --------------------------------------------------------------------

nightingale %>%
  arrange(Date, desc(value)) %>% 
  ggplot(aes(Month, value, fill = name)) +
  geom_col(width = 1, color = "grey", position = "identity", linewidth = .1) +
  scale_y_sqrt() +
  facet_wrap(~period) +
  coord_polar(start = (2 * pi) / 12 * 6) 
