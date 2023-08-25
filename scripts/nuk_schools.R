# Load libraries ---------------------------------------------------------------

library(tidyverse)
library(ggforce)
library(statgl)

# Import data ------------------------------------------------------------------

udxtds_raw <- statgl_fetch("UDETDS")

# Tidy/transform ---------------------------------------------------------------

udxtds <- udxtds_raw %>% 
  mutate(time = as.numeric(time)) %>% 
  separate(
    `district / school`, into = c("district", "school"), sep = " - ",  
    extra = "merge", fill = "right"
  ) %>% 
  pivot_wider(names_from = "unit") %>% 
  filter(district == "NUK") %>% 
  mutate(
    school = fct_reorder(school, `Number of pupils`, sum, .na_rm = TRUE, .desc = TRUE)
  )

# Visualise --------------------------------------------------------------------

udxtds %>% 
  ggplot(aes(x = grade, y = `Problem-solving proficiency (pct. correct)`,
             size = `Number of pupils`, color = subject, group = grade,
             alpha = time)) +
  geom_sina(maxwidth = .5) +
  coord_flip() +
  facet_wrap(~school, ncol = 1) +
  scale_size(range = c(.1, 5)) +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) + 
  theme_minimal() +
  scale_color_statgl()
