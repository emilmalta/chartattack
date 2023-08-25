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
    school = fct_reorder(school, `Number of pupils`, sum, .na_rm = TRUE, .desc = TRUE),
  )

# Visualise --------------------------------------------------------------------

udxtds %>% 
  ggplot(aes(
    x = fct_rev(grade), y = `Problem-solving proficiency (pct. correct)`,
    size = `Number of pupils`, color = subject, group = grade,
    alpha = time
  )) +
  geom_sina(maxwidth = .5) +
  coord_flip() +
  facet_wrap(~school, ncol = 1) +
  scale_size(range = c(.1, 7)) +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) + 
  scale_color_statgl(reverse = TRUE) +
  labs(
    title = "Grade test results", subtitle = "Nuuk City district", x = NULL
  ) + 
  theme_minimal() +
  theme(strip.text = element_text(hjust = 0),
    panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank()
  )
