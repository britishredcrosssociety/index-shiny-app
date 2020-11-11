##
## Save local copies of any dataframes we'll need to load in the app
##
library(tidyverse)
library(arrow)

ri <- read_csv("https://github.com/britishredcrosssociety/resilience-index/raw/main/data/processed/resilience%20index.csv")

# write_csv(ri, "data/resilience-index.csv")
# write_feather(ri, "data/resilience-index.feather")

# Test - arrow is ~9x faster
# bench::mark(
#   read_feather("data/resilience-index.feather"),
#   read_csv("data/resilience-index.csv")
# )

# ---- Bivariate Scores ----
# create 3 buckets for vulnerability and resilience to map to colours
# Create 3 buckets: 1-4, 5-8, 9-10
# decile_buckets <- c(1, 4, 8, 10)
quintile_buckets <- c(1, 2, 4, 5)
vul_buckets <- quintile_buckets
res_buckets <- quintile_buckets

# create color scale that encodes two variables
# red for vulnerability and blue for resilience
bivariate_color_scale <- tibble(
  "3 - 3" = "#3F2949", 
  "2 - 3" = "#435786",
  "1 - 3" = "#4885C1",
  "3 - 2" = "#77324C",
  "2 - 2" = "#806A8A",
  "1 - 2" = "#89A1C8",
  "3 - 1" = "#AE3A4E",
  "2 - 1" = "#BC7C8F",
  "1 - 1" = "#CABED0"
) %>%
  gather("group", "fill")

# Cut vulnerability and resilience scores into groups defined by buckets
# Join colour scale
ri_bivariate <-
  ri %>% 
  mutate(vul_cut = cut(`Vulnerability quintile`,
                       breaks = vul_buckets,
                       include.lowest = TRUE, 
                       right = TRUE),
         res_cut = cut(`Capacity quintile`,
                       breaks = res_buckets,
                       include.lowest = TRUE, 
                       right = TRUE),
         group = paste(as.numeric(vul_cut),
                       "-",
                       as.numeric(res_cut))) %>%
  left_join(bivariate_color_scale, by = "group")

# unique(ri_bivariate$vul_cut)
# unique(ri_bivariate$res_cut)

# - Create legend -
bivariate_color_scale <-
  bivariate_color_scale %>% 
  separate(group, into = c("vul", "res"), sep = " - ") %>%
  mutate(vul = as.integer(vul),
         res = as.integer(res))

# Legend
ggplot() +
  geom_tile(
    data = bivariate_color_scale,
    mapping = aes(
      x = vul,
      y = res,
      fill = fill),
    show.legend = FALSE
  ) +
  scale_fill_identity() +
  theme_void() +
  coord_fixed()

ggsave(filename = "www/bivar-legend-void.jpg",
       bg = "transparent",
       width = 5.21,
       height = 5.21)

# ---- Prep RI ----
write_feather(ri_bivariate, "data/resilience-index.feather")
