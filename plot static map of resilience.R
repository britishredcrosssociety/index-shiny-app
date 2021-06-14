library(tidyverse)
library(sf)
library(viridis)
library(patchwork)
library(geographr)
library(arrow)

ri <- read_feather("data/resilience-index.feather")

shp_la <- geographr::boundaries_lad %>%
  filter(str_detect(lad_code, "^E"))

# White theme
theme_map <- function(...) {
  theme_minimal() +
    theme(
      text = element_text(family = "FiraCode-Retina", color = "#22211d"),
      axis.line = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks = element_blank(),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      # panel.grid.minor = element_line(color = "#ebebe5", size = 0.2),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      plot.background = element_rect(fill = "#ffffff", color = NA),
      panel.background = element_rect(fill = "#ffffff", color = NA),
      legend.background = element_rect(fill = "#ffffff", color = NA),
      panel.border = element_blank(),
      # Add labs elements
      legend.title = element_text(size = 11),
      legend.text = element_text(size = 9, hjust = 0),
      plot.title = element_text(size = 15, hjust = 0.5),
      plot.subtitle = element_text(
        size = 10, hjust = 0.5,
        margin = margin(
          b = 0.2,
          t = 0.2,
          l = 2,
          unit = "cm"
        ),
        debug = F
      ),
      # captions
      plot.caption = element_text(
        size = 7,
        hjust = .5,
        margin = margin(
          t = 0.2,
          b = 0,
          unit = "cm"
        ),
        color = "#939184"
      ),
      ...
    )
}

shp_la %>% 
  left_join(ri %>% select(LAD19CD, group, fill), 
            by = c("lad_code" = "LAD19CD")) %>% 

  ggplot() +
  geom_sf(mapping = aes(fill = fill),
          color = "black",
          size = 0.1) +
  
  scale_fill_identity() +
  
  theme_map()
  # theme(plot.margin = unit(c(0.5, 1.5, 0.5, 1.5), "cm"))

ggsave(filename = "resilience-index.png",
       width = 150,
       height = 100,
       units = "mm")
