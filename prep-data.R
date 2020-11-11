# ---- Load libraries ----
library(tidyverse)
library(sf)
library(httr)
library(readxl)
library(lubridate)

# ---- Load data ----
res_index <- read_csv("data/resilience-index-la.csv")
la_shp <- read_sf("data/vulnerability-LA.geojson")
pop <- read_csv("data/la-population.csv")
eth <- read_csv("data/la-ethnicity.csv")

# ---- Clean data ----
# Prep data for bivariate maps
# Vulnerability scores
la_vul <-
  la_shp %>% 
  as_tibble() %>% 
  select(LAD19CD = Code,
         Name,
         vulnerability = Vulnerability.quintile)

# Join resilience scores
la_vul_res <-
  la_vul %>% 
  left_join(res_index %>% as_tibble(),
            by = "LAD19CD") %>% 
  select(la_code = LAD19CD,
         la_name = Name,
         resilience = `Resilience decile`,
         vulnerability) %>% 
  filter(str_detect(la_code, "^E"))

# Create list of LA's
english_las <-
  la_shp %>% 
  as_tibble() %>% 
  select(la_code = Code, la_name = Name) %>% 
  filter(str_detect(la_code, "^E"))

# Ethnicity data
# From 2011 Census
eth %>%
  rename(la_code = LAD19CD,
         count = count_lad19) %>% 
  filter(str_detect(la_code, "^E")) %>% 
  filter(ethnicity == "Asian/Asian British" |
           ethnicity == "Black/African/Caribbean/Black British" |
           ethnicity == "Mixed/multiple ethnic groups" |
           ethnicity == "Other ethnic group" |
           ethnicity == "White") %>% 
  mutate(ethnicity = case_when(
    ethnicity == "Asian/Asian British" ~ "Asian",
    ethnicity == "Black/African/Caribbean/Black British" ~ "Black",
    ethnicity == "Mixed/multiple ethnic groups" ~ "Mixed",
    ethnicity == "Other ethnic group" ~ "Other",
    ethnicity == "White" ~ "White"
  )) %>% 
  group_by(la_code) %>% 
  arrange(count) %>% 
  ungroup() %>% 
  write_rds("data/la-ethnicity.rds")

# Save pop as rds to improve load time
# From ONS
pop %>% 
  select(la_code = LAD19CD,
         population = pop_lad19) %>% 
  filter(str_detect(la_code, "^E")) %>% 
  write_rds("data/la-population.rds")

# Weekly infection rates
# Fetch National COVID-19 surveillance data report from https://www.gov.uk/government/publications/national-covid-19-surveillance-reports
# This URL corresponds to 4 September 2020 (week 36):
GET("https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/914814/Weekly_COVID19_report_data_w36.xlsx",
    write_disk(tf <- tempfile(fileext = ".xlsx")))

covid <- read_excel(tf, sheet = "Figure 11. All weeks rates UTLA", skip = 7)

unlink(tf); rm(tf)

# Clean the data
covid <-
  covid %>% 
  select(la_code = `UTLA code`, starts_with("week")) %>% 
  filter(str_sub(la_code, 1, 1) == "E") %>% 
  mutate(across(starts_with("week"), as.numeric))

# Hackney and City of London boroughs are counted as one. Split into separate rows, using the same data for each
covid <- bind_rows(covid %>% filter(la_code != "E09000012/E09000001"),
                   covid %>% filter(la_code == "E09000012/E09000001") %>% mutate(la_code = "E09000012"),
                   covid %>% filter(la_code == "E09000012/E09000001") %>% mutate(la_code = "E09000001"))

# Join to england_las list to add NA for missing LA's
covid <-
  english_las %>% 
  left_join(covid,
            by = "la_code") %>% 
  select(-la_name)

# Pivot longer
covid <-
  covid %>%
  pivot_longer(cols = !la_code,
               names_to = "week",
               values_to = "cases_per_100000_week") %>% 
  filter(week != "week 05" &
           week != "week 06" &
           week != "week 07" &
           week != "week 08" &
           week != "week 09")

# Calculate LA mean
covid <-
  covid %>% 
  group_by(week) %>% 
  mutate(week_mean = mean(cases_per_100000_week, na.rm = TRUE)) %>% 
  ungroup()

covid %>% 
  write_rds("data/la-covid-stats.rds")

# ---- Shielding ----
# Coronavirus Shielded Patient List, England - Local Authority: https://digital.nhs.uk/data-and-information/publications/statistical/mi-english-coronavirus-covid-19-shielded-patient-list-summary-totals/latest
# This URL corresponds to data extracted from 27 August 2020:
shielding <- read_csv("https://files.digital.nhs.uk/44/1D4817/Coronavirus%20Shielded%20Patient%20List%2C%20England%20-%20Open%20Data%20with%20CMO%20DG%20-%20LA%20-%202020-08-27.csv")

shielding <-
  shielding %>% 
  mutate(`Extract Date` = dmy(`Extract Date`)) %>% 
  filter(`Extract Date` == max(`Extract Date`)) %>% 
  filter(`LA Code` != "ENG") %>%
  filter(`Breakdown Field` == "ALL") %>%
  select(la_code = `LA Code`,
         `Clinically extremely vulnerable` = `Patient Count`)

# Join to england_las list to add NA for missing LA's
shielding <- 
  english_las %>% 
  left_join(shielding,
            by = "la_code")

shielding %>% 
  write_rds("data/la-shielding.rds")

# ---- Bivariate Scores ----
# create 3 buckets for vulnerability and resilience to map to colours
vul_buckets <-
  la_vul_res %>%
  pull(vulnerability) %>%
  quantile(probs = seq(0, 1, length.out = 4), na.rm = TRUE)

res_buckets <-
  la_vul_res %>%
  pull(resilience) %>%
  quantile(probs = seq(0, 1, length.out = 4), na.rm = TRUE)

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
la_vul_res_bivariate <-
  la_vul_res %>% 
  mutate(vul_cut = cut(vulnerability,
                       breaks = vul_buckets,
                       include.lowest = TRUE),
         res_cut = cut(resilience,
                       breaks = res_buckets,
                       include.lowest = TRUE),
         group = paste(as.numeric(vul_cut),
                       "-",
                       as.numeric(res_cut))) %>%
  left_join(bivariate_color_scale, by = "group")

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
  # labs(x = expression("Higher Vulnerability" %->% " "),
  #      y = expression("Higher Resilience" %->% " ")) +
  # theme_minimal() +
  # theme(
  #   panel.background = element_rect(fill = "transparent", color = NA),
  #   plot.background = element_rect(fill = "transparent", color = NA),
  #   legend.background = element_rect(fill = "transparent"),
  #   legend.box.background = element_rect(fill = "transparent"),
  #   axis.title.x = element_text(size = 22),
  #   axis.title.y = element_text(size = 22),
  #   axis.ticks = element_blank(),
  #   panel.grid.major = element_blank(),
  #   panel.grid.minor = element_blank(),
  #   axis.text.x = element_blank(),
  #   axis.text.y = element_blank()
  # ) +
  # quadratic tiles
  coord_fixed()

ggsave(filename = "www/bivar-legend-void.jpg",
       bg = "transparent",
       width = 5.21,
       height = 5.21)

# ---- Create shp file ----
la_shp %>% 
  select(la_code = Code,
         geometry) %>% 
  filter(str_detect(la_code, "^E")) %>% 
  left_join(
    la_vul_res_bivariate %>% 
      select(la_code,
             la_name,
             resilience,
             vulnerability,
             group,
             fill),
    by = "la_code"
  ) %>%
  write_sf("data/la_shp_vul_res.geojson")

# ---- Create DT table ----
data <-
  la_shp %>% 
  as_tibble() %>% 
  select(`LA Code` = Code,
         `LA Name` = Name,
         `Clinical vulnerability` = Clinical.Vulnerability.quintile,
         `Health & Wellbeing vulnerability` = Health.Wellbeing.Vulnerability.quintile,
         `Economic vulnerability` = Economic.Vulnerability.quintile,
         `Social vulnerability` = Social.Vulnerability.quintile,
         `Socioeconomic vulnerability` = Socioeconomic.Vulnerability.quintile,
         `Overall vulnerability` = Vulnerability.quintile) %>% 
  filter(str_detect(`LA Code`, "^E")) %>% 
  left_join(
    res_index %>% 
      select(`LA Code` = LAD19CD,
             `Individual resilience` = `Individual/household Resilience decile`,
             `Community resilience` = `Community Resilience decile`,
             `VCS resilience` = `VCS Resilience decile`,
             `System resilience` = `System Resilience decile`,
             `Overall resilience` = `Resilience decile`),
    by = "LA Code"
  )

data %>% 
  write_rds("data/data.rds")

# ---- Prepare data for rose type pie charts ----
vi <-
  data %>% 
  select(-ends_with("resilience"),
         -starts_with("Overall"),
         -`LA Name`) %>% 
  pivot_longer(cols = ends_with("vulnerability"),
               names_to = "vulnerability",
               values_to = "score") %>% 
  rename(la_code = `LA Code`) %>% 
  mutate(vulnerability = str_remove(vulnerability, " vulnerability"))

write_rds(vi, "data/vi-long.rds")

ri <-
  data %>% 
  select(-ends_with("vulnerability"),
         -starts_with("Overall"),
         -`LA Name`) %>% 
  pivot_longer(cols = ends_with("resilience"),
               names_to = "resilience",
               values_to = "score") %>% 
  rename(la_code = `LA Code`) %>% 
  mutate(resilience = str_remove(resilience, " resilience"))

write_rds(ri, "data/ri-long.rds")
