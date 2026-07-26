# Geography of used studies

# setup -------------------------------------------------------------------

library(sf)
library(tmap)
library(tidyverse)

tmap_mode("view")

tmap_options(check.and.fix = TRUE)

# spatial data ------------------------------------------------------------

# Get and process country-level data:

countries_raw <- 
  rnaturalearth::ne_countries() %>% 
  mutate(
    iso_a2,
    country =
      name_en %>% 
      tolower() %>% 
      str_remove_all("[^a-z ]") %>% 
      str_replace_all(" ", "_"),
    continent = 
      continent %>% 
      tolower() %>% 
      str_remove_all("[^a-z ]") %>% 
      str_replace_all(" ", "_"),
  ) %>% 
  select(
    iso_a2,
    country,
    continent
  )

# Get and process state-level data:

states_raw <-
  rnaturalearth::ne_states() %>% 
  mutate(
    iso_a2,
    state = 
      name_en %>% 
      tolower() %>% 
      str_remove_all("[^a-z ]") %>% 
      str_replace_all(" ", "_"),
    region,
    region_sub
  ) %>% 
  select(
    iso_a2,
    state,
    region,
    region_sub
  ) %>% 
  filter(
    iso_a2 %in%
      c(
        "CA",
        "US",
        "MX"
      )
  ) %>% 
  
  # The District!
  
  mutate(
    state = 
      if_else(
        state == "washington" &
          region == "South",
        "district_of_columbia",
        state
      )
  ) %>% 
  rename(country_code = iso_a2) %>% 
  st_transform(crs = 5070)

# Level 1 ecoregions

ecoregions <- 
  st_read("data/raw/ecoregions_level_1/NA_CEC_Eco_Level1.shp") %>% 
  janitor::clean_names() %>% 
  st_transform(crs = 5070) %>% 
  rmapshaper::ms_simplify(keep = 0.1) %>% 
  st_make_valid() %>% 
  mutate(
    ecoregion = str_to_title(na_l1name)
  ) %>% 
  select(ecoregion)

ecoregions %>% 
  tm_shape() +
  tm_polygons() +
  states_raw %>% 
  filter(state == "north_dakota") %>% 
  tm_shape() +
  tm_polygons(col = "red")

# States with level 1 ecoregion:

states_great_plains <- 
  states_raw %>% 
  st_filter(
    ecoregions %>% 
      filter(ecoregion == "Great Plains")
  ) %>% 
  mutate(region_sub = "Great Plains")

# Collapse ecoregions into a single column:

states_region <-
  states_raw %>% 
  filter(
    !state %in% states_great_plains$state
  ) %>% 
  bind_rows(states_great_plains)

# tabular data ------------------------------------------------------------

# Tabular data from shapefiles:

countries_tbl <- 
  countries_raw %>% 
  st_drop_geometry() %>% 
  as_tibble() %>% 
  rename(country_code = iso_a2)

states_tbl <- 
  states_region %>% 
  st_drop_geometry() %>% 
  as_tibble()

# Get citation data (regardless of used or unused):

citations_by_bmp_long <- 
  file.path(
    "https://docs.google.com/spreadsheets/d",
    "1Lf3v8fU0sCCAcJ6Wj1v8GwgogjI4ve3xcMlPzLW0hnU"
  ) %>% 
  googlesheets4::read_sheet() %>% 
  filter(article_type == "research") %>% 
  select(
    key, 
    geography
  )

# Get used citations:

citations_used_in_paper <- 
  c(
    "mean_diff",
    "beta_categorical",
    "beta_continuous"
  ) %>% 
  map_df(
    ~ file.path(
      "https://docs.google.com/spreadsheets/d",
      "14SWR7TXIKNvrYGr2_vwx9xBp5LDrDNaYcqZt6pldSoA"
    ) %>% 
      googlesheets4::read_sheet(.x) %>% 
      distinct(key)
  ) %>% 
  distinct(key)

# Separate entries with multiple geographies into rows and clean the data:

citations_clean <- 
  citations_by_bmp_long %>% 
  mutate(
    geography = 
      geography %>% 
      tolower() %>% 
      str_remove("conterminous_") %>% 
      str_replace("north_america", "canada; mexico; united_states") %>% 
      str_replace("united_states", "united_states_of_america") %>% 
      str_replace("china", "peoples_republic_of_china") %>% 
      str_replace("scotland", "united_kingdom")
  ) %>% 
  separate_longer_delim(geography, delim = ";") %>%
  mutate(
    geography = 
      geography %>% 
      str_trim() %>% 
      str_replace_all(" ", "_")
  ) %>% 
  filter(
    str_detect(geography, "[a-z]"),
    geography != "global"
  ) %>% 
  distinct()

# citations with geography ------------------------------------------------

# State-level geography reported:

citations_state_subset <- 
  citations_clean %>% 
  inner_join(
    states_tbl, 
    by = join_by(geography == state)
  ) %>% 
  rename(state = geography) %>% 
  left_join(
    countries_tbl,
    by = "country_code"
  ) %>% 
  select(
    key,
    continent,
    country_code,
    country,
    state,
    region,
    region_sub
  )

# Country-level geography reported:

citations_country_subset <- 
  citations_clean %>% 
  anti_join(citations_state_subset, by = "key") %>% 
  inner_join(
    countries_tbl, 
    by = join_by(geography == country)
  ) %>% 
  rename(country = geography)


# Citations with geography:

citations_geo <- 
  citations_state_subset %>% 
  bind_rows(citations_country_subset)

# counting papers by region -----------------------------------------------

# Citation counts by continent:

citations_geo %>% 
  distinct(key, continent) %>% 
  semi_join(citations_used_in_paper, by = "key") %>% 
  summarize(
    n = n(),
    .by = continent
  ) %>% 
  mutate(
    total_papers = sum(n),
    prop = n / total_papers * 100
  )

# Citation counts by country:

citations_geo %>% 
  distinct(key, country) %>% 
  semi_join(citations_used_in_paper, by = "key") %>% 
  summarize(
    n = n(),
    .by = country
  ) %>% 
  mutate(
    total_papers = sum(n),
    prop = n / total_papers * 100
  ) %>% 
  arrange(-n)

# Citation counts by region_sub:

citations_geo %>% 
  distinct(key, region_sub) %>% 
  drop_na(region_sub) %>% 
  semi_join(citations_used_in_paper, by = "key") %>% 
  summarize(
    n = n(),
    .by = region_sub
  ) %>% 
  mutate(
    total_papers = sum(n),
    prop = n / total_papers * 100
  ) %>% 
  arrange(-n)

# Or:

citations_geo %>% 
  mutate(
    region = 
      if_else(
        region_sub == "Great Plains",
        "Great Plains",
        region
      )
  ) %>% 
  distinct(key, region) %>% 
  drop_na(region) %>% 
  semi_join(citations_used_in_paper, by = "key") %>% 
  summarize(
    n = n(),
    .by = region
  ) %>% 
  mutate(
    total_papers = sum(n),
    prop = n / total_papers * 100
  ) %>% 
  arrange(-n)

# Citation counts by state:

citations_geo %>% 
  distinct(key, state) %>% 
  drop_na(state) %>% 
  semi_join(citations_used_in_paper, by = "key") %>% 
  summarize(
    n = n(),
    .by = state
  ) %>% 
  mutate(
    total_papers = sum(n),
    prop = n / total_papers * 100
  ) %>% 
  arrange(-n)

# mapping -----------------------------------------------------------------

citations_geo %>% 
  distinct(key, state, region_sub) %>% 
  semi_join(citations_used_in_paper, by = "key") %>% 
  summarize(
    n = n(),
    .by = c(state, region_sub)
  ) %>% 
  left_join(
    states_raw %>% 
      select(!region_sub),
    .,
    by = "state"
  ) %>% 
  summarize(
    n = sum(n, na.rm = TRUE),
    geometry = st_combine(geometry),
    .by = region_sub
  ) %>% 
  tm_shape() +
  tm_polygons(col = "n")

states_raw %>% 
  inner_join(
    citations_state_subset %>% 
      semi_join(citations_used_in_paper, by = "key") %>% 
      count(geography) %>% 
      mutate(
        percent_of_papers = 
          n/sum(n) * 100
      ),
    by = join_by(state == geography)
  ) %>% 
  tm_shape() +
  tm_polygons(
    col = "n",
    palette = "YlOrRd"
  )

# Distribution by region:

citations_state_subset %>% 
  semi_join(citations_used_in_paper, by = "key") %>% 
  count(region_sub) %>% 
  mutate(
    percent_of_papers = 
      n/sum(n) * 100
  ) %>% 
  arrange(-percent_of_papers)

states_raw %>% 
  filter(iso_a2 %in% c("US", "CA")) %>%
  left_join(
    citations_state_subset %>% 
      semi_join(citations_used_in_paper, by = "key") %>% 
      count(geography) %>% 
      mutate(
        percent_of_papers = 
          n/sum(n) * 100
      ),
    by = "region"
  ) %>% 
  summarize(
    n = unique(n),
    st_union(geometry),
    .by = region_sub
  ) %>% 
  tmap::tm_shape() +
  tmap::tm_polygons(
    col = "n",
    palette = "Greens"
  )

states_raw %>% 
  filter(iso_a2 %in% c("US", "CA")) %>% 
  mutate(
    region_sub = 
      case_when(
        name_en == "Saskatchewan" ~ "West North Central",
        .default = region_sub
      )
  ) %>% 
  left_join(
    citations_state_subset %>% 
      mutate(
        region_sub = 
          case_when(
            geography == "saskatchewan" ~ "West North Central",
            .default = region_sub
          )
      ) %>% 
      semi_join(citations_used_in_paper, by = "key") %>% 
      count(region_sub) %>% 
      mutate(
        percent_of_papers = 
          n/sum(n) * 100
      ),
    by = "region_sub"
  ) %>% 
  summarize(
    n = unique(n),
    st_union(geometry),
    .by = region_sub
  ) %>% 
  tmap::tm_shape() +
  tmap::tm_polygons(
    col = "n",
    palette = "Greens"
  )

