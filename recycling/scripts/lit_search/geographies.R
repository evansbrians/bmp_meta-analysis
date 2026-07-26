list.files("data/lpr_000b21a_e/")

library(sf)
library(tmap)

tmap_mode("view")

tmap_options(check.and.fix = TRUE)

# shapefiles --------------------------------------------------------------

countries <-
  rnaturalearth::ne_countries(
    scale = "medium"
  ) %>% 
  filter(continent != "Antarctica")

north_america_states <- 
  rnaturalearth::ne_states(
    country = c("canada", "United States of America")
  )

# literature --------------------------------------------------------------

lit_search <- 
  read_csv("data/citations_by_bmp_long..csv") %>% 
  select(key, bmp, geography) %>% 
  drop_na(geography) %>% 
  separate_wider_delim(
    geography,
    delim = ";",
    names_sep = "_",
    too_few = "align_start") %>% 
  pivot_longer(
    geography_1:geography_4,
    names_to = "temp",
    values_to = "geography"
  ) %>% 
  select(!temp) %>% 
  drop_na(geography) %>% 
  mutate(
    geography = 
      geography %>% 
      str_trim() %>% 
      str_replace("_", " ") %>% 
      str_to_title()
  )

geography_count_states <-
  north_america_states %>% 
  left_join(
    lit_search %>% 
      summarize(
        n_studies = n(),
        .by = geography
      ),
    by = join_by(name == geography)
  ) %>% 
  select(name, n_studies) %>% 
  drop_na(n_studies)
  # mutate(
  #   n_studies = 
  #     replace_na(
  #       n_studies, 0)
  # )

geography_count_countries <- 
  countries %>% 
  left_join(
    lit_search %>% 
      summarize(
        n_studies = n(),
        .by = geography
      ),
    by = join_by(name == geography)
  ) %>% 
  select(name, n_studies) %>% 
  drop_na(n_studies)
studies, 0)


tm_shape(geography_count_countries) +
  tm_polygons(
    col = "n_studies", 
    n = 15,
    palette = "-Spectral")

tm_shape(geography_count_states) +
  tm_polygons(
    col = "n_studies", 
    n = 15,
    palette = "-Spectral")
