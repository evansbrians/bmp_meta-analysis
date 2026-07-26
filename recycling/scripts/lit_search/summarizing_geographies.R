# The gloa here was to summarize the geographic locations of studies

# setup -------------------------------------------------------------------

# Libraries:

library(sf)
library(tidvyerse)

# States shapefiles:

states <-
  st_read("data/states.shp") %>% 
  as_tibble() %>% 
  janitor::clean_names() %>% 
  mutate(
    name = 
      tolower(name) %>% 
      str_replace_all(" ", "_")
  ) %>% 
  arrange(name) %>% 
  select(
    geography = name,
    region) %>% 
  mutate(
    region = 
      if_else(
        geography %in% c("texas", "oklahoma"),
        "West",
        region
      )
  )

# geography proc ----------------------------------------------------------

my_geography <- 
  papers %>% 
  select(key, geography) %>%
  pull(geography) %>% 
  unique() %>% 
  map(
    ~ str_split(.x, ";") %>% 
      unlist()
  ) %>% 
  unlist() %>% 
  str_trim() %>% 
  unique() %>% 
  keep(~ !is.na(.x)) %>% 
  keep(~ .x != "-") %>% 
  keep(~ .x != "conterminous_united_states") %>% 
  map_chr(
    ~ str_replace(
      .x,
      "^tennesse$",
      "tennessee"
    )
  )

# Make long-form version of geographies:

papers_geography_long <- 
  my_geography %>% 
  map_df(
    ~ papers %>% 
      filter(
        str_detect(
          geography,
          .x
        )
      ) %>% 
      mutate(geography = .x)
  ) %>% 
  distinct(key, geography)

# Check keys (for some reason):

papers_geography_long %>% 
  filter(
    !(
      geography %in% states$geography |
        geography == "united_states"
    )
  ) %>% 
  distinct(key)


# proportion of studies ---------------------------------------------------

papers_geography_long %>% 
  inner_join(
    states,
    by = "geography"
  ) %>% 
  summarize(
    n = 
      key %>% 
      unique() %>% 
      length(),
    .by = region
  ) %>% 
  mutate(
    prop = n / sum(n) * 100
  )

# I dunno:

papers_geography_long %>% 
  anti_join(
    states,
    by = join_by(geography == geography)
  ) %>% 
  distinct(key, geography)

