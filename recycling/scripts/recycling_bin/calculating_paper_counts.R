
# setup -------------------------------------------------------------------

library(googlesheets4)
library(tidyverse)

# Citations by bmp long:

papers <- 
  file.path(
    "https://docs.google.com/spreadsheets/d",
    "1Lf3v8fU0sCCAcJ6Wj1v8GwgogjI4ve3xcMlPzLW0hnU"
  ) %>% 
  read_sheet()

# Recorded effect sizes:

papers_effect_size <-
  file.path(
    "https://docs.google.com/spreadsheets/d",
    "14SWR7TXIKNvrYGr2_vwx9xBp5LDrDNaYcqZt6pldSoA"
  ) %>% 
  gs4_get() %>% 
  sheet_names() %>% 
  map_dfr(
    \(.x) {
      file.path(
        "https://docs.google.com/spreadsheets/d",
        "14SWR7TXIKNvrYGr2_vwx9xBp5LDrDNaYcqZt6pldSoA"
      ) %>% 
        read_sheet(sheet = .x) %>% 
        mutate(method = .x) 
    }
  ) %>% 
  distinct(paper, method)

# general summaries -------------------------------------------------------

papers %>% 
  summarize(
    n = n(), 
    .by = reviewed)

papers %>% 
  distinct(key, reviewed)

papers %>% 
  distinct(key, reviewed) %>% 
  mutate(
    reviewed = replace_na(reviewed, "no")
  ) %>% 
  summarize(
    n = n(),
    .by = reviewed
  )

# search for add rows -----------------------------------------------------

papers %>% 
  filter(
    str_detect(multiple_bmps, ";")
  ) %>% 
  mutate(
    n_bmp = str_count(multiple_bmps, ";") + 1
  ) %>% 
  summarize(
    n_rows = 
      length(
        unique(bmp)
      ),
    .by = c(paper, n_bmp)
  ) %>% 
  filter(n_bmp != n_rows) %>% 
  print(n = Inf)

# exploration: delay_hay --------------------------------------------------

delay_hay <- 
  papers %>% 
  filter(bmp == "delay_hay") %>% 
  select(
    paper, 
    effect_size:problem
  ) %>% 
  distinct() %>% 
  mutate(
    effect_calculated = 
      if_else(
        paper %in% papers_effect_size$paper,
        "yes",
        "no"
      )
  )

delay_hay %>% 
  print(n = Inf)

delay_hay %>% 
  summarize(
    n = n(),
    .by = useful
  )

delay_hay %>% 
  filter(problem != "-") %>% 
  summarize(
    n = n(),
    .by = problem
  ) # %>% pull(n) %>% sum()

# exploration: pesticides -------------------------------------------------

eliminate_pesticides <- 
  papers %>% 
  filter(bmp == "eliminate_pesticides") %>% 
  select(
    paper, 
    effect_size:problem
  ) %>% 
  distinct() %>% 
  mutate(
    effect_calculated = 
      if_else(
        paper %in% papers_effect_size$paper,
        "yes",
        "no"
      )
  )

eliminate_pesticides %>% 
  print(n = Inf)

eliminate_pesticides %>% 
  summarize(
    n = n(),
    .by = useful
  )

eliminate_pesticides %>% 
  filter(problem != "-") %>% 
  summarize(
    n = n(),
    .by = problem
  ) # %>% pull(n) %>% sum()

# install nest boxes ------------------------------------------------------

install_nest_boxes <- 
  papers %>% 
  filter(bmp == "install_nest_boxes") %>% 
  select(
    paper, 
    effect_size:problem
  ) %>% 
  distinct() %>% 
  mutate(
    effect_calculated = 
      if_else(
        paper %in% papers_effect_size$paper,
        "yes",
        "no"
      )
  )

install_nest_boxes %>% 
  print(n = Inf)

install_nest_boxes %>% 
  summarize(
    n = n(),
    .by = useful
  )

install_nest_boxes %>% 
  filter(problem != "-") %>% 
  summarize(
    n = n(),
    .by = problem
  )  %>% pull(n) %>% sum()

# keep cats indoors -------------------------------------------------------

keep_cats_indoors <- 
  papers %>% 
  filter(bmp == "keep_cats_indoors") %>% 
  select(
    paper, 
    effect_size:problem
  ) %>% 
  distinct() %>% 
  mutate(
    effect_calculated = 
      if_else(
        paper %in% papers_effect_size$paper,
        "yes",
        "no"
      )
  )

keep_cats_indoors %>% 
  print(n = Inf)

keep_cats_indoors %>% 
  summarize(
    n = n(),
    .by = useful
  )

keep_cats_indoors %>% 
  filter(problem != "-") %>% 
  summarize(
    n = n(),
    .by = problem
  )  %>% pull(n) %>% sum()

# plant nwsg --------------------------------------------------------------

plant_nwsg <- 
  papers %>% 
  filter(bmp == "plant_nwsg") %>% 
  select(
    paper, 
    effect_size:problem
  ) %>% 
  distinct() %>% 
  mutate(
    effect_calculated = 
      if_else(
        paper %in% papers_effect_size$paper,
        "yes",
        "no"
      )
  )

plant_nwsg %>% 
  print(n = Inf)

plant_nwsg %>% 
  summarize(
    n = n(),
    .by = useful
  )

plant_nwsg %>% 
  filter(problem != "-") %>% 
  summarize(
    n = n(),
    .by = problem
  )  %>% pull(n) %>% sum()

# prescribed fire ---------------------------------------------------------

prescribed_fire <- 
  papers %>% 
  filter(bmp == "prescribed_fire") %>% 
  select(
    paper, 
    effect_size:problem
  ) %>% 
  distinct() %>% 
  mutate(
    effect_calculated = 
      if_else(
        paper %in% papers_effect_size$paper,
        "yes",
        "no"
      )
  )

prescribed_fire %>% 
  print(n = Inf)

prescribed_fire %>% 
  summarize(
    n = n(),
    .by = useful
  )

prescribed_fire %>% 
  filter(problem != "-") %>% 
  summarize(
    n = n(),
    .by = problem
  ) # %>% pull(n) %>% sum()

# overwintering habitat ---------------------------------------------------

provide_overwintering_habitat <- 
  papers %>% 
  filter(bmp == "provide_overwintering_habitat") %>% 
  select(
    paper, 
    effect_size:problem
  ) %>% 
  distinct() %>% 
  mutate(
    effect_calculated = 
      if_else(
        paper %in% papers_effect_size$paper,
        "yes",
        "no"
      )
  )

provide_overwintering_habitat %>% 
  print(n = Inf)

provide_overwintering_habitat %>% 
  summarize(
    n = n(),
    .by = useful
  )

provide_overwintering_habitat %>% 
  filter(problem != "-") %>% 
  summarize(
    n = n(),
    .by = problem
  )  #%>% pull(n) %>% sum()

# remove non-native species -----------------------------------------------

remove_non_native_species <- 
  papers %>% 
  filter(bmp == "remove_non-native_species") %>% 
  select(
    paper, 
    effect_size:problem
  ) %>% 
  distinct() %>% 
  mutate(
    effect_calculated = 
      if_else(
        paper %in% papers_effect_size$paper,
        "yes",
        "no"
      )
  )

remove_non_native_species %>% 
  print(n = Inf)

remove_non_native_species %>% 
  summarize(
    n = n(),
    .by = useful
  )

remove_non_native_species %>% 
  filter(problem != "-") %>% 
  summarize(
    n = n(),
    .by = problem
  )  %>% pull(n) %>% sum()

# stream exclusion and buffers --------------------------------------------

stream_exclusion_and_buffers <- 
  papers %>% 
  filter(bmp == "stream_exclusion_and_buffers") %>% 
  select(
    paper, 
    effect_size:problem
  ) %>% 
  distinct() %>% 
  mutate(
    effect_calculated = 
      if_else(
        paper %in% papers_effect_size$paper,
        "yes",
        "no"
      )
  )

stream_exclusion_and_buffers %>% 
  print(n = Inf)

stream_exclusion_and_buffers %>% 
  summarize(
    n = n(),
    .by = useful
  )

stream_exclusion_and_buffers %>% 
  filter(problem != "-") %>% 
  summarize(
    n = n(),
    .by = problem
  )  %>% pull(n) %>% sum()

# summer pasture stockpiling ----------------------------------------------

summer_pasture_stockpiling <- 
  papers %>% 
  filter(bmp == "summer_pasture_stockpiling") %>% 
  select(
    paper, 
    effect_size:problem
  ) %>% 
  distinct() %>% 
  mutate(
    effect_calculated = 
      if_else(
        paper %in% papers_effect_size$paper,
        "yes",
        "no"
      )
  )

summer_pasture_stockpiling %>% 
  print(n = Inf)

summer_pasture_stockpiling %>% 
  summarize(
    n = n(),
    .by = useful
  )

summer_pasture_stockpiling %>% 
  filter(problem != "-") %>% 
  summarize(
    n = n(),
    .by = problem
  )  %>% pull(n) %>% sum()

# updgrade to darksky -----------------------------------------------------

upgrade_to_darksky <- 
  papers %>% 
  filter(bmp == "upgrade_to_darksky") %>% 
  select(
    paper, 
    effect_size:problem
  ) %>% 
  distinct() %>% 
  mutate(
    effect_calculated = 
      if_else(
        paper %in% papers_effect_size$paper,
        "yes",
        "no"
      )
  )

upgrade_to_darksky %>% 
  print(n = Inf)

upgrade_to_darksky %>% 
  summarize(
    n = n(),
    .by = useful
  )

upgrade_to_darksky %>% 
  filter(problem != "-") %>% 
  summarize(
    n = n(),
    .by = problem
  )  %>% pull(n) %>% sum()


# grazing_intensity -------------------------------------------------------




