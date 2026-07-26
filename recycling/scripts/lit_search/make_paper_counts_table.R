
# Script for making the paper counts table in our Google Drive

# setup -------------------------------------------------------------------

library(googlesheets4)
library(tidyverse)

# URL for the effect size table:

sheet_url <- 
  file.path(
    "https://docs.google.com/spreadsheets/d",
    "14SWR7TXIKNvrYGr2_vwx9xBp5LDrDNaYcqZt6pldSoA",
    "edit?gid=1072800016"
  )

# Get citations_by_bmp_long:

papers <- 
  file.path(
    "https://docs.google.com/spreadsheets/d",
    "1Lf3v8fU0sCCAcJ6Wj1v8GwgogjI4ve3xcMlPzLW0hnU"
  ) %>% 
  read_sheet() %>% 
  mutate(
    key = tolower(key),
    bmp = 
      if_else(
        bmp == "no_bmp", 
        "z-no_bmp",
        bmp
      )
  )

# Get effect size tables:

bmp_effects <- 
  sheet_url %>% 
  googlesheets4::sheet_names() %>% 
  map_df(
    ~ googlesheets4::read_sheet(
      sheet_url,
      sheet = .x
    ) %>% 
      mutate(
        sheet = .x,
        .before = paper
      )
  ) %>% 
  mutate(
    across(
      everything(),
      ~ tolower(.x)
    )
  )

papers %>% 
  filter(
    reviewed %in% c("no", "pending")
  ) %>% 
  semi_join(bmp_effects, by = "paper")

# n papers ----------------------------------------------------------------

n_papers <- 
  papers %>% 
  summarize(
    n_papers = 
      key %>% 
      unique() %>% 
      length(),
    .by = bmp
  ) %>% 
  arrange(bmp)

# reviewed papers ---------------------------------------------------------

reviewed_papers <- 
  papers %>% 
  drop_na(reviewed) %>% 
  filter(
    reviewed == "yes") %>% 
  summarize(
    n_papers_reviewed = 
      key %>% 
      unique() %>% 
      length(),
    .by = bmp
  )  %>% 
  arrange(bmp)

papers %>% 
  filter(reviewed %in% c("no", "pending")) %>% 
  summarize(
    n_papers_reviewed = 
      key %>% 
      unique() %>% 
      length(),
    .by = bmp
  )  %>% 
  arrange(bmp)

# effect size derived -----------------------------------------------------

effect_size_derived <-
  papers %>% 
  drop_na(effect_size) %>% 
  filter(
    useful %in% c("yes", "maybe"),
    effect_size == "yes"
  ) %>% 
  summarize(
    n_papers_effect_size = 
      key %>% 
      unique() %>% 
      length(),
    .by = bmp
  ) %>% 
  arrange(bmp)


# calculate by tracing ----------------------------------------------------

papers %>% 
  drop_na(effect_size) %>% 
  filter(
    effect_size == "calculated",
    useful %in% c("yes", "maybe"),
    problem == "trace_plot"
  ) %>% 
  summarize(
    n_papers_effect_size = 
      key %>% 
      unique() %>% 
      length(),
    .by = bmp
  ) %>% 
  arrange(bmp)

# calculate by tables ----------------------------------------------------

papers %>% 
  drop_na(effect_size) %>% 
  filter(
    effect_size == "calculated",
    useful %in% c("yes", "maybe"),
    problem != "trace_plot"
  ) %>% 
  summarize(
    n_papers_effect_size = 
      key %>% 
      unique() %>% 
      length(),
    .by = bmp
  ) %>% 
  arrange(bmp)




# papers with *problems* --------------------------------------------------

problems <- 
  papers %>% 
  drop_na(problem) %>% 
  filter(
    !problem %in% 
      c(
        "-",
        "no_bmp", 
        "trace_plot"
      )
  ) %>% 
  summarize(
    n = 
      key %>% 
      unique() %>% 
      length(),
    .by = c(bmp, problem)
  ) %>% 
  pivot_wider(
    names_from = problem,
    values_from = n
  ) %>% 
  select(
    bmp,
    unusable_treatment,
    unusable_response, 
    no_quantitative_results,
    no_error,
    review,
    no_access,
    unknown
  ) %>% 
  mutate(
    across(
      !bmp,
      ~ replace_na(.x, 0)
    ),
    bmp = 
      if_else(
        bmp == "no_bmp", 
        "z-no_bmp",
        bmp
      )
  ) %>% 
  arrange(bmp)

# make paper counts table -------------------------------------------------

paper_counts_table <- 
  n_papers %>% 
  left_join(reviewed_papers, by = "bmp") %>% 
  left_join(effect_size_derived, by = "bmp") %>% 
  left_join(problems, by = "bmp")
