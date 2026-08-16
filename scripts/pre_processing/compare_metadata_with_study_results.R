# Purpose: This script compares the papers in the metadata table (in Google
# sheets) with those in the tables where we extract study findings and clips out
# the table names for inserting into citations_by_bmp_long, var =
# in_analysis_table.

# setup -------------------------------------------------------------------

library(tidyverse)

# citations_by_bmp_long:

paper_metadata <-
  str_c(
    "https://docs.google.com/spreadsheets/d/",
    "1Lf3v8fU0sCCAcJ6Wj1v8GwgogjI4ve3xcMlPzLW0hnU"
  ) %>% 
  googlesheets4::read_sheet() %>%
  janitor::clean_names()

# papers used:

analysis_subset_papers <- 
  list(
    "mean_diff",
    "beta_categorical",
    "beta_continuous",
    "other_categorical",
    "other_continuous"
  ) %>% 
  set_names() %>% 
  map(
    \ (.x) {
      str_c(
        "https://docs.google.com/spreadsheets/d/",
        "14SWR7TXIKNvrYGr2_vwx9xBp5LDrDNaYcqZt6pldSoA"
      ) %>% 
      googlesheets4::read_sheet(sheet = .x) %>% 
        distinct(key, bmp, paper) %>% 
        mutate(table = .x)
    }
  ) %>% 
  list_rbind() %>% 
  summarize(
    table = str_flatten(table, collapse = "; "),
    .by = c(key, paper, bmp)
  )

# Add matches and write clip for inserting the column:

paper_metadata %>% 
  select(key, paper, bmp) %>% 
  left_join(
    analysis_subset_papers,
    by = c("key", "paper", "bmp")
  ) %>% 
  mutate(
    table = replace_na(table, "-")
  ) %>% 
  pull(table) %>% 
  clipr::write_clip()
