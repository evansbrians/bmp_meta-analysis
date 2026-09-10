# Purpose: This script compares the papers in the metadata table with those in
# the tables where we extract study findings and clips out the table names for
# inserting into citations_by_bmp_long, var = in_analysis_table.

# setup -------------------------------------------------------------------

library(tidyverse)

# The extraction workbook:

extraction_workbook <- "data/raw/bmp_review_analysis_subset.xlsx"

# Rows read before a column type is fixed:

guess_rows <- 10000

# citations_by_bmp_long:

paper_metadata <-
  readxl::read_excel(
    "data/raw/citations_by_bmp_long.xlsx",
    guess_max = guess_rows
  ) %>%
  janitor::clean_names()

# papers used:

analysis_subset_papers <-
  readxl::excel_sheets(extraction_workbook) %>%
  set_names() %>%
  map(
    \(.sheet) {
      readxl::read_excel(
        extraction_workbook,
        sheet = .sheet,
        guess_max = guess_rows
      ) %>%
        distinct(key, bmp, paper) %>%
        mutate(table = .sheet)
    }
  ) %>%
  list_rbind() %>%
  summarize(
    table =
      str_flatten(table, collapse = "; "),
    .by = c(key, paper, bmp)
  )

# Add matches and write the clip:

paper_metadata %>%
  select(key, paper, bmp) %>%
  left_join(
    analysis_subset_papers,
    by = join_by(key, paper, bmp)
  ) %>%
  mutate(
    table = replace_na(table, "-")
  ) %>%
  pull(table) %>%
  clipr::write_clip()
