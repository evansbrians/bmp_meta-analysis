# Flow diagram

# setup -------------------------------------------------------------------

library(tidyverse)

bmp_long_raw <-
  here::here(
    "https://docs.google.com/spreadsheets/d",
    "1Lf3v8fU0sCCAcJ6Wj1v8GwgogjI4ve3xcMlPzLW0hnU"
  ) %>% 
  googlesheets4::read_sheet()

# How many papers:

bmp_long_raw %>% 
  distinct(key) %>% 
  nrow()

# 1 no review papers ------------------------------------------------------

bmp_no_review <-
  bmp_long_raw %>% 
  filter(problem != "review")

# How many papers maintained:

bmp_no_review %>% 
  distinct(key) %>% 
  nrow()

# How many papers filtered:

bmp_long_raw %>% 
  filter(problem == "review") %>% 
  distinct(key) %>% 
  nrow()

# 2 no access -------------------------------------------------------------

bmp_accessible <-
  bmp_no_review %>% 
  filter(problem != "no_access")

# How many papers maintained:

bmp_accessible %>% 
  distinct(key) %>% 
  nrow()

# How many papers filtered:

bmp_no_review %>% 
  filter(problem == "no_access") %>% 
  distinct(key) %>% 
  nrow()

# 3 did not evaluate the effect of a bmp --------------------------------

bmp_covers_bmp <-
  bmp_accessible %>% 
  filter(problem != "no_bmp")

# How many papers maintained:

bmp_covers_bmp %>% 
  distinct(key) %>% 
  nrow()

# How many papers filtered:

bmp_accessible %>% 
  filter(problem != "no_bmp") %>% 
  distinct(key) %>% 
  nrow()

# x no error --------------------------------------------------------------

bmp_includes_error <-
  bmp_covers_bmp %>% 
  filter(problem != "no_error")

# How many papers maintained:

bmp_includes_error %>% 
  distinct(key) %>% 
  nrow()

# How many papers filtered:

bmp_covers_bmp %>% 
  filter(problem == "no_error") %>% 
  distinct(key) %>% 
  nrow()
