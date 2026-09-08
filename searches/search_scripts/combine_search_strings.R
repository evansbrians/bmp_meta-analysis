
# setup -------------------------------------------------------------------

library(clipr)
library(tidyverse)

# Search strings associated with best management practices:
  
bmp_strings <- 
  list.files("searches/bmps", full.names = TRUE) %>% 
  set_names(
    str_extract(., "[a-z_]*\\.txt$") %>% 
      str_remove("\\.txt")
  ) %>% 
  map(
    ~ read_lines(.x) %>% 
      str_c(collapse = "")
  )
  
# Search strings associated with response metrics:

response_metrics <-
  list.files("searches/response_metrics", full.names = TRUE) %>% 
  set_names(
    str_extract(., "[a-z_]*\\.txt$") %>% 
      str_remove("\\.txt")
  ) %>% 
  map(
    ~ read_lines(.x) %>% 
      str_c(collapse = "")
  )

# Species searches:

species_search <-
  file.path("searches/species_search.txt") %>% 
  read_lines()  %>% 
  str_c(collapse = "")

# searches ----------------------------------------------------------------

# Example search to paste into Web of Science:

c(
  bmp_strings$delay_hay,
  response_metrics$nest_success,
  species_search
) %>% 
  str_c(collapse = " AND ") %>% 
  write_clip()
