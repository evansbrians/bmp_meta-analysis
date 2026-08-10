
# setup -------------------------------------------------------------------

library(clipr)
library(tidyverse)

lit_search_path <- "manuscript/supplemental/lit_search"

# Search strings associated with best management practices:
  
bmp_strings <- 
  list.files("bmps", full.names = TRUE) %>% 
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
  list.files("response_metrics", full.names = TRUE) %>% 
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
  file.path("species_search.txt") %>% 
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
