library(tidyverse)

big_sheet <-
  googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1qllmPt3LEFx1PaxXcvYz7fkn05ZaLMs72nNtIrbPnSc/edit?usp=sharing")

big_sheet %>% 
  pull(paper) %>% 
  unique()
  
big_sheet %>% 
  filter(!is.na(effect))

big_sheet %>% 
  filter(
    !is.na(effect),
    bmp != "X")

big_sheet %>% 
  filter(
    !is.na(effect),
    bmp != "X") %>% 
  summarize(
    n = n(),
    .by = bmp
  ) %>% 
  arrange(
    desc(n)
  )

big_sheet %>% 
  filter(
    !is.na(effect),
    bmp != "X") %>% 
  mutate(
    response_class = 
      if_else(
        response_class == "local diversity",
        "species richness",
        response_class)
  ) %>% 
  summarize(
    n = n(),
    .by = response_class
  ) %>% 
  arrange(
    desc(n)
  )



  
