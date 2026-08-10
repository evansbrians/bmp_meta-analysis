str_c(
  "(ALL=((", 
  "nest AND ",
  str_c(
    "survival",
    "success",
    "predation",
    sep = " OR "
  ),
  "))",
  " OR (ALL=(",
  str_c(
    "survival",
    "abundance", 
    "density",
    "occupancy",
    sep = " OR "
  ),
  ")))"
) %>% 
  clipr::write_clip() %>% 
  write_file("searches/response_search.txt")



