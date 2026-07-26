library(clipr)
library(tidyverse)

# bird searches -----------------------------------------------------------

bird_search <-
  str_c(
    "(ALL=((\"bird\" OR \"avian\" OR \"ave$\") AND (",
    str_c(
      "grassland",
      "meadow",
      "shrub",
      "scrub",
      "hay*",
      "\"early-succession*\"",
      "\"open-habitat\"",
      sep = " OR "
    ),
    ")))"
  )

bird_search_by_species <-
  read_lines("searches/species_search.txt")

bird_search_combined <-
  str_c(
    "(", 
    str_c(
      bird_search,
      # "(TS=(*bird* OR avian OR ave$))",
      bird_search_by_species,
      sep = " OR "),
    ")"
  )

# response searches -------------------------------------------------------

response_search <-
  str_c(
    "(TS=(",
    str_c(
      "survival",
      "predation",
      "mortality",
      "\"nest success\"",
      "\"species_richness\"",
      "diversity",
      "abundance",
      "density",
      "occupancy",
      sep = " OR "
    ),
    "))"
  )

# combine bird and response searches --------------------------------------

bird_and_response_search <- 
  str_c(
    bird_search_combined,
    response_search,
    sep = " AND "
  )

# Testing:

bird_and_response_search %>% 
  write_clip()

# bmp: delay_hay ----------------------------------------------------------

delay_hay <-
  str_c(
    "(TS=(",
    str_c(
      "\"early hay*\"",
      "\"late hay*\"",
      "\"hay tim*\"",
      "hay-crop*",
      "\"hay harvest*\"",
      "\"mow*\"",
      "\"hayfield management\"",
      "\"early hay\"",
      "\"late hay\"",
      "\"delayed mowing\")",
      "(TS= (hay AND cut*))",
      "(TS=(mowing AND (regime OR dates)))",
      "(TS=\"mowed and unmowed\")",
      sep = " OR "
    ),
    ")"
  )

# For inputting into Web of Science:

str_c(
  "(DT = Article)",
  delay_hay,
  bird_search_combined,
  sep = " AND "
) %>% 
  clipr::write_clip()

# bmp: summer_pasture_stockpiling -----------------------------------------

summer_pasture_stockpiling <-
  str_c(
    "(TS=(",
    str_c(
      "\"amp graz*\"",
      "\"rest-rotation\"",
      "\"Adaptive multi-paddock\"",
      "\"pasture stockpiling\"",
      "\"rotational graz*\"",
      "\"mob graz*\"",
      "\"adaptive graz*\"",
      sep = " OR "
    ),
    "))"
  )

str_c(
  "(DT = Article)",
  summer_pasture_stockpiling,
  bird_search_combined,
  sep = " AND "
) %>% 
  clipr::write_clip()


