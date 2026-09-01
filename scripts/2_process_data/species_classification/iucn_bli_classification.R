
# Species list and habitat classification:
#
# Before analysis:
#
# 1. Obtain key from: https://api.iucnredlist.org/
# 2. Save the key to your R environment:
#   `usethis::edit_r_environ()`
#   `IUCN_REDLIST_KEY=[provided key]`

# setup -------------------------------------------------------------------

library(rredlist)
library(tidyverse)

# get a list of all birds -------------------------------------------------

# All bird records (~20 minutes):

bird_assessment_ids <-
  rl_class(
    "Aves",
    all = TRUE,
    quiet = FALSE
  )$assessments

# Request each full assessment:

bird_assessments <-
  rl_assessment_list(
    bird_assessment_ids$assessment_id,
    wait_time = 0.5,
    quiet = FALSE
  )

# Extract taxonomy and habitats:

bird_habitats <-
  rl_assessment_extract(
    bird_assessments,
    c("taxon", "habitats"),
    format = "df",
    flatten = TRUE
  )

# process list ------------------------------------------------------------

bird_habitats_taxon <-
  bird_habitats %>%
  select(
    sis_id:scientific_name,
    order_name:family_name,
    common_names,
    assessment_id,
    habitat_class = description.en,
    season,
    major_importance = majorImportance
  ) %>%
  distinct() %>%
  mutate(
    common_names = map(common_names, as_tibble)
  ) %>%
  unnest(common_names, keep_empty = TRUE) %>%
  filter(main, language == "eng") %>%
  select(
    !c(main, language)
  ) %>%
  distinct() %>%
  nest(habitat = assessment_id:major_importance)

# Write to file:

write_rds(bird_habitats_taxon, "data/processed/iucn_bli_classification.rds")


