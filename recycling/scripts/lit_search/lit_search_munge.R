
# setup -------------------------------------------------------------------

library(tidyverse)

# Read google form responses:

review_start <-
  googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1m3vAkLcGcKXLX4Rbc6VgiOcWC6llvvkGnrzWWaCH6AY/edit?usp=sharing") %>% 
  janitor::clean_names() %>% 
  rename(
    reviewer = your_name_first,
    author = authors_last_name,
    year = the_year_the_paper_was_written,
    class = type_of_paper,
    geography = the_geographic_location_of_the_paper_check_all_that_apply,
    bmp = best_management_practices_predictor_variable_check_all_that_apply,
    response_var = response_variable_check_all_that_apply
  )

# Best management practices:

bmps <-
  c(
    "Delay your first cutting of hay",
    "Summer pasture stockpiling",
    "Raise your blades",
    "Plant native warm season grasses",
    "Work with neighbors on shared management",
    "Remove non-native species",
    "Keep all cats indoors",
    "Add a flushing bar",
    "Set aside unmowed areas adjacent to mowed areas",
    "Manage fields in patches",
    "Avoid mowing at night",
    "Install nest boxes",
    "Provide overwintering habitat",
    "Eliminate the use of pesticides",
    "Upgrade all outdoor lighting to be Dark Sky compliant",
    "Transition to non-lead ammunition",
    "Stream exclusion and buffer plantings"
  )

# read and process google form responses ----------------------------------

intern_review <-
  review_start %>% 
  mutate(
    year = 
      year %>% 
      as.character() %>% 
      str_sub(end = 4) %>% 
      as.numeric(),
    author = str_trim(author)
  ) %>% 
  unite(
    col = "paper",
    c(author, year),
    sep = " ") %>% 
  filter(
    class == "Original research",
    numerical_response == "Yes") %>% 
  select(
    !c(timestamp, 
       reviewer, 
       class, 
       numerical_response)) %>% 
  
  # Make response variables long:
  
  separate_wider_delim(
    response_var, 
    delim = ",",
    names_sep = "_",
    too_few = "align_start") %>% 
  pivot_longer(
    matches("response_var"),
    values_to = "response_var") %>% 
  select(!name) %>% 
  drop_na(response_var) %>% 
  
  # Make best management practice long:
  
  mutate(
    bmp = str_remove_all(bmp, "i.e., ")
  ) %>% 
  separate_wider_delim(
    bmp, 
    delim = ",",
    names_sep = "_",
    too_few = "align_start") %>% 
  pivot_longer(
    matches("bmp_"),
    values_to = "bmp") %>% 
  select(!name) %>% 
  drop_na(bmp) %>% 
  
  # Clean geography, response_var, and bmp:
  
  mutate(
    
    # Clean geography:
    
    geography = 
      case_when(
        str_detect(geography, "Southeastern") ~ "Southeast",
        str_detect(geography, "Mid-Atlantic") ~ "Mid-Atlantic",
        str_detect(geography, "Eastern") ~ "Eastern US",
        is.na(geography) ~ "None",
        geography == "No specific location" ~ "None",
        .default = geography),
    
    # Clean response variable:
    
    response_var = str_trim(response_var),
    response_var =
      case_when(
        response_var == "Survival of nestlings" ~ "Nest success",
        response_var == "Survival of fledglings" ~ "Fledgling survival",
        response_var == "Survival of adults" ~ "Adult survival",
        str_detect(response_var, "species richness") ~ "Alpha diversity",
        .default = response_var
      ),
    
    # Clean best management practices:
    
    bmp = str_trim(bmp),
    bmp = 
      case_when(
        str_detect(bmp, "pestic") ~ 
          "Eliminate the use of pesticides",
        str_detect(bmp, "neighbors") ~
          "Work with neighbors on shared management",
        str_detect(bmp, "[Bb]urn|fire") ~ 
          "Prescribed burning",
        str_detect(bmp, "warm season grasses") ~ 
          "Plant native warm season grasses",
        str_detect(bmp, "perennial grasslands") ~ 
          "Plant native warm season grasses",
        str_detect(bmp, "habitat heterogeneity") ~ 
          "Manage fields in patches",
        str_detect(bmp, "flushing bar") ~ 
          "Add a flushing bar",
        bmp == "Don't mow at night" ~ 
          "Avoid mowing at night",
        bmp == "Raise the height of cutting blades" ~ 
          "Raise your blades",
        bmp == "Delayed haying" ~ 
          "Delay your first cutting of hay",
        str_detect(bmp, "Exclude cattle from streams") ~
          "Stream exclusion and buffer plantings",
        .default = bmp
      )
  ) %>% 
  
  # Remove records that have no best management practice or do not align
  # with one of the VWL BMPs:
  
  filter(
    !bmp %in%
      c(
        "None",
        "landscape and regional effects",
        "ownership types and the association of different land use activities",
        "hayfield coverage",
        "wetland abundance",
        "grass monocultures",
        "alternative vegetation management treatments",
        "scheduled harvests",
        "Prescribed burning",
        "hedgerows",
        "Tree removal",
        "buy local",
        "irrigation",
        "Now alternate year",
        "Alter mowing practices",
        "Do not mow from the outside first",
        "Till soil to a minimum",
        "diversify habitat management",
        "Removing shrub and tree cover",
        "land consolidation",
        "early harvest",
        "field buffers",
        "differing management strategies",
        "grazing management systems",
        "grazing intensity",
        "rotational grazing"
      )
  ) %>% 
  
  # Remove duplicates:
  
  distinct()


# response variables ------------------------------------------------------

intern_review %>% 
  summarize(
    n = 
      length(
        unique(paper)),
    .by = response_var
  ) %>% 
  arrange(
    desc(n)
  )

# best management practices -----------------------------------------------

intern_review %>% 
  summarize(
    n = 
      length(
        unique(paper)),
    .by = bmp
  ) %>% 
  arrange(
    desc(n)
  )



# combined response variables and BMPs ------------------------------------

# All:

response_var_bmp_summary <-
  intern_review %>% 
  summarize(
    n = 
      length(
        unique(paper)),
    .by = c(response_var, bmp)
  ) %>%
  arrange(
    desc(n)) %>% 
  filter(n > 1)

# Response variable is abundance:

response_var_bmp_summary %>%
  filter(response_var == "Bird abundance")

# Response variable is alpha diversity:

response_var_bmp_summary %>%
  filter(response_var == "Alpha diversity")

# Response variable is associated with (demographic) habitat quality:

response_var_bmp_summary %>%
  filter(
    str_detect(response_var, "survival|Nest|Repro")
  )

# targeting search --------------------------------------------------------

intern_review %>% 
  filter(
    response_var == "Bird abundance",
    bmp == "Plant native warm season grasses") %>% 
  distinct(paper) %>% 
  arrange(paper) %>% 
  clipr::write_clip()

intern_review %>% 
  filter(
    response_var == "Alpha diversity") %>% 
  distinct(paper) %>% 
  arrange(paper) %>% 
  clipr::write_clip()

