

# set-up ------------------------------------------------------------------

library(tidyverse)

# Google sheet url:

url <- 
  str_c(
    "https://docs.google.com/spreadsheets/d/",
    "14SWR7TXIKNvrYGr2_vwx9xBp5LDrDNaYcqZt6pldSoA"
  )

# Read in the google sheets:

analysis_subset_list <-
  googlesheets4::sheet_names(url) %>% 
  set_names() %>% 
  map(
    ~ googlesheets4::read_sheet(
      url,
      sheet = .x
    ) %>% 
      janitor::clean_names()
  )

# Function to clean common names:

fix_common_names <-
  function(.common_name) {
    .common_name %>% 
      str_replace_all("-", " ") %>% 
      str_remove_all("'") %>% 
      str_to_snake()
  }

# clean bmps --------------------------------------------------------------

analysis_subset_list_bmp_edits <-
  analysis_subset_list %>% 
  map(
    ~ pluck(.x) %>% 
      # distinct(.x, bmp) %>% 
      mutate(
        bmp = 
          bmp %>% 
          str_to_lower() %>% 
          str_replace_all("; ", "--") %>% 
          str_replace_all(" ", "_") %>% 
          str_trim() %>% 
          str_replace(
            "plant_native_warm_season_grasses",
            "plant_nwsg"
          ) %>% 
          str_remove("_\\(nwsgs\\)_and_wildflowers") %>% 
          str_replace("nwsgs", "nwsg") %>% 
          str_remove(",_including_insecticides_and_rodenticides") %>% 
          str_replace(
            "_the_use_of_pesticides",
            "_pesticides"
          ) %>% 
          str_replace("manage_fields_in_patches", "manage_in_patches") %>% 
          str_replace("_plantings", "s") %>%  
          str_replace(
            "delay_your_first_cutting_of_hay",
            "delay_hay"
          ) %>% 
          str_replace(
            "keep_all_cats_indoors",
            "keep_cats_indoors"
          ) %>% 
          str_replace("__", "_") %>% 
          str_replace("--", "; ")
      )
  )

# clean species names -----------------------------------------------------

analysis_subset_list_species_edits <- 
  analysis_subset_list_bmp_edits %>% 
  map(
    ~ .x %>% 
      mutate(
        species = 
          species %>% 
          fix_common_names() %>% 
          str_replace("^lapwing", "northern_lapwing") %>% 
          str_replace("^skylark", "eurasian_skylark") %>% 
          str_replace("florida_grasshopper_sparrow", "grasshopper_sparrow") %>% 
          str_replace("yellow_wagtail", "western_yellow_wagtail") %>% 
          str_replace("mc_cowns", "mccowns") %>% 
          str_replace("alpine_whinchat", "whinchat") %>% 
          str_replace("thick_billed_longspur", "mccowns_longspur") %>% 
          str_replace("tengmalms", "boreal") %>% 
          str_replace("plain_titmouse", "oak_titmouse") %>% 
          str_replace("swaintsonts_hawk", "swainsons_hawk") %>% 
          str_replace("western_western_", "western_") %>% 
          str_replace("swainsonts", "swainsons")
      ) %>% 
      filter(
        nchar(species) > 1
      )
  )
  

# Write to file:

analysis_subset_list_species_edits %>% 
  iwalk(
    \ (.table, idx) {
      write_csv(
        .table,
        file.path(
          "data/processed/for_analysis", 
          glue::glue("{idx}.csv")
        )
      )
    }
  )

# fix treatment and responses ---------------------------------------------

analysis_treatment_control_lumped <-
  analysis_subset_list_bmp_edits %>% 
  keep_at(
    c("mean_diff", "beta_categorical", "other_categorical")
  ) %>% 
  map_df(
    ~ distinct(
      .x, 
      paper,
      bmp, 
      treatment, 
      control,
      response_var
      # species
    ) %>% 
      # filter(
      #   str_detect(bmp, "remove")
      # ) %>% 
      mutate(
        across(
          c(treatment, control),
          ~ str_to_lower(.x)
        ),
        
        # Lump treatments:
        
        treatment_new =
          case_when(
            
            # Remove non-native species treatments:
            
            str_detect(bmp, "remove") &
              str_detect(
                treatment, 
                "native|treated|removal|spray|low|pris|herbi"
              ) ~ "native or treated",
            
            # Edge and shrub treatments:
            
            str_detect(bmp, "edge") &
              str_detect(treatment, "wide") ~ "edge and shrub habitat",
            
            # Eliminate pesticides treatments:
            
            str_detect(bmp, "pesticides") &
              str_detect(treatment, "organic") ~
              "organic",
            str_detect(bmp, "pesticides") &
              str_detect(treatment, "non-orchard") ~
              "check - non-orchard",
            
            # Install nest boxes treatments:
            
            str_detect(bmp, "nest_boxes") &
              str_detect(treatment, "removal") ~
              "check - remove nest boxes",
            str_detect(bmp, "nest_boxes") &
              str_detect(treatment, "kestrel|owl") ~
              "check - install predator nest boxes",
            str_detect(bmp, "nest_boxes") &
              str_detect(treatment, "install|add|with|^nest box(es)?$") ~
              "install nest boxes",
            
            # Delay hay treatments:
            
            str_detect(bmp, "delay_hay") &
              str_detect(treatment, "late|delay") ~
              "delayed hay",
            str_detect(bmp, "delay_hay") &
              str_detect(treatment, "unmow|pre-haying") ~
              "check - unmowed or before mowing",
            
            # Plant nwsg treatments:
            
            str_detect(bmp, "plant_nwsg") & 
              str_detect(treatment, "native|(n)?wsg|warm|diverse") ~
              "native",
            
            # Fire treatments:
            
            str_detect(bmp, "fire") &
              str_detect(treatment, "time since fire|(post.)?(fire|burn)") &
              str_detect(
                treatment, 
                "current|recent|[0-5]|that year|year of|annual"
              ) ~ 
              "0-5 years post-fire",
            str_detect(bmp, "fire") &
              str_detect(treatment, "time since fire|post.(fire|burn)") &
              str_detect(treatment, "[6-9]") ~ ">5 years post-fire",
            str_detect(bmp, "fire") &
              str_detect(treatment, "burn|fire") ~ "fire management",
            
            # Overwintering habitat treatments:
            
            str_detect(bmp, "overwintering") ~
              "to be lumped",
            
            # Grazing intensity treatments:
            
            str_detect(bmp, "grazing") ~
              "to be lumped",
            
            # Manage in patches treatments:
            
            str_detect(bmp, "patches") ~
              "to be lumped",
            
            # Mow towards refugia treatments:
            
            str_detect(bmp, "refugia") ~
              "to be lumped",
            
            # Keep cats indoors treatments:
            
            str_detect(bmp, "cats") ~
              "to be lumped",
            
            # Summer pasture stockpiling indoors treatments:
            
            str_detect(bmp, "stockpi") ~
              "to be lumped",
            
            # Darksky treatments:
            
            str_detect(bmp, "darks") ~
              "to be lumped",
            .default = "check - other"
          ),
        
        # Lump controls:
        
        control_new = 
          case_when(
            
            # Remove non-native species controls:
            
            str_detect(bmp, "remove") &
              str_detect(
                control, 
                "non-nat|inva|loni|untreat|cont|high|no (s|r)|pseuda"
              ) ~ "non-native or no removal",
            
            # Edge and shrub controls:
            
            str_detect(bmp, "edge") &
              str_detect(control, "non-buff|no border") ~
              "edge and shrub control",
            
            # Eliminate pesticides controls:
            
            str_detect(bmp, "pesticides") &
              str_detect(control, "convention(ion)?al|absence") ~
              "conventional",
            str_detect(bmp, "pesticides") &
              str_detect(control, "orchard") ~
              "check - orchard",
            
            # Install nest boxes controls:
            
            str_detect(bmp, "nest_boxes") &
              str_detect(control, "removal") ~
              "check - before removal of nest boxes",
            str_detect(bmp, "nest_boxes") &
              str_detect(control, "kestrel|owl") ~
              "check - no predator nest boxes",
            str_detect(bmp, "nest_boxes") &
              str_detect(control, "without|natural|no box|pre.*install") ~
              "no nest boxes or natural cavities",
            
            # Delay hay controls:
            
            str_detect(bmp, "delay_hay") &
              str_detect(control, "early|no.*delay") ~
              "early hay",
            str_detect(bmp, "delay_hay") &
              str_detect(control, "continuous") ~
              "check - continuously grazed",
            str_detect(bmp, "delay_hay") &
              str_detect(control, "mowed") ~
              "check - unmowed or before mowing",
            
            # Plant nwsg controls:
            
            str_detect(bmp, "plant_nwsg") & 
              str_detect(control, "nwsg") ~
              "native",
            str_detect(bmp, "plant_nwsg") & 
              str_detect(
                control, 
                "exotic|cool|csg|not nwsg|non-native|non-buff|crop"
              ) ~
              "non-native",
            
            # Overwintering habitat controls:
            
            str_detect(bmp, "overwintering") ~
              "to be lumped",
            
            # Grazing intensity controls:
            
            str_detect(bmp, "grazing") ~
              "to be lumped",
            
            # Manage in patches controls:
            
            str_detect(bmp, "patches") ~
              "to be lumped",
            
            # Mow towards refugia controls:
            
            str_detect(bmp, "refugia") ~
              "to be lumped",
            
            # Keep cats indoors controls:
            
            str_detect(bmp, "cats") ~
              "to be lumped",
            
            # Summer pasture stockpiling controls:
            
            str_detect(bmp, "stockpi") ~
              "to be lumped",
            
            # Darksky controls:
            
            str_detect(bmp, "darks") ~
              "to be lumped",
            .default = "check - other"
          )
      )
  ) #%>% 
# select(!bmp) %>% 
# distinct(bmp, treatment, control, treatment_new) %>% 
# print(n = Inf)

treatment_control_to_check <- 
  analysis_treatment_control_lumped %>% 
  filter(
    treatment_new != "to be lumped",
    control_new != "to be lumped",
    str_detect(treatment_new, "check") | str_detect(control_new, "check"),
    .by = bmp
  ) %>% 
  select(
    paper,
    bmp,
    treatment,
    control,
    treatment_new,
    control_new,
    response_var
  ) %>% 
  arrange(
    bmp
  )

treatment_control_to_check %>% 
  pull(bmp) %>% 
  unique() %>% 
  set_names() %>% 
  map(
    ~ treatment_control_to_check %>% 
      filter(
        str_detect(bmp, .x)
      )
  )

# temp --------------------------------------------------------------------

excel_file <- 
  readxl::excel_sheets("data/raw/bmp_review_analysis_subset.xlsx") %>% 
  set_names() %>% 
  map(
    ~ readxl::read_excel(
      "data/raw/bmp_review_analysis_subset.xlsx",
      sheet = .x
    )
  )

