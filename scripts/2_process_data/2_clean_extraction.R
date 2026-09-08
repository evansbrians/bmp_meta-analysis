# This script:
# - Reformats the Google sheets
# - Cleans grouping variables (e.g., BMPs, species)
# - Flags whether a nest-success response is a daily or a period rate
# - Saves each tab as an individual csv file in data/processed.

# set-up ------------------------------------------------------------------

library(tidyverse)

# Project functions:

source("src/functions.R")

# Sentinels that mean missing:

null_tokens <-
  c(
    "",
    "-",
    "na",
    "n/a",
    "none",
    "nan"
  )

# Controlled-vocabulary columns, folded to snake_case:

vocabulary_columns <-
  c(
    "response_class",
    "treatment_control_flag",
    "error_class",
    "link",
    "test_statistic"
  )

# Google sheet url:

url <-
  str_c(
    "https://docs.google.com/spreadsheets/d/",
    "14SWR7TXIKNvrYGr2_vwx9xBp5LDrDNaYcqZt6pldSoA"
  )

# Read the sheets:

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

# Species classification frame:

species_guilds <-
  here::here(
    "data/processed",
    "species_classified_analysis_frame.csv"
  ) %>%
  read_csv(
    col_select =
      c(
        species,
        analysis_class,
        species_include = include
      )
  )

# clean cells -------------------------------------------------------------

analysis_subset_list_cells <-
  analysis_subset_list %>%
  map(
    \(.table) {
      .table %>%
        mutate(
          across(
            where(is.character),
            \(.column) {
              squished <- str_squish(.column)
              if_else(
                str_to_lower(squished) %in% null_tokens,
                NA_character_,
                squished
              )
            }
          )
        ) %>%
        mutate(
          across(
            any_of(vocabulary_columns),
            \(.column) {
              str_to_snake(.column)
            }
          )
        )
    }
  )

# clean error classes -----------------------------------------------------

# Repair the remaining spellings:

analysis_subset_list_bmp_edits <-
  analysis_subset_list_cells %>%
  map(
    \(.table) {
      .table %>%
        separate_longer_delim(bmp, ";") %>%
        mutate(
          error_class =
            error_class %>%
            str_replace_all("confident_intervals", "confidence_intervals") %>%
            str_replace_all("\\bse\\b", "standard_error"),
          bmp =
            bmp %>%
            str_to_lower() %>%
            str_trim() %>%

            # Codes the extraction spells its own way:

            str_replace(
              "^set_aside_adjacent_unmowed$",
              "manage_in_patches"
            ) %>%
            str_replace(
              "^remove_non[-_]native.*$",
              "remove_non_native_shrubs"
            )
        )
    }
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

# clean link functions ----------------------------------------------------

# Coefficient link scales:

link_vocabulary <-
  c(
    "identity",
    "logit",
    "logistic_exposure",
    "log",
    "cloglog",
    "probit"
  )

# add nest survival scale -------------------------------------------------

# Flag daily against period nest survival:

analysis_subset_list_survival_scale <-
  analysis_subset_list_species_edits %>%
  map(
    \(.table) {
      .table %>%
        mutate(
          link =
            if ("link" %in% names(.table)) {
              link
            } else {
              NA_character_
            },
          baseline_survival =
            if ("baseline_survival" %in% names(.table)) {
              as.numeric(baseline_survival)
            } else {
              NA_real_
            },
          beta_is_derived =
            if ("beta_is_derived" %in% names(.table)) {
              beta_is_derived %>%
                as.character() %>%
                str_to_lower() %>%
                case_match(
                  c("yes", "y", "true", "t") ~ TRUE,
                  c("no", "n", "false", "f") ~ FALSE
                )
            } else {
              NA
            }
        ) %>%
        mutate(

          # One unnamed variant:

          link = str_replace(link, "^log_link$", "log")
        ) %>%
        mutate(

          # Logistic exposure is always daily:

          survival_scale =
            case_when(
              response_class != "nest_success" ~ NA_character_,
              link == "logistic_exposure" ~ "daily_survival",
              str_detect(response_var, "[Dd]aily|DSR|dsr|[Mm]ayfield") ~
                "daily_survival",
              .default = "period_survival"
            )
        )
    }
  )

# filtering pass ----------------------------------------------------------

# Keep only the final BMPs:

analysis_subset_list_bmp_filter <-
  analysis_subset_list_survival_scale %>%
  map(
    ~ .x %>%
      filter(
        !bmp %in% c("keep_cats_indoors", "upgrade_to_darksky")
      ) %>%

      # One last BMP to clean:

      mutate(
        bmp =
          if_else(
            bmp == "grazing_intensity",
            "reduce_grazing_intensity",
            bmp
          )
      )
  )

# write to file -----------------------------------------------------------

analysis_subset_list_bmp_filter %>%
  iwalk(
    \ (.table, idx) {
      write_csv(
        .table,
        file.path(
          "data/processed/cleaned_data",
          glue::glue("{idx}.csv")
        )
      )
    }
  )

# clear the environment ----------------------------------------------------

rm(
  list = ls()
)
