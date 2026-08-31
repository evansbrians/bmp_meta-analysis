# This script:
# - Reformats the Google sheets
# - Cleans grouping variables (e.g., BMPs, species)
# - Flags whether a nest-success response is a daily or a period rate
# - Saves each tab as an individual csv file in data/processed.

# set-up ------------------------------------------------------------------

library(tidyverse)

source("scripts/src/functions.R")

# Sentinels that mean missing in the source workbook. "unknown" is not among
# them: in the screening columns it is a real category and is kept.

null_tokens <-
  c(
    "",
    "-",
    "na",
    "n/a",
    "none",
    "nan"
  )

# Columns holding a controlled vocabulary rather than free text. These are
# folded to snake_case so a spelling variant cannot become a new category.

# `key`, `bmp` and the free-text columns are excluded: keys would be split
# on their digits, and bmp is semicolon-delimited.

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

# Read in the species classification frame:

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

# snake_case has already folded the spacing and capitalisation, so what is
# left is an abbreviation and a typo.

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

            # The two codes the extraction still spells its own way:

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

# The scale a reported coefficient sits on. Only these reach the hazard
# scale, so a value outside the list is a silent exclusion.

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

# Period survival S = DSR^d, so a daily rate and a period probability are not
# comparable until the effect-size step puts them on the log hazard scale.

# `link` is the scale a coefficient lives on, `baseline_survival` the control
# level it maps from, `beta_is_derived` whether it was computed or read off.

# All three are created empty where the sheet does not yet carry them.

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

          # snake_case leaves one variant the vocabulary does not name.

          link = str_replace(link, "^log_link$", "log")
        ) %>%
        mutate(

          # A logistic-exposure coefficient is on logit(DSR), so it is daily
          # whatever the response is called.

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

# Ensure only BMPs from the final list are in the document:

analysis_subset_list_bmp_filter <-
  analysis_subset_list_survival_scale %>%
  map(
    ~ .x %>%
      filter(
        !bmp %in% c("keep_cats_indoors", "upgrade_to_darksky")
      ) %>%

      # One final BMP to clean:

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

# clean the environment ---------------------------------------------------

# Everything this script produces is written above; the next script
# reads it back from disk, so nothing is handed on in memory.

rm(list = ls())
