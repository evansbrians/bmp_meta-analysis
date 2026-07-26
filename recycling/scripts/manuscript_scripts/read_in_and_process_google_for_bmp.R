# Read in and process the data from Google Drive

# setup -------------------------------------------------------------------

library(googlesheets4)
library(esc)
library(meta)
library(tidyverse)

source("scripts/functions.R")

# Read in effect size tables:

effect_size_tables_google <-
  read_effect_size_tables()

# Data frames for lumping:

species_lumped <-
  read_rds("data/proc_2025-10-16/species_lumped.rds")

response_vars_lumped <-
  read_rds("data/proc_2025-10-16/response_vars_lumped.rds")

bmps_lumped <-
  read_rds("data/proc_2025-10-16/bmp_lumped.rds")

# pre-processing ----------------------------------------------------------

# Add lumped variable values:

effect_size_tables_lumped <- 
  effect_size_tables_google %>% 
  map(
    ~ .x %>% 
      separate_longer_delim(bmp, delim = ";") %>% 
      
      # Bring in the lumping tables:
      
      left_join(bmps_lumped, by = "bmp") %>% 
      left_join(
        response_vars_lumped, 
        by = c("response_class", "response_var")
      ) %>% 
      left_join(species_lumped, by = "species") %>% 
      
      # Replace old variable values with the new values:
      
      mutate(
        bmp = bmp_new,
        response_var = response_new,
        species = species_new,
        .keep = "unused"
      ) %>% 
      relocate(
        species_class = classification,
        .after = species
      ) %>% 
      
      # Remove columns that we no longer need:
      
      select(
        !c(
          paper,
          response_class,
          notes
        )
      )
  )

effect_size_tables_lumped %>% 
  write_rds(
    file.path(
      "manuscript",
      "manuscript_processed_data",
      "effect_size_tables_lumped.rds"
    )
  )

# fix error estimates for beta categorical output -------------------------

beta_categorical_list_by_error <-
  effect_size_tables_lumped %>% 
  pluck("beta_categorical") %>% 
  split(.$error_class)

beta_categorical_list_by_error$standard_deviation <- 
  beta_categorical_list_by_error %>% 
  pluck("standard_deviation") %>% 
  mutate(
    se = sd / sqrt(n)
  )

# Get standard deviation and standard error from the confidence intervals and
# sample sizes:

beta_categorical_list_by_error$confidence_intervals <- 
  beta_categorical_list_by_error %>% 
  pluck("confidence_intervals") %>% 
  mutate(
    lcl = 
      if_else(
        is.na(lcl) &
          !is.na(lcl_e),
        lcl_e,
        lcl
      ),
    ucl = 
      if_else(
        is.na(ucl) &
          !is.na(ucl_e),
        ucl_e,
        ucl
      ),
    sd = 
      confint_to_sd(
        lower_cl = lcl,
        upper_cl = ucl,
        n = n
      ),
    se = sd / sqrt(n)
  )

# Combine:

beta_categorical_effect_sizes <- 
  beta_categorical_list_by_error %>% 
  map_df(
    ~ .x %>% 
      select(
        key,
        bmp:species_class,
        sign,
        beta,
        n:ucl
      ) %>% 
      select(
        !matches("cl_[ec]$|s[de]_[ec]$"),
      )
  )

beta_categorical_effect_sizes %>% 
  write_rds(
    file.path(
      "manuscript",
      "manuscript_processed_data",
      "beta_categorical_effect_sizes.rds"
    )
  )

# fix error estimates for mean diff output --------------------------------

# Get mean diff table, split into a list where each item is the error class:

mean_diff_list_by_error <- 
  effect_size_tables_lumped %>% 
  pluck("mean_diff") %>% 
  split(.$error_class)

# Calculate standard deviation and standard error for confidence interval data:

mean_diff_list_by_error$confidence_intervals <-
  mean_diff_list_by_error$confidence_intervals %>% 
  mutate(
    sd_e = 
      confint_to_sd(
        lower_cl = lcl_e,
        upper_cl = ucl_e,
        n = n_e
      ),
    sd_c = 
      confint_to_sd(
        lower_cl = lcl_c,
        upper_cl = ucl_c,
        n = n_c
      ),
    se_e = sd_e / sqrt(n_e),
    se_c = sd_e / sqrt(n_c)
  )

# Calculate the standard error for standard deviation data:

mean_diff_list_by_error$standard_deviation <- 
  mean_diff_list_by_error$standard_deviation %>% 
  mutate(
    se_e = sd_e / sqrt(n_e),
    se_c = sd_c / sqrt(n_c)
  )

# Calculate the standard deviation for standard error data:

mean_diff_list_by_error$standard_error <- 
  mean_diff_list_by_error$standard_error %>% 
  mutate(
    sd_e = se_e * sd(n_e),
    sd_c = se_c * sd(n_c)
  )

# Combine and maintain the columns of interest:

mean_diff_combined <- 
  mean_diff_list_by_error %>% 
  map_df(
    ~ .x %>% 
      select(
        !c(
          error_class,
          lcl_e:ucl_e,
          lcl_c:ucl_c
        )
      )
  ) %>% 
  
  # Dropping NA associated with missing sample size from Monroe 2016
  
  drop_na(n_e)

# get effect sizes for mean diff table ------------------------------------

mean_diff_effect_sizes <- 
  
  # Mean diff effect sizes:
  
  1:nrow(mean_diff_combined) %>% 
  map_df(
    \(i) {
      
      # Make one row tibble:
      
      df <-
        mean_diff_combined %>% 
        slice(i)
      
      # Calculate effect size:
      
      esc_mean_sd(
        grp1m = df$xbar_e, 
        grp2m = df$xbar_c, 
        grp1sd = df$sd_e, 
        grp2sd = df$sd_c, 
        grp1n = df$n_e, 
        grp2n = df$n_c,
        study = df$key
      ) %>% 
        as_tibble()
    }
  ) %>% 
  select(
    beta = es,
    n = sample.size,
    se,
    lcl = ci.lo,
    ucl = ci.hi
  ) %>% 
  
  # Add standard deviation:
  
  mutate(
    sd = se * sqrt(n),
    .after = n
  ) %>% 
  
  # Add character columns:
  
  bind_cols(
    mean_diff_combined %>% 
      select(
        key,
        bmp,
        treatment:species_class, 
        sign
      ),
    .
  )

mean_diff_effect_sizes %>% 
  write_rds(
    file.path(
      "manuscript",
      "manuscript_processed_data",
      "mean_diff_effect_sizes.rds"
    )
  )

# combine beta categorical and mean diff ----------------------------------

bind_rows(
  beta_categorical_effect_sizes,
  mean_diff_effect_sizes
) %>% 
  mutate(
    beta = beta * sign,
    .keep = "unused"
  ) %>% 
  write_rds(
    file.path(
      "manuscript",
      "manuscript_processed_data",
      "categorical_effect_sizes.rds"
    )
  )

# old below here ----------------------------------------------------------



# get and pre-process effect size tables ----------------------------------

effect_size_tables <- 
  effect_size_tables_google %>% 
  map(
    ~ .x %>% 
      mutate(
        
        # Fix BMP naming convention:
        
        bmp =
          bmp %>% 
          str_replace_all(" ", "_") %>% 
          str_replace_all(";_", "; ") %>% 
          str_replace("plant.*(wildflowers|nwsgs)", "plant_nwsg") %>% 
          str_remove("_planting") %>% 
          str_remove("the_use_of_") %>% 
          str_remove(",_including_insecticides_and_rodenticides") %>% 
          str_remove("your_first_cutting_of_") %>% 
          str_remove("fields_") %>% 
          str_replace("_all_", "_")%>% 
          str_replace("set.*areas", "set_aside_adjacent_unmowed"),
        
        # Fix response_class naming convention:
        
        response_class = 
          response_class %>% 
          str_replace_all(" ", "_"),
        
        # Fix species naming convention:
        
        species = 
          species %>% 
          str_replace_all("[- ]", "_") %>% 
          str_remove_all("'") %>% 
          str_replace("florida_g", "g") %>% 
          case_when(
            str_detect(., "facultative") ~ "facultative_grassland",
            str_detect(., "indigo_bunting;_blue_grosbeak") ~ "shrub",
            str_detect(., "shrub|edge") ~ "shrub",
            str_detect(., "grassland|obligate") ~ "obligate_grassland",
            .default = .
          ),
        
        # Fix error class:
        
        error_class = 
          case_when(
            str_detect(error_class, "dev") ~ "standard_deviation",
            str_detect(error_class, "^se$|err") ~ "standard_error",
            str_detect(error_class, "^conf") ~ "confidence_intervals",
            .default = error_class
          ),
        
        # Numeric columns should be numeric:
        
        across(
          matches(
            "^xbar|beta|^.*n_?[ec]?$|se(_[ec])?$|sd|[ul]cl|df|value"
          ),
          ~ as.numeric(.x)
        )
      ) %>% 
      
      # Add in species classification:
      
      full_join(
        species_classes,
        by = join_by(species == common_name)
      ) %>% 
      select(
        key:species,
        species_class,
        everything()
      )
  )

effect_size_tables %>% 
  write_rds("data/proc_2025-10-16/effect_size_tables.rds")

# process beta categorical ------------------------------------------------



# process mean diff -------------------------------------------------------



# Calculate the effect size:



mean_diff_effect_sizes %>% 
  write_rds("data/proc_2025-10-16/mean_diff_effect_sizes.rds")

# Add beta_categorical effect sizes:

categorical_effect_sizes <- 
  mean_diff_effect_sizes %>% 
  bind_rows(
    beta_categorical_effect_sizes %>% 
      rename(sample_size = n) %>% 
      select(!sd)
  )

categorical_effect_sizes %>% 
  write_rds("data/proc_2025-10-16/categorical_effect_sizes.rds")
