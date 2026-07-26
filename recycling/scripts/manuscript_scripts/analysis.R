# setup -------------------------------------------------------------------

library(esc)
library(meta)
library(tidyverse)

source("scripts/functions.R")

# Globally assign response variables that have a large enough sample size:

responses <-
  c(
    "abundance", 
    "nest_success",
    "species_richness"
  )

effect_sizes_categorical <-  
  file.path(
    "manuscript",
    "manuscript_processed_data",
    "categorical_effect_sizes.rds"
  ) %>% 
  read_rds() %>% 
  
  # Filter to only usable response variables:
  
  filter(response_var %in% responses) %>% 
  
  # Filter to rows in which there are at least 3 papers for a given BMP and
  # response variable:
  
  filter(
    length(
      unique(key)
    ) >= 3,
    .by = c(bmp, response_var)
  ) %>% 
  
  # Fire issues:
  
  filter(
    !(
      bmp == "Prescribed Fire" &
        str_detect(
          treatment,
          str_c(
            "year of burn",
            "burned that year",
            "current year",
            "<1 growing season",
            "[678] years",
            sep = "|"
          ) 
        )
    )
  ) %>% 
  drop_na(beta)

effect_sizes_categorical %>% 
  write_rds(
    file.path(
      "manuscript",
      "manuscript_processed_data",
      "categorical_effect_sizes_fire_and_n_papers_subset.rds"
    )
  )

# pool effect sizes -------------------------------------------------------

# Calculate pooled effect sizes:

pooled_effects_categorical <- 
  responses %>%
  set_names() %>% 
  map(
    \(response) {
      data_response <-
        effect_sizes_categorical %>% 
        filter(response_var == {{ response }})
      
      metagen(
        TE = beta,
        seTE = se,
        n.e = n,
        studlab = key,
        data = data_response,
        sm = "SMD",
        common = FALSE,
        random = TRUE,
        method.tau = "REML",
        method.random.ci = "HK",
        title = {{ response }}
      )
    }
  )

pooled_effects_categorical %>% 
  write_rds(
    file.path(
      "manuscript",
      "manuscript_processed_data",
      "model_output",
      "pooled_effects_categorical.rds"
    )
  )

# random effects models by bmp --------------------------------------------

bmp_models <-
  pooled_effects_categorical %>% 
  map(
    ~ update(
      .x,
      subgroup = bmp, 
      tau.common = FALSE
    )
  )

bmp_models %>% 
  write_rds(
    file.path(
      "manuscript",
      "manuscript_processed_data",
      "model_output",
      "bmp_models.rds"
    )
  )

# random effects models by bmp and species class --------------------------

# Make a data frame for species class analysis:

effect_sizes_species_class <-
  effect_sizes_categorical %>% 
  drop_na(species_class) %>% 
  
  # Convert the species classes to a long-form data frame:
  
  separate_longer_delim(
    .,
    species_class, 
    delim = ";"
  ) %>% 
  
  # Subset to bmp-species_classes-response_var groupings with at least 3 papers:
  
  filter(
    length(
      unique(key)
    ) >= 3,
    .by = 
      c(
        bmp,
        species_class,
        response_var
      )
  ) %>% 
  
  # Subset to bmp-response_var pairs in which there are at least 2 species
  # classes:
  
  filter(
    length(
      unique(species_class)
    ) >= 2,
    .by = c(bmp, response_var)
  ) %>% 
  
  # Subset to columns of interest:
  
  select(
    key:bmp,
    response_var,
    species_class,
    beta:n,
    se
  )

# Run models:

model_output_species_class <-
  c("abundance", "nest_success") %>% 
  set_names() %>% 
  map(
    \(.response_var) {
      effect_sizes_species_class %>% 
        filter(response_var == .response_var) %>% 
        pull(bmp) %>% 
        unique() %>% 
        sort() %>% 
        set_names() %>% 
        map(
          \(.bmp) {
            
            # Subset to the BMP and response variable of interest:
            
            effect_sizes_species_classes_bmp <- 
              effect_sizes_species_class %>% 
              filter(
                bmp == .bmp,
                response_var == .response_var
              )
            
            # Calculate pooled effect sizes:
            
            pooled_output <-
              metagen(
                TE = beta,
                seTE = se,
                n.e = n,
                studlab = key,
                data = effect_sizes_species_classes_bmp,
                sm = "SMD",
                common = FALSE,
                random = TRUE,
                method.tau = "REML",
                method.random.ci = "HK",
                title = "species_class"
              )
            
            # Output with species_class as the subgroup:
            
            subgroup_output <-
              update(
                pooled_output,
                subgroup = species_class,
                tau.common = FALSE
              )
            
            # Output table:
            
            effects_table <- 
              make_effects_table(subgroup_output) %>% 
              rename(species_class = bmp)
            
            # All output:
            
            lst(
              pooled_output,
              subgroup_output,
              effects_table
            )
          }
        )
    }
  )

# Write files:

effect_sizes_species_class %>% 
  write_rds(
    file.path(
      "manuscript",
      "manuscript_processed_data",
      "effect_sizes_species_class.rds"
    )
  )

model_output_species_class %>% 
  write_rds(
    file.path(
      "manuscript",
      "manuscript_processed_data",
      "model_output",
      "model_output_species_class.rds"
    )
  )

# by species --------------------------------------------------------------

# Generate the effect size table that will be the inputs for the model:

effect_sizes_species <- 
  effect_sizes_categorical %>% 
  
  # Subset to bmp-species-response_var groupings with at least 3 papers:
  
  filter(
    length(
      unique(key)
    ) >= 3,
    .by = 
      c(
        bmp,
        species,
        response_var
      )
  ) %>% 
  
  # Subset to bmp-response_var pairs with at least two species:
  
  filter(
    length(
      unique(species)
    ) >= 2,
    .by = c(bmp, response_var)
  ) %>% 
  
  # Remove lumped species:
  
  filter(
    !str_detect(
      species,
      "all_spe|wintering|artificial"
    )
  ) %>% 
  
  # Subset columns:
  
  select(
    key:bmp,
    response_var:species,
    beta:ucl
  )

# Summary of the effect size table:

effect_sizes_species %>% 
  summarize(
    n_papers = 
      key %>% 
      unique() %>% 
      length(),
    n_estimates = n(),
    .by = 
      c(
        response_var,
        bmp,
        species
      )
  )

# Model running:

model_output_species <-
  c("abundance", "nest_success") %>% 
  set_names() %>% 
  map(
    \(.response_var) {
      effect_sizes_species %>% 
        filter(response_var == .response_var) %>% 
        filter(bmp != "Prescribed Fire") %>% 
        pull(bmp) %>% 
        unique() %>% 
        sort() %>% 
        set_names() %>% 
        map(
          \(.bmp) {
            
            # Subset to the BMP and response variable of interest:
            
            effect_sizes_species_bmp <- 
              effect_sizes_species %>% 
              filter(
                bmp == .bmp,
                response_var == .response_var
              )
            
            # Calculate pooled effect sizes:
            
            pooled_output <-
              metagen(
                TE = beta,
                seTE = se,
                n.e = n,
                studlab = key,
                data = effect_sizes_species_bmp,
                sm = "SMD",
                common = FALSE,
                random = TRUE,
                method.tau = "REML",
                method.random.ci = "HK",
                title = "species_class"
              )
            
            # Output with species_class as the subgroup:
            
            subgroup_output <-
              update(
                pooled_output,
                subgroup = species,
                tau.common = FALSE
              )
            
            # Output table:
            
            effects_table <- 
              make_effects_table(subgroup_output) %>% 
              rename(species_class = bmp)
            
            # All output:
            
            lst(
              pooled_output,
              subgroup_output,
              effects_table
            )
          }
        )
    }
  )

# model_output_species$abundance %>% 
#   names() %>% 
#   map_df(
#     ~ model_output_species %>% 
#       pluck(.x) %>% 
#       pluck("effects_table") %>% 
#       mutate(
#         bmp = .x, 
#         .before = species_class
#       )
#   ) %>% 
#   print(n = Inf)

# Write files:

effect_sizes_species %>% 
  write_rds(
    file.path(
      "manuscript",
      "manuscript_processed_data",
      "effect_sizes_species.rds"
    )
  )

model_output_species %>% 
  write_rds(
    file.path(
      "manuscript",
      "manuscript_processed_data",
      "model_output",
      "model_output_species.rds"
    )
  )

# old below here ----------------------------------------------------------



# Get the list of BMPs used in this analysis:

bmps <-
  effect_sizes_categorical %>% 
  pull(bmp) %>% 
  str_split(pattern = "; ") %>% 
  unlist() %>% 
  unique() %>% 
  sort()

# Convert to a long list:

effect_sizes_categorical_bmp_long <-
  bmps %>% 
  map_df(
    ~ effect_sizes_categorical %>% 
      filter(
        str_detect(bmp, .x)
      ) %>% 
      mutate(bmp = .x)
  ) %>% 
  mutate(
    beta = 
      if_else(
        str_detect(
          response_var,
          str_c(
            "predation",
            "daily mortality",
            "daily nest failure",
            "percent of nests predated",
            sep = "|"
          )
        ),
        -beta,
        beta
      )
  ) %>% 
  filter(key != "pytisvj6") %>% 
  filter(
    length(
      unique(key)
    ) > 3,
    .by = c(response_class, bmp)
  )

# Create a tibble of unique response and response_var combinations:

responses <-
  effect_sizes_categorical_bmp_long %>% 
  mutate(
    response = 
      response_var %>% 
      case_when(
        str_detect(
          .,
          str_c(
            "abundance",
            "density",
            "(nests|pairs)/ha",
            "crowing",
            "singing males",
            "territories",
            "birds (captured|per count)",
            "nests per patch",
            "detection frequency",
            "number of detections",
            sep = "|"
          )
        ) ~ "abundance",
        str_detect(., "diversity|species evenness") ~ "species_diversity",
        str_detect(., "richness|species per field") ~ "species_richness",
        str_detect(., "occupancy") ~ "occupancy",
        str_detect(
          ., 
          str_c(
            "dsr",
            "successful n",
            "fledge_rate",
            "nest surv",
            "nest(ing)? succ",
            "nest pred",
            "depred",
            "daily nest",
            "predation r",
            "surviving to fledging",
            "daily surv",
            "fledging success",
            "daily mort",
            "nests predated",
            sep = "|"
          )
        ) ~ "nest_success",
        str_detect(
          .,
          str_c(
            "(eggs|fledglings) per|number of (offs|fledg)|clutch size|",
            "chicks per female|productivity"
          )
        ) ~ "fecundity",
        str_detect(., "brood para") ~ "brood_parasitism",
        str_detect(
          ., 
          "(male|female|juvenile|non-breeding season) surv"
        ) ~ "survival",
        .default = .
      )
  ) %>% 
  distinct(response, response_var)

# Calculate pooled effect sizes:

pooled_effects_categorical <- 
  responses %>%
  filter(response != "occupancy") %>% 
  pull(response) %>% 
  unique() %>% 
  set_names() %>% 
  map(
    \(response) {
      data_response <-
        effect_sizes_categorical_bmp_long %>% 
        left_join(responses, by = "response_var") %>% 
        filter(response == {{response}})
      
      metagen(
        TE = beta,
        seTE = se,
        n.e = sample_size,
        studlab = key,
        data = data_response,
        sm = "SMD",
        common = FALSE,
        random = TRUE,
        method.tau = "REML",
        method.random.ci = "HK",
        title = response
      )
    }
  )