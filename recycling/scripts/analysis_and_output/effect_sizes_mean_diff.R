
# Effect sizes for standardized mean differences

# setup -------------------------------------------------------------------

library(meta)
library(googlesheets4)
library(tidyverse)

# Get citations_by_bmp_long:

papers <- 
  file.path(
    "https://docs.google.com/spreadsheets/d",
    "1Lf3v8fU0sCCAcJ6Wj1v8GwgogjI4ve3xcMlPzLW0hnU"
  ) %>% 
  read_sheet() %>% 
  mutate(
    key = tolower(key),
    bmp = 
      if_else(
        bmp == "no_bmp", 
        "z-no_bmp",
        bmp
      )
  )

# Make vector of bmps:

bmp_vector <-
  papers %>% 
  filter(
    !str_detect(bmp, "^z")
  ) %>% 
  distinct(bmp) %>% 
  pull() %>% 
  sort() %>% 
  set_names()

# Get processed mean difference frame:

mean_diff_start <-
  read_rds("data/processed/mean_diff_proc.rds") %>% 
  
  # Mortality to successful nests:
  
  mutate(
    across(
      c(xbar_e, xbar_c),
      ~ if_else(
        response_class == "nest_success" &
          response_metric == "mortality",
        -.x,
        .x
      )
    )
  ) %>% 
  
  # Temporarily remove gowdy's crazy effect size:
  
  filter(
    !(str_detect(paper, "gowdy") &
       response_class == "abundance")
  ) %>% 
  
  # Temporary species grouping:
  
  mutate(
    species = 
      case_when(
        species == "breeding grassland species" ~ "grassland obligates",
        species == "grassland species" ~ "grassland obligates",
        species == "breeding shrub/scrub species" ~ "shrub species",
        species == "wintering shrub/scrub species" ~ "shrub species",
        species == "wintering species" ~ "all species",
        species == "breeding species" ~ "all species",
        .default = species
      )
  ) %>% 
  
  # Combine predictors:
  
  mutate(
    response_metric = 
      case_when(
        str_detect(
          response_metric,
          "DSR|nest survival|fledge") ~ "nest success",
        .default = response_metric
      )
  ) %>% 
  
  # Reorder the columns:
  
  select(
    bmp, 
    response_class:response_metric,
    species,
    everything()
  ) %>% 
  
  # Remove missing quantitative values:
  
  filter(
    !if_any(
      xbar_e:sd_c,
      is.na
    )
  )

# Group taxa by habitat association:

mean_diff_taxa_grouped <- 
  mean_diff_start %>% 
  filter(
    str_detect(
      species %>% 
        str_remove_all("'") %>% 
        str_replace_all("-", " ") %>% 
        str_replace_all(";", "|") %>% 
        str_to_lower(),
      grassland_obligates
    )
  ) %>% 
  mutate(species = "grassland obligates") %>% 
  bind_rows(
    mean_diff_start %>% 
      filter(
        str_detect(
          species %>% 
            str_remove_all("'") %>% 
            str_replace_all("-", " ") %>% 
            str_replace_all(";", "|") %>% 
            str_to_lower(),
          shrubland_spp
        )
      ) %>% 
      mutate(species = "shrub species")
  )


# Nested version for analysis:

mean_diff_nested <- 
  
  # Combine rames
  
  mean_diff_start %>% 
  bind_rows(mean_diff_taxa_grouped) %>% 
  
  # Make a key column:
  
  unite(
    "key", 
    bmp:species, 
    sep = ";",
    remove = FALSE
  ) %>% 
  
  # Subset to key combinations with more than three papers:
  
  filter(
    length(
      unique(
        paper
      )
    ) > 3,
    .by = key
  ) %>%
  
  # Nest all other variables:
  
  nest(paper_list = paper:sd_c)

# calculate standardized effect sizes by paper ----------------------------

effect_sizes_by_paper <-
  mean_diff_nested %>% 
  pull(key) %>% 
  map(
    \(key){
      
      # Prepare data for effect size calculation:
      
      mean_diff_nested %>% 
        filter(key == {{key}}) %>% 
        unnest(paper_list) %>% 
        transpose() %>% 
        map(
          \(x) {
            
            # Calculate effect sizes:
            
            get_effects(
              paper = x$paper,
              mean_treatment = x$xbar_e,
              mean_control = x$xbar_c,
              n_treatment = x$n_e,
              n_control = x$n_c,
              sd_treatment = x$sd_e,
              sd_control = x$sd_c
            ) %>%
              
              # Wrangle the output:
              
              drop_na(effect_size) %>% 
              mutate(
                key = x$key,
                bmp = x$bmp,
                response_class = x$response_class,
                response_metric = x$response_metric,
                predictor_var = x$predictor_var,
                species = x$species,
                .before = study) %>% 
              select(
                bmp,
                study,
                everything()
              )
          }
        ) %>% 
        list_rbind()
    }
  ) %>% 
  list_rbind() %>% 
  select(
    key,
    everything()
  )

# pool effect sizes -------------------------------------------------------

# Run random effects model:

effect_model <- 
  mean_diff_nested %>% 
  pull(key) %>% 
  unique() %>% 
  set_names() %>% 
  map(
    \(key) {
      mean_diff_nested %>% 
        filter(key == {{key}}) %>% 
        unnest(paper_list) %>% 
        metacont(
          n.e = n_e,
          mean.e = xbar_e,
          sd.e = sd_e,
          n.c = n_c,
          mean.c = xbar_c,
          sd.c = sd_c,
          studlab = paper,
          data = .,
          sm = "SMD",
          method.smd = "Hedges",
          fixed = FALSE,
          random = TRUE,
          method.tau = "REML",
          method.random.ci = "HK",
          title = {{key}}
        )
    }
  )

# Make output table:

effect_model_output <- 
  effect_model %>% 
  names() %>% 
  map(
    \(.x) {
      output <- 
        effect_model %>% 
        pluck(.x)
      
      tibble(
        key = .x,
        # n_studies = output$k.study,
        n_effect_sizes = output$k,
        overall_effect = output$TE.random,
        lower_ci = output$lower.random,
        upper_ci = output$upper.random,
      )
    }
  ) %>% 
  list_rbind() %>% 
  separate(
    key,
    into = 
      c(
        "bmp",
        "response_class",
        "response_metric",
        "taxa"
      ),
    sep = ";",
    remove = FALSE
  ) %>% 
  mutate(
    predictor_response = 
      str_c(
        bmp, 
        " (",
        taxa,
        ")"
      ) %>%
      str_replace_all("_", " ") %>% 
      str_to_title() %>% 
      str_replace_all(" And", " and") %>% 
      str_replace_all("Nwsg", "NWSG") %>% 
      fct_rev(),
    .after = bmp
  ) %>% 
  left_join(
    mean_diff_nested %>% 
      unnest(paper_list) %>% 
      summarize(
        n_studies = 
          length(
            unique(paper)
            ), 
        .by = key),
    by = "key"
  )

effect_model_output %>% 
  write_rds("data/processed/effect_sizes_mean_diff.rds")


