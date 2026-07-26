
# setup -------------------------------------------------------------------

library(esc)
library(meta)
library(tidyverse)

source("scripts/functions.R")

effect_sizes_categorical <-  
  read_rds("data/proc_2025-10-16/categorical_effect_sizes.rds")

# pool effect sizes -------------------------------------------------------

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

# random effects models ---------------------------------------------------

random_effects_output_categorical <-
  pooled_effects_categorical %>% 
  map(
    ~ update(
      .x,
      subgroup = bmp, 
      tau.common = FALSE
    )
  )

# Make effect size tables:

effects_tables <- 
  random_effects_output_categorical %>% 
  map(
    ~ tibble(
      bmp = .x$subgroup.levels,
      n_estimates = .x$k.w,
      effect_size = .x$TE.random.w,
      se = .x$seTE.random.w,
      lcl = .x$lower.random.w,
      ucl = .x$upper.random.w,
      pval = .x$pval.random.w
    ) %>% 
      filter(n_estimates > 1)
  ) %>% 
  keep(~ nrow(.x) > 2)

effect_sizes_categorical_bmp_long %>% 
  filter(response_class == "abundance") %>% 
  summarize(
    n_papers = 
      key %>% 
      unique() %>% 
      length(),
    .by = bmp
  )

data_for_effects_plots <- 
  names(effects_tables) %>% 
  set_names() %>% 
  map(
    \(x) {
      effects_tables %>% 
        pluck(x) %>% 
        left_join(
          effect_sizes_categorical_bmp_long %>% 
            filter(response_class == x) %>% 
            summarize(
              n_studies = 
                key %>% 
                unique() %>% 
                length(),
              .by = bmp
            ),
          by = "bmp"
        ) %>% 
        filter(
          !str_detect(bmp, "darksky"),
          n_studies > 1
        ) %>% 
        mutate(
          bmp =
            bmp %>% 
            str_replace_all("_", " ") %>% 
            str_to_title() %>% 
            str_replace("Nwsg", "NWSG") %>% 
            fct_reorder(effect_size)
        )
    }
  ) %>% 
  keep(
    ~ nrow(.x) > 3
  )

write_rds(
  data_for_effects_plots,
  "data/processed/data_for_effects_plots.rds"
)

effects_plots <-
  data_for_effects_plots %>% 
  names() %>% 
  set_names() %>% 
  map(
    \(.x) {
      data_for_effects_plots %>% 
        pluck(.x) %>% 
        ggplot() +
        aes(
          x = effect_size,
          y = bmp,
          label = n_studies
        ) +
        geom_point(
          size = 1.5
        ) +
        geom_segment(
          aes(
            x = lcl,
            xend = ucl
          ),
          lineend = "round",
          linewidth = 0.60
        ) +
        geom_vline(
          xintercept = 0,
          linetype = "dashed",
          linewidth = 0.25
        ) +
        geom_text(
          aes(x = ucl),
          nudge_x = 0.1,
          size = 3
        ) +
        labs(
          title = .x,
          x = "Pooled standardized effect size",
          y = "Best management practice"
        ) +
        theme(
          panel.background = element_rect(fill = "white"),
          panel.grid.major = 
            element_line(
              color = "#dcdcdc",
              size = 0.25
            ),
          panel.grid.minor.x = 
            element_line(
              linetype = "dashed",
              color = "#dcdcdc",
              size = 0.25
            ),
          axis.line = element_line(color = "black"),
          text = element_text(family = "Times"),
          axis.title = element_text(size = 14),
          plot.title = element_text(size = 18)
        )
    }
  )

# lumping species-habitat associations-------------------------------------

effect_sizes_categorical_w_spp_class <- 
  effect_sizes_categorical_bmp_long %>% 
  drop_na(species) %>% 
  mutate(
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
      )
  ) %>% 
  full_join(
    read_rds("data/processed/species_lump_list.rds") %>% 
      bind_rows() %>% 
      distinct(),
    by = join_by(species == common_name),
    relationship = "many-to-many"
  ) %>% 
  mutate(
    combined_class = 
      if_else(
        is.na(combined_class),
        species,
        combined_class
      )
  ) %>% 
  filter(
    str_detect(combined_class, "_grassland|shrub"),
    response_class != "species_richness"
  ) %>% 
  select(
    key,
    bmp,
    response_class:control,
    bird_class = combined_class,
    beta,
    sample_size,
    se
  )

# Separate into a list where each item is a species-habitat class:

effect_size_list_by_spp_class <-
  effect_sizes_categorical_w_spp_class %>% 
  filter(beta < 20) %>%
  split(.$bird_class) %>% 
  map(
    ~ select(.x, !bird_class)
  )

# Calculate pooled effect size:

pooled_effects_by_spp_class <- 
  effect_size_list_by_spp_class %>% 
  map(
    \(species_class_data) {
      species_class_data %>%
        pull(response_class) %>% 
        unique() %>% 
        set_names() %>% 
        map(
          \(response) {
            data_response <-
              species_class_data %>% 
              filter(response_class == {{response}})
            
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
    }
  )

# Random effects model:

random_effects_output_by_spp_class <- 
  pooled_effects_by_spp_class %>% 
  map(
    \(model_by_spp_class) {
      model_by_spp_class %>% 
        map(
          \(response) {
            update(
              response,
              subgroup = bmp, 
              tau.common = FALSE
            )
          }
        )
    }
  )

# Make effect size table by species class and response variable:

effects_table_by_spp_class_and_response <- 
  random_effects_output_by_spp_class %>% 
  names() %>% 
  set_names() %>% 
  map(
    \(species_class) {
      list_by_response <-
        random_effects_output_by_spp_class %>% 
        pluck(species_class)
      
      # For each response class ...
      
      list_by_response %>% 
        names() %>% 
        map(
          \(response) {
            response_df <-
              list_by_response %>% 
              pluck(response)
            
            # Construct a tibble with the response frame:
            
            tibble(
              bmp = response_df$subgroup.levels,
              species_class = {{species_class}},
              response = {{response}},
              n_estimates = response_df$k.w,
              effect_size = response_df$TE.random.w,
              se = response_df$seTE.random.w,
              lcl = response_df$lower.random.w,
              ucl = response_df$upper.random.w,
              pval = response_df$pval.random.w
            ) %>% 
              filter(n_estimates > 1) %>% 
              distinct()
          }
        ) %>% 
        # keep(~ nrow(.x) > 2) %>% 
        bind_rows() %>% 
        distinct()
    }
  )
