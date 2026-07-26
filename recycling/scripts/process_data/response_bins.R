

# setup -------------------------------------------------------------------

library(tidyverse)

# URL for the google sheet:

sheet_url <- 
  "https://docs.google.com/spreadsheets/d/14SWR7TXIKNvrYGr2_vwx9xBp5LDrDNaYcqZt6pldSoA/edit?gid=1072800016#gid=1072800016"

# Read in original sheet:

bmp_raw <- 
  googlesheets4::sheet_names(sheet_url) %>% 
  map_df(
    ~ googlesheets4::read_sheet(
      sheet_url,
      sheet = .x
    ) %>% 
      mutate(
        sheet = .x,
        .before = paper
      )
  ) %>% 
  mutate(
    across(
      everything(),
      ~ tolower(.x)
    )
  )

# repair -----------------------------------------------------------------

# Change bmps to shortened snake_case version:

bmp_fix_bmps <- 
  bmp_raw %>% 
  mutate(
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
      str_replace("set.*areas", "set_aside_adjacent_unmowed")
  )

# Separate rows with multiple bmps and fix response snake_case:

bmp_fixed <- 
  bmp_fix_bmps %>% 
  filter(
    !str_detect(bmp, ";")
  ) %>% 
  distinct(bmp) %>% 
  pull() %>% 
  map_df(
    ~ bmp_fix_bmps %>% 
      filter(
        str_detect(bmp, .x)
      ) %>% 
      mutate(
        bmp = .x
      )
  ) %>% 
  mutate(
    response_class =
      response_class %>%
      str_replace_all(" ", "_"),
    response_var =
      response_var %>%
      str_replace_all("_", " ")
  )

# classify response variables ---------------------------------------------

bmp_response_classified <- 
  bmp_fixed %>% 
  filter(sheet == "mean_diff") %>%
  mutate(
    response_metric =
      case_when(
        str_detect(
          response_var, 
          "nest density|nests/|nests per|abundance of nests"
        ) ~ "nest density",
        str_detect(
          response_var, 
          "log"
        ) ~ "log abundance",
        str_detect(
          response_var,
          "detect"
        ) ~ "detection",
        str_detect(
          response_var,
          "hatch-year"
        ) ~ "age demographics",
        str_detect(
          response_var, 
          "abundance|density|number|territor|breeding pair|count"
        ) ~ "abundance",
        str_detect(
          response_var, 
          "dsr|daily survival rate"
        ) ~ "DSR",
        str_detect(
          response_var,
          "fledglings per successful nest"
        ) ~ "fledging rate per successful nest",
        str_detect(
          response_var,
          "fledglings per nest|number of nestlings|fledge rate|per female|number of fledglings|productivity"
        ) ~ "fledge rate",
        str_detect(
          response_var,
          "clutch|eggs per"
        ) ~ "clutch size",
        str_detect(
          response_var,
          "mortality|killed|brood loss|brood reduction|predat"
        ) ~ "mortality",
        str_detect(
          response_var,
          "nest failure"
        ) ~ "nest failure",
        str_detect(
          response_var, 
          "successful nests per 100"
        ) ~ "Tara is very very very sad",
        str_detect(
          response_var, 
          "nest survival|[Nn]est.success|success"
        ) ~ "nest survival",
        str_detect(
          response_var, 
          "shannon"
        ) ~ "shannon diversity index",
        str_detect(
          response_var, 
          "diversity"
        ) ~ "diversity",
        str_detect(
          response_var, 
          "evenn?ess"
        ) ~ "evenness",
        str_detect(
          response_var, 
          "species.richness|species per"
        ) ~ "species richness",
        str_detect(
          response_var, 
          "juvenile"
        ) ~ "juvenile survival",
        str_detect(
          response_var, 
          "survival$"
        ) ~ "survival",
        str_detect(
          response_var, 
          "community occupancy"
        ) ~ "community occupancy",
        str_detect(
          response_var, 
          "occupancy"
        ) ~ "occupancy",
        .default = "other"
      )#,
    # response_metric =
    #   case_when(
    #     response_metric %in% 
    #       c(
    #         "DSR",
    #         "Nest survival",
    #         "fledge rate"
    #       ) ~ "nest success",
    #     .default = response_metric
    #   )
  )

# function to generate summary table --------------------------------------

summarize_response_counts <-
  function(
    .data, 
    by_species =  TRUE
  ) {
    .data %>% 
      {
        if(by_species) {
          group_by(
            .,
            response_metric,
            bmp,
            response_class,
            species
          )
        } else {
          group_by(
            .,
            response_metric,
            bmp,
            response_class
          )
        }
      } %>% 
      summarize(
        n = n(),
        n_papers =
          length(
            unique(paper)
          )
      ) %>% 
      ungroup()
  }

# summarize across species ------------------------------------------------

bmp_fixed %>% 
  pull(bmp) %>% 
  unique() %>% 
  set_names() %>% 
  map(
    \(x) {
      .data <- 
        bmp_response_classified %>% 
        filter(bmp == x)
      
      if(nrow(.data) > 1) {
        .data %>% 
          summarize_response_counts(by_species = FALSE) %>% 
          select(
            response_class,
            response_metric,
            n,
            n_papers
          ) %>% 
          arrange(
            response_class,
            response_metric,
            desc(n_papers)
          ) %>% 
          filter(n_papers > 1)
      } else {
        NULL
      }
    }
  )

# summarize by species ----------------------------------------------------

bmp_fixed %>% 
  pull(bmp) %>% 
  unique() %>% 
  set_names() %>% 
  map(
    \(x) {
      .data <- 
        bmp_response_classified %>% 
        filter(bmp == x)
      
      if(nrow(.data) > 1) {
        .data %>% 
          summarize_response_counts(by_species = TRUE) %>% 
          select(
            response_class,
            response_metric,
            species,
            n,
            n_papers
          ) %>% 
          arrange(
            response_class,
            response_metric,
            species,
            desc(n_papers)
          ) %>% 
          filter(n_papers > 1)
      } else {
        NULL
      }
    }
  )

# plot effect sizes -------------------------------------------------------

bmp_response_classified %>% 
  pull(response_class) %>% 
  unique() %>% 
  map(
    ~ bmp_response_classified %>% 
      summarize(
        n_total = n(),
        .by =
          c(
            bmp,
            response_class,
            response_metric
          )
      ) %>% 
      filter(response_class == .x) %>%
      mutate(
        bmp = 
          bmp %>% 
          str_replace_all("_", " ") %>% 
          str_to_title() %>% 
          fct_rev(),
        response_metric = 
          response_metric %>% 
          str_to_title()
      ) %>% 
      ggplot() +
      aes(
        x = bmp,
        y = n_total,
        fill = response_metric
      ) +
      geom_bar(
        stat = "identity",
        color = "black"
      ) +
      coord_flip() +
      scale_y_continuous(
        limits = 
          ~ c(
            0, 
            plyr::round_any(
              max(.x) + 2,
              5
            )
          ),
        expand = c(0, 0)
      ) +
      scale_fill_brewer(palette = "Dark2") +
      labs(
        title = 
          str_c(
            "Response class: ", 
            .x %>% 
              str_replace_all("_", " ") %>% 
              str_to_title()
          ),
        x = "BMP",
        y = "Number of effect size estimates",
        fill = "Response Metric"
      ) +
      theme_bw() +
      theme(
        axis.title = 
          element_text(
            size = 16, 
            family = "times"
          ),
        axis.title.x = element_text(vjust = -1),
        plot.title = 
          element_text(
            size = 20, 
            family = "times"
          ),
        plot.margin = 
          unit(
            c(6, 6, 12, 6), 
            "pt"
          ),
        axis.text = 
          element_text(
            size = 14, 
            family = "times",
            color = "black"
          ),
        legend.title = 
          element_text(
            size = 18, 
            family = "times"
          ),
        legend.text = 
          element_text(
            size = 16, 
            family = "times"
          ),
        legend.key.spacing.y = unit(8, "pt"),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.minor.x = element_line(linetype = "dashed")
      )
  )

# plot number of papers ---------------------------------------------------

bmp_response_classified %>% 
  pull(response_class) %>% 
  unique() %>% 
  map(
    ~ bmp_response_classified %>% 
      summarize(
        n = 
          key %>% 
          unique() %>% 
          length(),
        .by =
          c(
            bmp,
            response_class,
            response_metric
          )
      ) %>% 
      filter(response_class == .x) %>%
      mutate(
        total_n = sum(n),
        .by = bmp
      ) %>% 
      mutate(
        bmp = 
          bmp %>% 
          str_replace_all("_", " ") %>% 
          str_to_title() %>% 
          fct_rev(),
        response_metric = 
          response_metric %>% 
          str_to_title()
      ) %>% 
      ggplot() +
      aes(
        x = bmp,
        y = n,
        fill = response_metric
      ) +
      geom_bar(
        stat = "identity",
        color = "black"
      ) +
      coord_flip() +
      scale_y_continuous(
        limits = 
          ~ c(
            0, 
            plyr::round_any(
              max(.x) + 2,
              5
            )
          ),
        expand = c(0, 0)
      ) +
      scale_fill_brewer(palette = "Dark2") +
      labs(
        title = 
          str_c(
            "Response class: ", 
            .x %>% 
              str_replace_all("_", " ") %>% 
              str_to_title()
          ),
        x = "BMP",
        y = "Number of papers",
        fill = "Response Metric"
      ) +
      theme_bw() +
      theme(
        axis.title = 
          element_text(
            size = 16, 
            family = "times"
          ),
        axis.title.x = element_text(vjust = -1),
        plot.title = 
          element_text(
            size = 20, 
            family = "times"
          ),
        plot.margin = 
          unit(
            c(6, 6, 12, 6), 
            "pt"
          ),
        axis.text = 
          element_text(
            size = 14, 
            family = "times",
            color = "black"
          ),
        legend.title = 
          element_text(
            size = 18, 
            family = "times"
          ),
        legend.text = 
          element_text(
            size = 16, 
            family = "times"
          ),
        legend.key.spacing.y = unit(8, "pt"),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.minor.x = element_line(linetype = "dashed")
      )
  )
