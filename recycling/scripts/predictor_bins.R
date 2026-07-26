

# setup -------------------------------------------------------------------

library(tidyverse)

# Read in the processed mean diff data:

mean_diff <- 
  read_rds("data/processed/mean_diff_proc.rds")

# Add in the beta categorical data:

categorical <- 
  mean_diff %>% 
  mutate(
    sheet = "mean_diff",
    .before = paper
  ) %>% 
  bind_rows(
    googlesheets4::read_sheet(
      "https://docs.google.com/spreadsheets/d/14SWR7TXIKNvrYGr2_vwx9xBp5LDrDNaYcqZt6pldSoA/edit?gid=1072800016#gid=1072800016",
      sheet = "beta_categorical"
    ) %>% 
      mutate(
        sheet = "beta_categorical",
        .before = paper
      ) 
  ) %>% 
  mutate(
    across(
      everything(),
      ~ tolower(.x)
    ),
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

categorical_fixed <- 
  categorical %>% 
  filter(
    !str_detect(bmp, ";")
  ) %>% 
  distinct(bmp) %>% 
  pull() %>% 
  map_df(
    ~ categorical %>% 
      filter(
        str_detect(bmp, .x)
      ) %>% 
      mutate(
        bmp = .x
      )
  )


# theme function ----------------------------------------------------------

my_theme <- 
  function() {
    theme(
      text = element_text(family = "Times"),
      axis.title = element_text(size = 16, face = "bold"),
      axis.title.x = element_text(vjust = -1),
      axis.title.y = element_text(vjust = 4),
      plot.title = 
        element_text(
          size = 20, 
          hjust = 0,
          margin = margin(0, 0, 10, 19),
          face = "bold"
        ),
      plot.margin = 
        unit(
          c(10, 10, 12, 18), 
          "pt"
        ),
      axis.text = 
        element_text(
          size = 12, 
          color = "black"
        ),
      plot.title.position = "plot",
      panel.grid.major.y = element_blank(),
      panel.grid.minor.y = element_blank(),
      panel.grid.major.x = element_line(color = "#cccccc"),
      panel.grid.minor.x = element_line(color = "#cccccc", linetype = "dashed"),
      panel.background = element_rect(fill = "#eefeff")
    )
  }

# classify treatments ----------------------------------------------------

# Lumped: Prescribed fire

categorical_fixed %>%
  select(bmp, treatment, control) %>% 
  filter(
    bmp == "prescribed_fire",
    str_detect(treatment, "graz")
  ) %>% 
  distinct() %>% 
  print(n = Inf)

prescribed_fire_classified <- 
  categorical_fixed %>% 
  select(paper, bmp, treatment, control) %>% 
  filter(bmp == "prescribed_fire") %>% 
  mutate(
    treatment_split =
      case_when(
        str_detect(treatment, "annual|<1") &
          str_detect(treatment, "grazed") &
          !str_detect(treatment, "ungrazed") ~ "grazed and burned; <1 year post-fire",
        str_detect(treatment, "1.*post.(fire|burn)") &
          str_detect(treatment, "graz") ~ "grazed and burned; 1 year post-fire",
        str_detect(treatment, "2.*post.(fire|burn)") &
          str_detect(treatment, "graz") ~ "grazed and burned; 2 years post-fire",
        str_detect(treatment, "annual|current|that year|year of (fire|burn)|<1") ~ "<1 year post-fire",
        str_detect(treatment, "time since fire|post.(fire|burn)") &
          str_detect(treatment, "1") ~ "1 year post-fire",
        str_detect(treatment, "time since fire|post.(fire|burn)") &
          str_detect(treatment, "2") ~ "2 years post-fire",
        str_detect(treatment, "time since fire|post.(fire|burn)") &
          str_detect(treatment, "3") ~ "3 years post-fire",
        str_detect(treatment, "time since fire|post.(fire|burn)") &
          str_detect(treatment, "5") ~ "5 years post-fire",
        str_detect(treatment, "time since fire|post.(fire|burn)") &
          str_detect(treatment, "6") ~ "6 years post-fire",
        str_detect(treatment, "time since fire|post.(fire|burn)") &
          str_detect(treatment, "7") ~ "7 years post-fire",
        str_detect(treatment, "time since fire|post.(fire|burn)") &
          str_detect(treatment, "8") ~ "8 years post-fire",
        str_detect(treatment, "2-year burn sites") ~ "2 year burn frequency",
        str_detect(treatment, "4-year burn (frequency|sites)") ~ "4 year burn frequency",
        str_detect(treatment, "patch.*graz") ~ "patch-burn grazed",
        str_detect(treatment, "patch") ~ "patch-burn",
        str_detect(treatment, "graz") ~ "grazed and burned",
        str_detect(treatment, "native") ~ "native",
        str_detect(treatment, "mixed") ~ "mixed grass",
        str_detect(treatment, "idle") ~ "idle fields",
        str_detect(treatment, "crp") ~ "crp fields",
        str_detect(treatment, "roadsides") ~ "roadsides",
        str_detect(treatment, "burn|fire") ~ "burned fields",
        TRUE ~ "other"
      ),
    treatment_class =
      case_when(
        str_detect(treatment, "annual|current|that year|year of (fire|burn)|<1") & 
          str_detect(treatment, "patch") ~ "patch-burn; <1 year post-fire",
        str_detect(treatment, "[12].*post.(fire|burn)") &
          str_detect(treatment, "patch") ~ "patch-burn; 1-2 years post-fire",
        str_detect(treatment, "annual|<1") &
          str_detect(treatment, "grazed") &
          !str_detect(treatment, "ungrazed") ~ "grazed and burned; <1 year post-fire",
        str_detect(treatment, "[12].*post.(fire|burn)") &
          str_detect(treatment, "graz") ~ "grazed and burned; 1-2 years post-fire",
        str_detect(treatment, "annual|current|that year|year of (fire|burn)|<1") ~ "<1 year post-fire",
        str_detect(treatment, "time since fire|post.(fire|burn)") &
          str_detect(treatment, "[12]|2-year burn") ~ "1-2 years post-fire",
        str_detect(treatment, "time since fire|post.(fire|burn)") &
          str_detect(treatment, "[3-5]|4-year burn") ~ "3-5 years post-fire",
        str_detect(treatment, "time since fire|post.(fire|burn)") &
          str_detect(treatment, "[6-8]") ~ "6-8 years post-fire",
        str_detect(treatment, "patch") ~ "patch-burn",
        str_detect(treatment, "graz") ~ "grazed and burned",
        str_detect(treatment, "native|mixed|idle|crp|roadside|burn|fire") ~ "burned fields",
        TRUE ~ "other"
      )#,
    # control_class =
    #   case_when(
    #     str_detect(control, "(un|no(n-|t )? ?)burn|untreat|before|control|idle") ~ "Unburned fields",
    #     str_detect(control, "annual") ~ "Annually burned fields",
    #     str_detect(control, "hay") ~ "Hayed fields",
    #     str_detect(control, "since fire|post.(fire|burn)") &
    #       str_detect(control, "3") ~ "3 years post-fire",
    #     str_detect(control, "burn sites") &
    #       str_detect(control, "4") ~ "4 year burn frequency",
    #     str_detect(control, "burn sites") &
    #       str_detect(control, "10") ~ "10 year burn frequency",
    #     str_detect(control, "burn frequency") &
    #       str_detect(control, "20") ~ "20 year burn frequency",
    #     str_detect(control, "since fire|post.(fire|burn)") &
    #       str_detect(control, "80") ~ ">80 years post-fire",
    #     str_detect(control, "graze-and-burn") ~ "Grazed and burned fields",
    #     str_detect(control, "grazed") ~ "Grazed fields",
    #     str_detect(control, "traditional") ~ "Traditional management",
    #     str_detect(control, "mowed") ~ "Mowed fields",
    #     str_detect(control, "fire") ~ "Burned fields",
    #     TRUE ~ "other"
    #   )
  )

prescribed_fire_fix_treatment_class <- 
  prescribed_fire_classified %>% 
  filter(
    !str_detect(treatment_class, ";")
  ) %>% 
  distinct(treatment_class) %>% 
  pull() %>% 
  map_df(
    ~ prescribed_fire_classified %>% 
      filter(
        str_detect(treatment_class, .x)
      ) %>% 
      mutate(
        treatment_class = .x
      )
  )

prescribed_fire_fix_treatment_class %>% 
  filter(
    !treatment_split == "other",
    !treatment_class == "other",
    # !control_class == "other"
  ) %>% 
  distinct(paper, treatment, control, treatment_class) %>% 
  ggplot() +
  aes(
    x = treatment_class,
    # fill = treatment_split
  ) +
  geom_bar(
    color = "#3d5013",
    fill = "#8cac4c",
    size = .7
  ) +
  scale_y_continuous(
    limits = c(0, 20),
    expand = c(0, 0)
  ) +
  coord_flip() +
  labs(
    title = "Prescribed fire: Treatments",
    x = "Number of papers",
    y = "Classified treatment"
  ) +
  theme_bw() +
  my_theme()

# nwsgs -------------------------------------------------------------------

categorical_fixed %>%
  select(bmp, treatment, control) %>% 
  filter(
    bmp == "plant_nwsg",
    # str_detect(treatment, "strips")
  ) %>% 
  distinct() %>% 
  print(n = Inf)

plant_nwsg_treatment <- 
categorical_fixed %>% 
  select(paper, bmp, treatment, control) %>% 
  filter(bmp == "plant_nwsg") %>% 
  mutate(
    treatment_class =
      case_when(
        str_detect(treatment, "idle") ~ "native idle fields",
        str_detect(treatment, "burn.*graze") ~ "native burned and grazed fields",
        str_detect(treatment, "burned") ~ "native burned fields",
        str_detect(treatment, "grazed") ~ "native grazed fields",
        str_detect(treatment, "strips|buffer") ~ "native filter strips",
        str_detect(treatment, "hay|multi") ~ "hayed native fields",
        str_detect(treatment, "grazed") ~ "native grazed fields",
        str_detect(treatment, "pre-restoration") ~ "pre-restoration",
        TRUE ~ "nwsg sites"
      )
  )


categorical_fixed %>%
  select(bmp, treatment, control) %>% 
  filter(
    bmp == "plant_nwsg",
    # str_detect(treatment, "strips")
  ) %>% 
  distinct(control) %>% 
  print(n = Inf)

categorical_fixed %>% 
  select(paper, bmp, control) %>% 
  filter(bmp == "plant_nwsg") %>% 
  mutate(
    control_class =
      case_when(
        str_detect(control, "idle") ~ "native idle fields",
        TRUE ~ "other"
      )
  ) %>% 
  distinct() %>% 
  print(n = Inf)

plant_nwsg_treatment %>% 
  filter(
    !treatment_class == "other",
    # !control_class == "other"
  ) %>% 
  distinct(paper, treatment, control, treatment_class) %>% 
  ggplot() +
  aes(
    x = treatment_class,
    # fill = treatment_split
  ) +
  geom_bar(
    color = "#3d5013",
    fill = "#8cac4c",
    size = .7
  ) +
  scale_y_continuous(
    limits = c(0, 20),
    expand = c(0, 0)
  ) +
  coord_flip() +
  labs(
    title = "Plant native warm-season grass: Treatments",
    x = "Number of papers",
    y = "Classified treatment"
  ) +
  theme_bw() +
  my_theme()

# edge and shrub ----------------------------------------------------------

categorical_fixed %>%
  select(bmp, treatment) %>% 
  filter(
    bmp == "edge_and_shrub_habitat",
    # str_detect(treatment, "strips")
  ) %>% 
  distinct() %>% 
  print(n = Inf)

# categorical_fixed %>% 
#   select(paper, bmp, treatment) %>% 
#   filter(bmp == "plant_nwsg") %>% 
#   mutate(
#     treatment_class =
#       case_when(
#         str_detect(treatment, "idle") ~ "native idle fields",


# delay hay ---------------------------------------------------------------

# Treatment:

delay_hay_treatment <- 
categorical_fixed %>%
  select(paper, bmp, treatment, control) %>%
  filter(bmp == "delay_hay") %>%
  mutate(
    treatment_class =
      case_when(
        str_detect(treatment, "late|delay") ~ "delayed management",
        str_detect(treatment, "unmow") ~ "no management",
        TRUE ~ "other"
      ),
    control_class =
      case_when(
        str_detect(control, "early|no harvest") ~ "early management",
        str_detect(control, "conti") ~ "continuous management",
        str_detect(control, "mow") ~ "mowed fields",
        TRUE ~ "other"
      )
  )

delay_hay_treatment %>% 
  filter(
    !treatment_class == "other",
    # !control_class == "other"
  ) %>% 
  distinct(paper, treatment, control, treatment_class) %>% 
  ggplot() +
  aes(
    x = treatment_class,
    # fill = treatment_split
  ) +
  geom_bar(
    color = "#3d5013",
    fill = "#8cac4c",
    size = .7
  ) +
  scale_y_continuous(
    limits = c(0, 20),
    expand = c(0, 0)
  ) +
  coord_flip() +
  labs(
    title = "Delay hay: Treatments",
    x = "Number of papers",
    y = "Classified treatment"
  ) +
  theme_bw() +
  my_theme()

# summer pasture stockpiling ----------------------------------------------

# I think these can all be lumped together!

categorical_fixed %>%
  select(paper, bmp, treatment, control) %>%
  filter(bmp == "summer_pasture_stockpiling") %>%
  mutate(
    treatment_class =
      case_when(
        str_detect(treatment, "short|pulse") ~ "short duration grazing",
        str_detect(treatment, "rotation") ~ "rotational grazing",
        TRUE ~ "other"
      ),
    control_class =
      case_when(
        str_detect(control, "season|long") ~ "long duration grazing",
        str_detect(control, "conti|burn") ~ "continuous grazing",
        TRUE ~ "other"
      )
  ) %>% 
  distinct() %>% 
  print(n = Inf)

# eliminate pesticides ----------------------------------------------------

# Treatments and controls can all be lumped together!

categorical_fixed %>%
  select(paper, bmp, treatment, control) %>%
  filter(bmp == "eliminate_pesticides") %>%
  mutate(
    treatment_class =
      case_when(
        str_detect(treatment, "organic|non-orchard") ~ "organic farm",
        TRUE ~ "other"
      ),
    control_class =
      case_when(
        str_detect(control, "convent|orchard") ~ "conventional farm",
        TRUE ~ "other"
      )
  )%>% 
  distinct() %>% 
  print(n = Inf)





