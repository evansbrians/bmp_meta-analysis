
# setup -------------------------------------------------------------------

library(tidyverse)

bmp_review <- 
  googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1qllmPt3LEFx1PaxXcvYz7fkn05ZaLMs72nNtIrbPnSc/edit?usp=sharing") %>%
  mutate(
    bmp = 
      if_else(
        str_detect(bmp, "pesticides"),
        "Eliminate the use of pesticides, including insecticides and rodenticides",
        bmp
      )
  ) %>% 
  filter(
    bmp != "X",
    !str_detect(bmp, "Dark Sky"),
    !is.na(effect)
  )

bmp_review_mod <-
  bmp_review %>% 
  filter(
    str_detect(bmp, ";")
    ) %>% 
  separate(bmp, into = c("bmp1", "bmp2"), sep = "; ") %>% 
  # select(paper:bmp2) %>% 
  pivot_longer(
    bmp1:bmp2,
    names_to = NULL,
    values_to = "bmp"
  ) %>% 
  bind_rows(
    bmp_review %>% 
      filter(
        !str_detect(bmp, ";")
      )
  ) %>% 
  arrange(paper) %>% 
  filter(predictor_var != "no burn vs. 1-yr postburn")

# Show bmp classes:

bmp_review_mod %>% 
  pull(response_class) %>% 
  unique()

# simple boxplots ---------------------------------------------------------

# Species richness:

bmp_sr_raw <-
  bmp_review_mod %>% 
  filter(response_class == "species richness")

bmp_sr_raw_summary <-
  bmp_sr_raw %>% 
  summarize(
    n_studies = n(),
    .by = bmp
  )

bmp_review_mod %>% 
  filter(
    response_class == "species richness",
  ) %>% 
  mutate(bmp) %>% 
  ggplot() +
  aes(
    x = 
      fct_reorder(
        .f = bmp, 
        .x = effect, 
        .fun = median),
    y = effect) +
  geom_hline(
    yintercept = 0,
    linetype = "dashed") +
  geom_boxplot(fill = "#dcdcdc") + 
  geom_text(
    data = bmp_sr_raw_summary,
    aes(
      x = bmp, 
      y = -1.5, 
      label = n_studies)
  ) +
  coord_flip() +
  labs(
    title = "Effects of BMPs on Species Richness",
    x = "BMP", 
    y = "Effect") +
  theme_classic()

# Abundance:

bmp_abundance_raw <-
  bmp_review_mod %>% 
  filter(response_class == "abundance")

bmp_abundance_raw_summary <-
  bmp_abundance_raw %>% 
  summarize(
    n_studies = n(),
    .by = bmp
  )


bmp_review %>% 
  filter(
    response_class == "abundance",
    !str_detect(bmp, ";"), 
    
    # Remove some outliers that we have to look into more closely:
    
    between(effect, -25, 200)
  ) %>% 
  mutate(bmp) %>% 
  ggplot() +
  aes(
    x = 
      fct_reorder(
        .f = bmp, 
        .x = effect, 
        .fun = median),
    y = effect) +
  geom_hline(
    yintercept = 0,
    linetype = "dashed") +
  geom_boxplot(fill = "#dcdcdc") + 
  geom_text(
    data = bmp_abundance_raw_summary,
    aes(
      x = bmp, 
      y = -5, 
      label = n_studies)
  ) +
  coord_flip() +
  labs(
    title = "Effects of BMPs on Abundance",
    x = "BMP", 
    y = "Effect") +
  theme_classic()

# weighted means ----------------------------------------------------------

# Species richness:

bmp_sr <-
  bmp_review_mod %>% 
  filter(
    response_class == "species richness",
    !is.na(sample_size)
  ) %>% 
  mutate(
    id = row_number()
  ) %>% 
  select(
    id, 
    paper,
    bmp,
    effect,
    sample_size)

bmp_sr_sample_size <-
  bmp_sr$id %>% 
  map(
    \(x) {
      bmp_subset <-
        bmp_sr %>% 
        filter(id == x)
      
      bmp_subset %>% 
        slice(
          rep(
            1:n(), 
            each = bmp_subset$sample_size)
        )
    }
  ) %>% 
  bind_rows()

bmp_sample_sizes <-
  bmp_sr_sample_size %>% 
  distinct() %>% 
  summarize(
    sample_size = sum(sample_size),
    n_studies = n(),
    .by = bmp
  )

bmp_sr_sample_size %>% 
  ggplot() +
  aes(
    x = 
      fct_reorder(
        .f = bmp, 
        .x = effect, 
        .fun = median),
    y = effect) +
  geom_hline(
    yintercept = 0,
    linetype = "dashed") +
  geom_boxplot(fill = "#dcdcdc") + 
  geom_text(
    data = bmp_sample_sizes,
    aes(
      x = bmp, 
      y = -1.5, 
      label = sample_size),
    color = "blue"
  ) +
  coord_flip() +
  labs(
    title = "Effects of BMPs on species richness, weighted by sample size",
    x = "BMP", 
    y = "Effect") +
  theme_classic()

# Abundance

bmp_abundance <-
  bmp_review_mod %>% 
  filter(
    response_class == "abundance",
    !is.na(sample_size)
  ) %>% 
  mutate(
    id = row_number()
  ) %>% 
  select(
    id, 
    paper,
    bmp,
    effect,
    sample_size)

bmp_abundance_sample_size <-
  bmp_abundance$id %>% 
  map(
    \(x) {
      bmp_subset <-
        bmp_sr %>% 
        filter(id == x)
      
      bmp_subset %>% 
        slice(
          rep(
            1:n(), 
            each = bmp_subset$sample_size)
        )
    }
  ) %>% 
  bind_rows()

bmp_abundance_sample_sizes <-
  bmp_abundance_sample_size %>% 
  distinct() %>% 
  summarize(
    sample_size = sum(sample_size),
    n_studies = n(),
    .by = bmp
  )

bmp_abundance_sample_size %>% 
  ggplot() +
  aes(
    x = 
      fct_reorder(
        .f = bmp, 
        .x = effect, 
        .fun = median),
    y = effect) +
  geom_hline(
    yintercept = 0,
    linetype = "dashed") +
  geom_boxplot(fill = "#dcdcdc") + 
  geom_text(
    data = bmp_abundance_sample_sizes,
    aes(
      x = bmp, 
      y = -1.5, 
      label = sample_size),
    color = "blue"
  ) +
  coord_flip() +
  labs(
    title = "Effects of BMPs on abundance, weighted by sample size",
    x = "BMP", 
    y = "Effect") +
  theme_classic()
