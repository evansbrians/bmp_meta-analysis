# Custom functions for meta-analysis

# utility functions -------------------------------------------------------

# Length of unique values in a vector:

length_unique <- 
  function(.x) {
    length(
      unique(.x)
    )
  }

# Standard error:

se <-
  function(.x) {
    sd(.x) / 
      sqrt(
        length(.x)
      )
  }

# Confidence interval to standard deviation:

confint_to_sd <-
  function(
    lower_cl,
    upper_cl,
    n) {
    abs(upper_cl - lower_cl)/3.92 * sqrt(n)
  }

# Standard error to standard deviation:

se_to_sd <-
  function(se, n) {
    se * sqrt(n)
  }

# Standard deviation to standard error:

sd_to_se <-
  function(sd, n) {
    sd / sqrt(n)
  }

# read the Google Sheet ---------------------------------------------------

# Read in effect size tables, with a little bit of cleaning:

read_effect_size_tables <-
  function() {
    sheet_url <- 
      file.path(
        "https://docs.google.com/spreadsheets/d",
        "14SWR7TXIKNvrYGr2_vwx9xBp5LDrDNaYcqZt6pldSoA"
      )
    
    sheet_url %>% 
      googlesheets4::sheet_names() %>% 
      
      # Keep only tables in which there is error:
      
      keep(
        ~ !str_detect(.x, "^other")
      ) %>% 
      
      # Set table names and iterate across each:
      
      set_names() %>% 
      map(
        
        # Read in the sheet:
        
        ~ googlesheets4::read_sheet(
          sheet_url,
          sheet = .x
        ) %>% 
          
          # Fix names:
          
          janitor::clean_names() %>% 
          
          # Remove blank columns:
          
          select(
            !matches("^x[0-9]")
          ) %>% 
          
          # Add the name of the sheet to the data frame after key:
          
          mutate(
            sheet = .x,
            .after = key
          ) %>% 
          
          # Change character columns to lower case:
          
          mutate(
            across(
              where(is.character),
              ~ tolower(.x)
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
          )
      )
  }

# Get standard deviation: -------------------------------------------------

get_sd <-
  function(
    error_class,
    se = NULL,
    lower_cl = NULL,
    upper_cl = NULL,
    n = NULL,
    stdev = NULL
  ) {
    switch(
      error_class,
      confidence_intervals = confint_to_sd(lower_cl, upper_cl, n),
      standard_error = se_to_sd(se, n),
      standard_deviation = stdev,
      none = 0
    )
  }

# Add standard deviations to a table --------------------------------------

# This only works with the specific column names in the data!

add_sd_to_table <-
  function(.data) {
    rowwise(.data) %>% 
      mutate(
        
        # Standard deviation of treatment:
        
        sd_e = 
          get_sd(
            error_class = error_class,
            se = se_e,
            lower_cl = lcl_e,
            upper_cl = ucl_e,
            n = n_e,
            stdev = sd_e
          ),
        
        # Standard deviation of control:
        
        sd_c = 
          get_sd(
            error_class = error_class,
            se = se_c,
            lower_cl = lcl_c,
            upper_cl = ucl_c,
            n = n_c,
            stdev = sd_c
          )
      ) %>% 
      ungroup()
  }

# Function to get standardized effect sizes -------------------------------

get_meta <-
  function(
    mean_treatment,
    n_treatment,
    sd_treatment,
    mean_control,
    n_control,
    sd_control
  ) {
    metacont(
      n.e = n_treatment,
      mean.e = mean_treatment,
      sd.e = sd_treatment,
      n.c = n_control,
      mean.c = mean_control,
      sd.c = sd_control,
      sm = "SMD",
      method.smd = "Cohen"
    )
  }

get_effects <-
  function(
    paper,
    mean_treatment,
    n_treatment,
    sd_treatment,
    mean_control,
    n_control,
    sd_control
  ) {
    metacont(
      n.e = n_treatment,
      mean.e = mean_treatment,
      sd.e = sd_treatment,
      n.c = n_control,
      mean.c = mean_control,
      sd.c = sd_control,
      sm = "SMD",
      method.smd = "Cohen"
    ) %>% 
      as_tibble() %>% 
      janitor::clean_names() %>%
      transmute(
        study = paper,
        effect_size = te_common,
        n_treatment = n_e,
        n_control = n_c,
        sd_treatment = sd_e,
        sd_control = sd_c,
        lower,
        upper,
        sd_total = se_te_common,
        pval
      )
  }

# Hand-calculated standard mean difference --------------------------------

std_mean_difference <-
  function(
    mean_response,
    mean_control,
    n_response,
    n_control,
    s_response,
    s_control
  ) {
    n_k <- n_response + n_control
    part_1 <-
      1 - 3 / (4 * n_k)
    part_2_numerator <-
      mean_response - mean_control
    
    part_2_denominator <-
      sqrt(
        ((n_response - 1) * s_response^2 +
           (n_control - 1) * s_control^2) /(n_k - 2)
      )
    part_1 * part_2_numerator/part_2_denominator
  }

# pooled standard deviation -----------------------------------------------

sd_pooled <-
  function(
    n_1,
    s_1,
    n_2,
    s_2
  ) {
    sqrt(
      (
        (
          (n_1-1)*s_1^2) + 
          (
            (n_2-1)*s_2^2
          )
      ) /
        (
          (n_1-1) + (n_2-1)
        )
    )
  }

# make effects table from metagen output ----------------------------------

make_effects_table <- 
  function(metagen_output) {
    tibble(
      bmp = metagen_output$subgroup.levels,
      n_estimates = metagen_output$k.w,
      effect_size = metagen_output$TE.random.w,
      se = metagen_output$seTE.random.w,
      lcl = metagen_output$lower.random.w,
      ucl = metagen_output$upper.random.w,
      `tau^2` = metagen_output$tau2.w,
      `tau` = metagen_output$tau.w,
      `q` = metagen_output$Q.w,
      `i^2` = metagen_output$I2.w,
      pval = metagen_output$pval.random.w
    )
  }

# functions for tables and plots ------------------------------------------

format_output_table <-
  function(
    output_subset,
    sig_figs = 2
  ) {
    output_subset %>% 
      mutate(
        
        # Round digits:
        
        across(
          effect_size:pval,
          ~ round(.x, digits = sig_figs)
        ),
        
        # Fix names:
        
        across(
          response_metric:bmp,
          ~ .x %>% 
            str_replace("_", " ") %>% 
            str_to_title() %>% 
            str_replace("Nwsgs", "NWSG") %>% 
            str_replace("And", "and") %>% 
            str_replace("In ", "in ") %>% 
            str_replace("-N", "-n")
        ),
        
        # Add plus or minus symbol:
        
        beta =
          effect_size %>% 
          str_c(" \u00b1", se),
        
        # Confidence intervals:
        
        `95% CI` = 
          str_c(
            "(",
            lcl,
            ", ",
            ucl,
            ")"
          ),
        .after = n_estimates,
        .keep = "unused"
      ) %>% 
      
      # Rename columns:
      
      rename(
        K = n_estimates,
        "\u03b2 \u00b1SE" = beta,
        "\u03c4^2" = `tau^2`,
        "\u03c4" = tau,
        Q = q,
        `I^2` = `i^2`,
        p = pval
      )
  }

# Function to fix bmp names

fix_bmp <-
  function(.bmp) {
    .bmp %>% 
      str_to_title() %>% 
      str_replace("Nwsgs", "NWSG") %>% 
      str_replace("And", "and") %>% 
      str_replace("In ", "in ") %>% 
      str_replace("-N", "-n") %>% 
      str_replace("Buffer", "Buffers") %>% 
      str_remove(" Plantings$")
  }

# Function to fix response variable names:

fix_response <- 
  function(.response_name) {
    .response_name %>% 
      str_replace("_", " ") %>% 
      str_to_title()
  }

# Function to fix species names:

fix_species <-
  function(.species_name) {
    .species_name %>% 
      str_to_lower() %>% 
      str_replace("red_wing", "red-wing") %>% 
      str_replace("brown_head", "brown-head") %>%
      str_replace("ring_neck", "ring-neck") %>% 
      str_replace("yellow_brea", "yellow-brea") %>% 
      str_replace_all("_", " ") %>% 
      str_replace("henslows", "Henslow's") %>% 
      str_to_sentence()
  }

# Theme for manuscript plots:

my_manuscript_theme <- 
  function() {
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
      plot.title = element_text(size = 18),
      plot.title.position = "plot"
    )
  }

my_theme_today <-
  function() {
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
      axis.title = element_text(size = 12),
      plot.title = element_text(size = 16),
      axis.text = element_text(size = 8),
      panel.spacing = unit(.30, "lines"),
      panel.border = 
        element_rect(
          color = "black", 
          fill = NA, 
          size = 0.5
        ),
      strip.background = element_rect(color = "black", size = 1)
    ) 
  }

# Format the names of the BMPs, species labels, and response classes for
# plotting and tables:

format_labels <-
  function(.data) {
    .data %>% 
      mutate(
        bmp = 
          bmp %>% 
          str_replace_all("_", " ") %>% 
          str_to_title() %>% 
          str_replace("And", "&") %>% 
          str_replace("-N", "-n") %>% 
          str_replace("wsg", "WSG") %>%
          str_replace(" In ", " in "),
        # species_class = 
        #   species_class %>% 
        #   str_replace_all("_", " ") %>% 
        #   str_to_title(),
        response = 
          response %>% 
          str_replace_all("_", " ") %>% 
          str_to_title()
      )
  }

