# Purpose: Every named function the project uses, for the cleaning scripts in
# scripts/pre_proc and the analysis in scripts/analysis alike. No numbered
# script defines a function of its own.

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

# Function to clean common names:

fix_common_names <-
  function(.common_name) {
    .common_name %>% 
      str_replace_all("-", " ") %>% 
      str_remove_all("'") %>% 
      str_to_snake()
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
      mutate(
        .keep = "none",
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
      panel.background =
        element_rect(fill = "white"),
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
      axis.line =
        element_line(color = "black"),
      text =
        element_text(family = "Times"),
      axis.title =
        element_text(size = 14),
      plot.title =
        element_text(size = 18),
      plot.title.position = "plot"
    )
  }

my_theme_today <-
  function() {
    theme(
      panel.background =
        element_rect(fill = "white"),
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
      axis.line =
        element_line(color = "black"),
      text =
        element_text(family = "Times"),
      axis.title =
        element_text(size = 12),
      plot.title =
        element_text(size = 16),
      axis.text =
        element_text(size = 8),
      panel.spacing = unit(.30, "lines"),
      panel.border = 
        element_rect(
          color = "black", 
          fill = NA, 
          size = 0.5
        ),
      strip.background =
        element_rect(
          color = "black",
          size = 1
        )
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


# utility ------------------------------------------------------------------

# tables handed between scripts ---------------------------------------------

# Path of one table in the plain-text store handed between scripts.

bmp_table_file <-
  function(table_name) {
    file.path(
      "brian_sandbox/data/db_mirror",
      str_c(table_name, ".csv")
    )
  }

bmp_write_table <-
  function(
    .data,
    table_name) {
    write_csv(
      .data,
      bmp_table_file(table_name),
      na = ""
    )
    invisible(.data)
  }

# Columns the current effect-size table must carry. A table missing any of
# them was written by an earlier version of 1_effect_sizes.R.

effect_size_columns <-
  c(
    "es_id",
    "key",
    "bmp",
    "response_metric",
    "response_scale",
    "guild",
    "label_type",
    "species_key",
    "yi",
    "sei",
    "in_primary_pool"
  )

bmp_read_table <-
  function(
    table_name,
    required_columns = NULL) {
    table_file <- bmp_table_file(table_name)
    if (!fs::file_exists(table_file)) {
      cli::cli_abort("No table written yet for {table_name}.")
    }
    table_data <-
      read_csv(
        table_file,
        show_col_types = FALSE
      )
    present_columns <- names(table_data)
    missing_columns <-
      setdiff(required_columns, present_columns)
    if (length(missing_columns) > 0) {
      cli::cli_abort(
        "Stale {table_name} table, missing {missing_columns}. \\
         Re-run {.file 1_effect_sizes.R}."
      )
    }
    table_data
  }

# error conversion ---------------------------------------------------------

# SD implied by a 95% confidence interval.

confint_to_sd <-
  function(
    lower_cl,
    upper_cl,
    n) {
    abs(upper_cl - lower_cl) / 3.92 *
      sqrt(n)
  }

# SE implied by a 95% confidence interval.

confint_to_se <-
  function(
    lower_cl,
    upper_cl) {
    abs(upper_cl - lower_cl) / 3.92
  }

se_to_sd <-
  function(
    se,
    n) {
    se * sqrt(n)
  }

sd_to_se <-
  function(
    sd,
    n) {
    sd / sqrt(n)
  }

# effect sizes -------------------------------------------------------------

# Hedges' small-sample correction factor J.

hedges_correction <-
  function(df_total) {
    1 - 3 / (4 * df_total - 1)
  }

sd_pooled <-
  function(
    n_e,
    sd_e,
    n_c,
    sd_c) {
    numerator <-
      (n_e - 1) * sd_e^2 + (n_c - 1) * sd_c^2
    sqrt(
      numerator / (n_e + n_c - 2)
    )
  }

# Tibble of yi, sei and n_total from two group means.

g_from_group_means <-
  function(
    xbar_e,
    sd_e,
    n_e,
    xbar_c,
    sd_c,
    n_c) {
    s_pooled <-
      sd_pooled(
        n_e = n_e,
        sd_e = sd_e,
        n_c = n_c,
        sd_c = sd_c
      )
    cohens_d <- (xbar_e - xbar_c) / s_pooled
    correction <-
      hedges_correction(
        df_total = n_e + n_c - 2
      )
    variance_d <-
      (n_e + n_c) / (n_e * n_c) +
      cohens_d^2 / (2 * (n_e + n_c))
    tibble(
      yi = correction * cohens_d,
      sei =
        sqrt(
          correction^2 * variance_d
        ),
      n_total = n_e + n_c
    )
  }

# Tibble of yi, sei and n_total from a two-level coefficient.

g_from_categorical_beta <-
  function(
    beta,
    se,
    n_total,
    n_e = NA_real_,
    n_c = NA_real_) {
    groups_known <-
      !is.na(n_e) &
      !is.na(n_c)
    t_statistic <- beta / se
    cohens_d <-
      if_else(
        groups_known,
        t_statistic * sqrt(1 / n_e + 1 / n_c),
        2 * t_statistic / sqrt(n_total - 2)
      )
    n_e_used <-
      if_else(
        groups_known,
        n_e,
        n_total / 2
      )
    n_c_used <-
      if_else(
        groups_known,
        n_c,
        n_total / 2
      )
    correction <-
      hedges_correction(
        df_total = n_e_used + n_c_used - 2
      )
    variance_d <-
      (n_e_used + n_c_used) / (n_e_used * n_c_used) +
      cohens_d^2 / (2 * (n_e_used + n_c_used))
    tibble(
      yi = correction * cohens_d,
      sei =
        sqrt(
          correction^2 * variance_d
        ),
      n_total = n_e_used + n_c_used
    )
  }

# modelling ----------------------------------------------------------------

# Drops cells below the per-metric effect-size and study minima.

# A cell is modelled only above both floors. Nest success is thinly sampled,
# so its study floor is relaxed.

inclusion_thresholds <-
  tribble(
    ~ response_metric, ~ metric_min_effect_sizes, ~ metric_min_studies,
    "abundance", 3L, 3L,
    "species_richness", 3L, 3L,
    "nest_success", 3L, 2L
  )

# Sampler settings. The seed fixes every fit in the analysis, primary and
# sensitivity alike.

sampler_settings <-
  list(
    chains = 2,
    iter = 8000,
    warmup = 2000,
    cores = 2,
    adapt_delta = 0.99,
    max_treedepth = 12,
    backend = "rstan",
    seed = 20260726
  )

apply_inclusion_thresholds <-
  function(
    .data,
    grouping_vars,
    thresholds = inclusion_thresholds,
    min_effect_sizes = 3,
    min_studies = 3) {
    if (!"response_metric" %in% names(.data)) {
      cli::cli_abort(
        "{.fn apply_inclusion_thresholds} needs a response_metric column."
      )
    }
    .data %>%
      left_join(
        thresholds %>%
          select(
            response_metric,
            metric_min_effect_sizes,
            metric_min_studies
          ),
        by = "response_metric"
      ) %>%
      mutate(
        metric_min_effect_sizes =
          replace_na(
            metric_min_effect_sizes,
            min_effect_sizes
          ),
        metric_min_studies =
          replace_na(
            metric_min_studies,
            min_studies
          )
      ) %>%
      filter(
        n() >= first(metric_min_effect_sizes),
        n_distinct(key) >= first(metric_min_studies),
        .by = all_of(grouping_vars)
      ) %>%
      select(
        !c(
          metric_min_effect_sizes,
          metric_min_studies
        )
      )
  }

# Adds meets_primary_threshold.

flag_primary_threshold <-
  function(
    .data,
    min_effect_sizes = 3,
    min_studies = 3) {
    .data %>%
      mutate(
        meets_primary_threshold =
          k >= min_effect_sizes &
          n_studies >= min_studies
      )
  }

# Fitted brms model with known sampling standard error.

fit_meta_model <-
  function(
    .data,
    model_formula,
    priors,
    settings = sampler_settings,
    refit_from = NULL) {
    if (!is.null(refit_from)) {
      return(
        update(
          refit_from,
          newdata = .data,
          chains = settings$chains,
          iter = settings$iter,
          warmup = settings$warmup,
          cores = settings$cores,
          seed = settings$seed,
          refresh = 0,
          silent = 2
        )
      )
    }
    brm(
      formula = model_formula,
      data = .data,
      prior = priors,
      chains = settings$chains,
      iter = settings$iter,
      warmup = settings$warmup,
      cores = settings$cores,
      control =
        list(
          adapt_delta = settings$adapt_delta,
          max_treedepth = settings$max_treedepth
        ),
      backend = settings$backend,
      seed = settings$seed,
      refresh = 0,
      silent = 2
    )
  }

# Fits one compile group: the first spec compiles the Stan program and the
# rest reuse it through refit_from.

fit_model_group <-
  function(
    specs,
    pools,
    priors) {
    if (!is.na(specs$refit_from[[1]])) {
      cli::cli_abort(
        "{.fn fit_model_group} needs the compiling model in the first row."
      )
    }
    template <-
      pools[[specs$pool[[1]]]] %>%
      fit_meta_model(
        model_formula = specs$model_formula[[1]],
        priors = priors
      )
    reused <-
      specs %>%
      slice(-1) %>%
      pmap(
        \(pool, model_formula, ...) {
          pools[[pool]] %>%
            fit_meta_model(
              model_formula = model_formula,
              refit_from = template
            )
        }
      )
    template %>%
      list() %>%
      c(reused) %>%
      set_names(specs$model)
  }

# Median, SD, credible interval and P(draw > 0).

summarise_draws_vector <-
  function(
    draws,
    interval = 0.95) {
    lower_tail <- (1 - interval) / 2
    quantiles <-
      quantile(
        draws,
        probs =
          c(
            lower_tail,
            1 - lower_tail
          )
      )
    tibble(
      estimate = median(draws),
      post_sd = sd(draws),
      lcl = quantiles[[1]],
      ucl = quantiles[[2]],
      prob_positive = mean(draws > 0),
      excludes_zero =
        sign(quantiles[[1]]) ==
        sign(quantiles[[2]])
    )
  }

draws_tibble <-
  function(fit) {
    fit %>%
      as_draws_df() %>%
      as_tibble()
  }

# Posterior cell means, with brms prefixes stripped from terms.

summarise_cell_means <-
  function(
    fit,
    term_prefix,
    interval = 0.95) {
    draws <- draws_tibble(fit)
    cell_prefix <- str_c("b_", term_prefix)
    cell_pattern <- str_c("^b_", term_prefix)
    draws %>%
      names() %>%
      keep(
        ~ str_starts(.x, cell_prefix)
      ) %>%
      set_names() %>%
      map(
        ~ draws %>%
          pull(.x) %>%
          summarise_draws_vector(interval = interval)
      ) %>%
      list_rbind(names_to = "term") %>%
      mutate(
        cell =
          term %>%
          str_remove(cell_pattern),
        .before = term
      )
  }

# Posterior difference between two cells, or NULL if either is absent.

contrast_cells <-
  function(
    fit,
    cell_a,
    cell_b,
    term_prefix,
    interval = 0.95) {
    draws <- draws_tibble(fit)
    column_a <-
      str_c(
        "b_",
        term_prefix,
        cell_a
      )
    column_b <-
      str_c(
        "b_",
        term_prefix,
        cell_b
      )
    columns <- c(column_a, column_b)
    if (!all(columns %in% names(draws))) {
      return(NULL)
    }
    (draws[[column_a]] - draws[[column_b]]) %>%
      summarise_draws_vector(interval = interval) %>%
      mutate(
        cell_a = {{ cell_a }},
        cell_b = {{ cell_b }},
        .before = estimate
      ) %>%
      rename(
        prob_a_greater = prob_positive
      )
  }

typical_sampling_variance <-
  function(sampling_se) {
    weights <- 1 / sampling_se^2
    (length(sampling_se) - 1) * sum(weights) /
      (sum(weights)^2 - sum(weights^2))
  }

# Posterior tau and I-squared share for each random-effect level.

summarise_heterogeneity <-
  function(
    fit,
    sampling_se,
    interval = 0.95) {
    draws <- draws_tibble(fit)
    typical_variance <- typical_sampling_variance(sampling_se)
    variance_draws <-
      draws %>%
      names() %>%
      keep(
        ~ str_starts(.x, "sd_")
      ) %>%
      set_names() %>%
      map(
        ~ draws[[.x]]^2
      )
    total_variance <-
      variance_draws %>%
      reduce(`+`) +
      typical_variance
    variance_draws %>%
      imap(
        \(.variance, .level) {
          bind_cols(
            draws %>%
              pull(.level) %>%
              summarise_draws_vector(interval = interval) %>%
              select(
                tau = estimate,
                tau_lcl = lcl,
                tau_ucl = ucl
              ),
            (.variance / total_variance * 100) %>%
              summarise_draws_vector(interval = interval) %>%
              select(
                i2 = estimate,
                i2_lcl = lcl,
                i2_ucl = ucl
              )
          )
        }
      ) %>%
      list_rbind(names_to = "level") %>%
      mutate(
        level =
          level %>%
          str_remove("^sd_") %>%
          str_remove("__Intercept$"),
        typical_sampling_variance = typical_variance,
        .before = tau
      )
  }

# Worst rhat, ESS and divergence count across parameters.

summarise_convergence <-
  function(
    fit,
    model_name) {
    summary_draws <-
      fit %>%
      as_draws_df() %>%
      summarise_draws()
    tibble(
      model = {{ model_name }},
      n_parameters = nrow(summary_draws),
      max_rhat =
        max(
          summary_draws$rhat,
          na.rm = TRUE
        ),
      min_bulk_ess =
        min(
          summary_draws$ess_bulk,
          na.rm = TRUE
        ),
      min_tail_ess =
        min(
          summary_draws$ess_tail,
          na.rm = TRUE
        ),
      n_divergent =
        fit %>%
        nuts_params() %>%
        filter(Parameter == "divergent__") %>%
        pull(Value) %>%
        sum()
    )
  }

# labels -------------------------------------------------------------------

format_bmp <-
  function(.bmp) {
    .bmp %>%
      str_replace("non_native", "non-native") %>%
      str_replace_all("_", " ") %>%
      str_to_title() %>%
      str_replace_all("\\bAnd\\b", "and") %>%
      str_replace_all("\\bIn\\b", "in") %>%
      str_replace_all("\\bOf\\b", "of") %>%
      str_replace("Nwsgs?", "NWSG") %>%
      str_replace("-N", "-n")
  }

format_response <-
  function(.response) {
    .response %>%
      str_replace_all("_", " ") %>%
      str_to_title()
  }

# The manuscript tables name the pooled guild in full; the results page
# overrides this with a shorter label after sourcing.

pooled_guild_display <- "All grassland birds (pooled)"

format_guild <-
  function(
    .guild,
    pooled_display = pooled_guild_display) {
    sentence_case <-
      .guild %>%
      str_replace_all("_", " ") %>%
      str_to_sentence()
    if_else(
      .guild == "all_grassland",
      pooled_display,
      sentence_case
    )
  }

# Possessive stems and their display forms.

species_possessives <-
  c(
    henslows = "Henslow's",
    bairds = "Baird's",
    cassins = "Cassin's",
    bachmans = "Bachman's",
    brewers = "Brewer's",
    swainsons = "Swainson's",
    mccowns = "McCown's",
    bells = "Bell's",
    bewicks = "Bewick's"
  )

# Species common name with possessive stems restored.

format_species <-
  function(.species) {
    spaced_name <-
      .species %>%
      str_replace_all("_", " ")
    names(species_possessives) %>%
      reduce2(
        species_possessives,
        str_replace,
        .init = spaced_name
      ) %>%
      str_to_sentence()
  }

format_number <-
  function(
    .x,
    digits = 2) {
    formatC(
      .x,
      format = "f",
      digits = digits
    )
  }

# "estimate (lcl, ucl)" as one string.

format_estimate <-
  function(
    estimate,
    lcl,
    ucl,
    digits = 2) {
    str_c(
      format_number(estimate, digits = digits),
      " (",
      format_number(lcl, digits = digits),
      ", ",
      format_number(ucl, digits = digits),
      ")"
    )
  }

# plotting -----------------------------------------------------------------

wrap_label <-
  function(
    .text,
    width = 90) {
    str_wrap(
      .text,
      width = width
    )
  }

theme_bmp <-
  function(base_size = 11) {
    theme_bw(base_size = base_size) +
      theme(
        panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.major.x =
          element_line(
            colour = "grey88",
            linewidth = 0.25
          ),
        strip.background =
          element_rect(
            fill = "grey95",
            colour = "black",
            linewidth = 0.4
          ),
        strip.text =
          element_text(
            size = rel(0.9),
            face = "bold"
          ),
        axis.title =
          element_text(
            size = rel(1.0)
          ),
        plot.title =
          element_text(
            size = rel(1.15),
            face = "bold"
          ),
        plot.title.position = "plot"
      )
  }

# output -------------------------------------------------------------------

write_output_table <-
  function(
    .data,
    file_name,
    directory = "output/tables") {
    write_csv(
      .data,
      file.path(directory, file_name),
      na = ""
    )
    invisible(.data)
  }

write_output_figure <-
  function(
    plot_object,
    file_name,
    width = 7,
    height = 5) {
    ggsave(
      filename =
        file.path("output/figures", file_name),
      plot = plot_object,
      width = width,
      height = height,
      dpi = 400
    )
    invisible(plot_object)
  }

# shared readers and writers -----------------------------------------------

# Guild and label type per record. 2_clean_google_sheet.R already attached
# each species' analysis class, so the guild is read off the record rather
# than joined back from the classification frame. A species the
# classification never reached carries a missing class and is marked here,
# not dropped in silence and not joined blind.

add_species_guilds <-
  function(
    .data,
    guild_levels =
      c("obligate_grassland", "facultative_grassland"),
    guild_response_metrics =
      c("nest_success", "abundance")) {
    guilded <-
      .data %>%
      mutate(
        is_community = species_key == "all_species",
        candidate_guild = str_c(analysis_class, "_grassland"),
        label_type =
          case_when(
            is_community ~ "community",
            !is.na(analysis_class) ~ "species"
          ),
        guild =
          if_else(
            !is_community &
              candidate_guild %in% guild_levels,
            candidate_guild,
            NA_character_
          ),
        species_classified = !is.na(label_type),
        species_missing =
          !species_classified &
            response_metric %in% guild_response_metrics
      )

    unclassified <-
      guilded %>%
      filter(species_missing) %>%
      pull(species_key) %>%
      unique()

    if (length(unclassified) > 0) {
      cli::cli_warn(
        "{length(unclassified)} species carry no analysis class, held out. \\
         Named in the unclassified-species audit."
      )
    }

    guilded
  }

# Species the classification never assigned, most frequent first. Richness is
# assemblage-level, so its rows never appear here.

unclassified_grouping <-
  c(
    "source_sheet",
    "species_key"
  )

write_unclassified_species <-
  function(
    .data,
    file_name) {
    .data %>%
      filter(species_missing) %>%
      summarise(
        n_records = n(),
        n_studies = n_distinct(key),
        .by = any_of(unclassified_grouping)
      ) %>%
      arrange(
        desc(n_records)
      ) %>%
      write_audit_table(file_name = file_name)
  }

write_audit_table <-
  function(
    .data,
    file_name) {
    .data %>%
      write_output_table(
        file_name = file_name,
        directory = "output/audits"
      )
  }

# statistic conversion -----------------------------------------------------

# The value on the rows the condition holds for, missing elsewhere, so that
# each conversion is only ever evaluated inside its own domain.

value_when <-
  function(
    .condition,
    .value) {
    if_else(
      .condition,
      .value,
      NA_real_
    )
  }

d_from_r <-
  function(.correlation) {
    2 * .correlation / sqrt(1 - .correlation^2)
  }

# Hasselblad and Hedges (1995), log odds ratio to standardised difference.

d_from_odds_ratio <-
  function(.odds_ratio) {
    log_odds <- log(.odds_ratio)
    log_odds * sqrt(3) / pi
  }

# effect size derivation ---------------------------------------------------

# Group SD from a reported SD, else an SE, else a confidence interval.

derive_group_sd <-
  function(
    sd_reported,
    se_reported,
    lower_cl,
    upper_cl,
    n) {
    has_sd <- !is.na(sd_reported)
    has_se <- !is.na(se_reported)
    has_lower <- !is.na(lower_cl)
    has_upper <- !is.na(upper_cl)
    has_confint <- has_lower & has_upper
    sd_from_se <- se_to_sd(se_reported, n)
    sd_from_confint <-
      confint_to_sd(
        lower_cl,
        upper_cl,
        n
      )
    tibble(
      group_sd =
        case_when(
          has_sd ~ sd_reported,
          has_se ~ sd_from_se,
          has_confint ~ sd_from_confint,
          .default = NA_real_
        ),
      group_sd_form =
        case_when(
          has_sd ~ "reported SD",
          has_se ~ "SE x sqrt(n)",
          has_confint ~ "95% CI width / 3.92 x sqrt(n)",
          .default = "unavailable"
        )
    )
  }

# Group SD columns for one arm, named after that arm.

add_group_sd <-
  function(
    .data,
    arm) {
    derived <-
      derive_group_sd(
        sd_reported = .data[[str_c("sd_", arm)]],
        se_reported = .data[[str_c("se_", arm)]],
        lower_cl = .data[[str_c("lcl_", arm)]],
        upper_cl = .data[[str_c("ucl_", arm)]],
        n = .data[[str_c("n_", arm)]]
      ) %>%
      rename(
        "sd_{arm}_used" := group_sd,
        "sd_{arm}_form" := group_sd_form
      )
    bind_cols(.data, derived)
  }

# Coefficient SE from a reported SE, else an SD, else either interval.

derive_beta_se <-
  function(
    se_reported,
    sd_reported,
    lower_cl,
    upper_cl,
    lower_cl_e,
    upper_cl_e,
    n) {
    has_se <- !is.na(se_reported)
    has_sd <- !is.na(sd_reported)
    has_lower <- !is.na(lower_cl)
    has_upper <- !is.na(upper_cl)
    has_lower_e <- !is.na(lower_cl_e)
    has_upper_e <- !is.na(upper_cl_e)
    has_confint <- has_lower & has_upper
    has_confint_e <- has_lower_e & has_upper_e
    se_from_sd <- sd_to_se(sd_reported, n)
    se_from_confint <- confint_to_se(lower_cl, upper_cl)
    se_from_confint_e <- confint_to_se(lower_cl_e, upper_cl_e)
    tibble(
      se_used =
        case_when(
          has_se ~ se_reported,
          has_sd ~ se_from_sd,
          has_confint ~ se_from_confint,
          has_confint_e ~ se_from_confint_e,
          .default = NA_real_
        ),
      se_form =
        case_when(
          has_se ~ "reported SE",
          has_sd ~ "SD / sqrt(n)",
          has_confint ~ "95% CI width / 3.92",
          has_confint_e ~ "95% CI width / 3.92",
          .default = "unavailable"
        )
    )
  }

# Zero-padded, input-class-prefixed row identifier.

add_row_id <-
  function(
    .data,
    prefix) {
    .data %>%
      mutate(
        row_id =
          row_number() %>%
          formatC(
            width = 4,
            flag = "0"
          ) %>%
          str_c(.env$prefix, .),
        .before = key
      )
  }

# The coarser practice the models group on. Practice strings arrive canonical
# and one per row from 2_clean_google_sheet.R, so all that is left is the
# grouping, which db_vocab carries for the database and the analysis alike.

add_analysis_bmp <-
  function(.data) {
    renamed <-
      .data %>%
      left_join(
        db_vocab$bmp_renames,
        by = join_by(bmp == recorded_code)
      ) %>%
      mutate(bmp = coalesce(bmp_code, bmp)) %>%
      select(!bmp_code)

    unknown <-
      renamed %>%
      pull(bmp) %>%
      unique() %>%
      setdiff(db_vocab$bmp_canonical$bmp_code)

    if (length(unknown) > 0) {
      cli::cli_abort(
        "Practices missing from db_vocab$bmp_canonical: {unknown}."
      )
    }

    renamed %>%
      left_join(
        db_vocab$bmp_analysis_groups,
        by = join_by(bmp == bmp_code)
      ) %>%
      mutate(bmp = coalesce(analysis_bmp_code, bmp)) %>%
      select(
        !c(analysis_bmp_code, analysis_bmp_label)
      )
  }

# The one read path for all five cleaned extraction tables, and the only
# source the analysis reads. Names, practices and species are canonical there
# already, and each row carries its species' analysis class, so nothing is
# cleaned here. `sign` and the review flags are absent from the two continuous
# tables.

read_extraction <-
  function(
    sheet_name,
    directory = "data/processed/for_analysis") {
    directory %>%
      fs::path(sheet_name, ext = "csv") %>%
      read_csv(show_col_types = FALSE) %>%
      mutate(
        source_sheet = sheet_name,
        species_key = species,
        response_metric = response_class,
        response_scale =
          derive_response_scale(response_var)
      )
  }

derive_response_scale <-
  function(.response_var) {
    .response_var %>%
      str_to_lower() %>%
      str_detect("diversity|evenness|shannon|simpson") %>%
      if_else("diversity", "count")
  }

# Split a response variable into a year-or-site token and a base expression.

split_response_var <-
  function(.response_var) {
    tokens <-
      .response_var %>%
      str_extract_all("\\(([^)]*)\\)|[0-9]{2,4}(-[0-9]{2,4})?") %>%
      map_chr(
        ~ .x %>%
          str_remove_all("[()]") %>%
          sort() %>%
          str_c(collapse = "+")
      )
    base <-
      .response_var %>%
      str_remove_all("\\([^)]*\\)") %>%
      str_remove_all("[0-9]{2,4}(-[0-9]{2,4})?") %>%
      str_remove_all("[-;,]") %>%
      str_squish()
    tibble(
      response_token = tokens,
      response_base = base
    )
  }

rank_response_expression <-
  function(
    .response_metric,
    .response_var) {
    matches <-
      response_expression_preference %>%
      filter(
        response_metric == .response_metric,
        str_detect(
          str_to_lower(.response_var),
          pattern
        )
      )
    if (nrow(matches) == 0) {
      return(99L)
    }
    min(matches$preference_rank)
  }

# model pools --------------------------------------------------------------

build_guild_pool <-
  function(
    .data,
    metric) {
    .data %>%
      filter(
        response_metric == {{ metric }},
        !is.na(guild)
      ) %>%
      mutate(
        guild = fct_drop(guild)
      ) %>%
      apply_inclusion_thresholds(
        grouping_vars = "guild"
      ) %>%
      mutate(
        across(
          c(bmp, species_key),
          fct_drop
        )
      )
  }

# Both guilds in one stratum: assemblage rows stay out, the contributing guild
# is kept as source_guild.

build_pooled_pool <-
  function(
    .data,
    metric,
    pooled_label = "all_grassland") {
    .data %>%
      filter(
        response_metric == {{ metric }},
        !is.na(guild)
      ) %>%
      mutate(
        source_guild =
          as.character(guild),
        guild = factor(pooled_label)
      ) %>%
      apply_inclusion_thresholds(
        grouping_vars = "guild"
      ) %>%
      mutate(
        across(
          c(bmp, species_key),
          fct_drop
        )
      )
  }

# Inclusion thresholds applied at the guild x BMP cell level.

# Practices with an estimable cell in BOTH guilds. The pooled x practice model
# is restricted to these, so a pooled estimate never stands on one guild's
# evidence while being read as an assemblage-level result.

practices_in_both_guilds <-
  function(.guild_bmp_pool) {
    .guild_bmp_pool %>%
      distinct(
        guild,
        bmp
      ) %>%
      summarise(
        n_guilds = n_distinct(guild),
        .by = bmp
      ) %>%
      filter(n_guilds == 2) %>%
      pull(bmp) %>%
      as.character()
  }

build_guild_bmp_pool <-
  function(.data) {
    .data %>%
      apply_inclusion_thresholds(
        grouping_vars = c("guild", "bmp")
      ) %>%
      mutate(
        guild_bmp =
          str_c(
            guild,
            "__",
            bmp
          ) %>%
          as.factor(),
        across(
          c(bmp, species_key),
          fct_drop
        )
      )
  }

count_cells <-
  function(
    .pool,
    grouping_vars) {
    .pool %>%
      summarise(
        k = n(),
        n_studies = n_distinct(key),
        .by = all_of(grouping_vars)
      )
  }

# contrasts and manuscript tables ------------------------------------------

# Marks a row as a guild estimate or as the pooled estimate, so that the two
# are never read as the same kind of thing.

add_guild_scope <-
  function(.data) {
    .data %>%
      mutate(
        guild_scope =
          if_else(
            guild == "all_grassland",
            "pooled",
            "by guild"
          ),
        .after = guild
      )
  }

contrast_guilds_within_bmp <-
  function(
    model_name,
    metric) {
    fit <-
      fitted_models %>%
      pluck(model_name)
    shared_bmps <-
      fit %>%
      draws_tibble() %>%
      names() %>%
      keep(
        ~ str_starts(.x, "b_guild_bmp")
      ) %>%
      str_remove("^b_guild_bmp") %>%
      tibble(cell = .) %>%
      separate_wider_delim(
        cell,
        delim = "__",
        names = c("guild", "bmp")
      ) %>%
      filter(
        n_distinct(guild) == 2,
        .by = bmp
      ) %>%
      distinct(bmp) %>%
      pull(bmp)
    if (length(shared_bmps) == 0) {
      return(NULL)
    }
    shared_bmps %>%
      map(
        \(.bmp) {
          contrast_cells(
            fit = fit,
            cell_a = str_c("obligate_grassland__", .bmp),
            cell_b = str_c("facultative_grassland__", .bmp),
            term_prefix = "guild_bmp"
          ) %>%
            mutate(
              response_metric = {{ metric }},
              bmp = .bmp,
              contrast = "obligate minus facultative",
              .before = cell_a
            )
        }
      ) %>%
      list_rbind()
  }

extract_species_estimates <-
  function(
    model_name,
    metric) {
    fit <-
      fitted_models %>%
      pluck(model_name)
    draws <- draws_tibble(fit)
    species_columns <-
      draws %>%
      names() %>%
      keep(
        ~ str_starts(.x, "r_species_key\\[")
      )
    species_guild <-
      fit$data %>%
      as_tibble() %>%
      distinct(species_key, guild) %>%
      mutate(
        across(
          everything(),
          as.character
        )
      )
    species_columns %>%
      set_names() %>%
      map(
        \(.column) {
          species_name <-
            .column %>%
            str_extract("(?<=\\[).+(?=,)")
          species_row <-
            species_guild %>%
            filter(species_key == species_name)
          if (nrow(species_row) != 1) {
            return(NULL)
          }
          guild_column <-
            str_c("b_guild", species_row$guild)
          (draws[[guild_column]] + draws[[.column]]) %>%
            summarise_draws_vector() %>%
            mutate(
              species_key = species_name,
              guild = species_row$guild,
              .before = estimate
            )
        }
      ) %>%
      list_rbind() %>%
      mutate(
        response_metric = {{ metric }},
        .before = species_key
      )
  }

# Rows whose interval excludes zero are bolded.

format_manuscript_table <-
  function(
    .data,
    caption) {
    formatted <-
      .data %>%
      mutate(
        `Effect size (95% CrI)` =
          format_estimate(
            estimate = estimate,
            lcl = lcl,
            ucl = ucl
          ),
        `P(effect > 0)` =
          format_number(
            prob_positive,
            digits = 3
          )
      ) %>%
      select(
        !c(
          estimate,
          lcl,
          ucl,
          prob_positive
        )
      )
    bold_rows <- which(formatted$excludes_zero)
    table_object <-
      formatted %>%
      select(!excludes_zero) %>%
      flextable::flextable() %>%
      flextable::set_caption(caption) %>%
      flextable::autofit()
    if (length(bold_rows) > 0) {
      table_object <-
        table_object %>%
        flextable::bold(
          i = bold_rows,
          bold = TRUE
        )
    }
    table_object
  }

# figure construction ------------------------------------------------------

read_table_output <-
  function(file_name) {
    file.path(
      "output/tables",
      str_c(file_name, ".csv")
    ) %>%
      read_csv(show_col_types = FALSE)
  }

effect_axis_label_for <-
  function(.metric) {
    if_else(
      .metric == "nest_success",
      "Pooled effect size (log hazard ratio, 95% credible interval)",
      effect_axis_label
    )
  }

add_bmp_label <-
  function(.data) {
    .data %>%
      mutate(
        bmp_label =
          bmp %>%
          format_bmp() %>%
          fct_reorder(estimate)
      )
  }

add_guild_label <-
  function(.data) {
    .data %>%
      mutate(
        guild_label =
          guild %>%
          format_guild() %>%
          factor(
            levels = names(guild_colours)
          )
      )
  }

add_zero_line <-
  function(plot_object) {
    plot_object +
      geom_vline(
        xintercept = 0,
        linetype = "dashed",
        linewidth = 0.3,
        colour = "grey40"
      )
  }

add_interval_layers <-
  function(
    plot_object,
    interval_mapping =
      aes(
        xmin = lcl,
        xmax = ucl
      ),
    point_mapping = NULL,
    point_size = 2.6,
    line_width = 0.7,
    layer_position = "identity") {
    plot_object +
      geom_linerange(
        mapping = interval_mapping,
        linewidth = line_width,
        position = layer_position
      ) +
      geom_point(
        mapping = point_mapping,
        size = point_size,
        position = layer_position
      )
  }

add_forest_layers <-
  function(
    plot_object,
    label_mapping = sample_size_label,
    label_nudge = 0.12) {
    plot_object %>%
      add_zero_line() %>%
      add_interval_layers(
        point_mapping =
          aes(shape = excludes_zero)
      ) +
      geom_text(
        mapping = label_mapping,
        hjust = 0,
        nudge_x = label_nudge,
        size = 2.9,
        colour = "grey25"
      ) +
      scale_shape_manual(
        values =
          c(
            `TRUE` = 16,
            `FALSE` = 1
          ),
        guide = "none"
      )
  }

build_guild_figure <-
  function(
    .data,
    metric,
    plot_title) {
    plot_data <-
      .data %>%
      filter(response_metric == {{ metric }}) %>%
      add_guild_label() %>%
      add_bmp_label()
    ggplot(
      data = plot_data,
      mapping =
        aes(
          x = estimate,
          y = bmp_label,
          colour = guild_label
        )
    ) %>%
      add_forest_layers() +
      facet_wrap(
        facets = vars(guild_label),
        ncol = 1,
        scales = "free_y"
      ) +
      scale_colour_manual(
        values = guild_colours,
        guide = "none"
      ) +
      scale_x_continuous(
        expand =
          expansion(
            mult = c(0.06, 0.28)
          )
      ) +
      labs(
        title = plot_title,
        subtitle =
          wrap_label(
            str_c(
              "Each guild estimated separately, and the two guilds pooled ",
              "in their own panel. ",
              filled_point_note
            )
          ),
        x = effect_axis_label_for(metric),
        y = NULL
      ) +
      theme_bmp(base_size = 12)
  }

# sensitivity --------------------------------------------------------------

# Refit one prepared pool; NULL when no cell clears the thresholds.

refit_cells <-
  function(
    .pool,
    template_model,
    template_name,
    cell_variable,
    grouping_vars,
    specification,
    response_metric) {
    prepared <-
      .pool %>%
      apply_inclusion_thresholds(
        grouping_vars = grouping_vars
      )
    if (nrow(prepared) < 3) {
      return(NULL)
    }
    prepared <-
      prepared %>%
      mutate(
        guild_bmp =
          if ("guild" %in% names(prepared)) {
            str_c(
              guild,
              "__",
              bmp
            )
          } else {
            NA_character_
          }
      ) %>%
      filter(
        !is.na(.data[[cell_variable]])
      ) %>%
      mutate(
        across(
          any_of(factor_columns),
          ~ .x %>%
            as.factor() %>%
            fct_drop()
        )
      )
    if (n_distinct(prepared[[cell_variable]]) < 1) {
      return(NULL)
    }
    fit <-
      prepared %>%
      fit_meta_model(
        model_formula = NULL,
        refit_from = template_model
      )
    cell_means <-
      fit %>%
      summarise_cell_means(term_prefix = cell_variable)
    cell_means <-
      switch(
        cell_variable,
        guild_bmp =
          cell_means %>%
          separate_wider_delim(
            cell,
            delim = "__",
            names = c("guild", "bmp")
          ),
        guild =
          cell_means %>%
          rename(guild = cell) %>%
          mutate(
            bmp = "all practices (guild-level model)"
          ),
        bmp =
          cell_means %>%
          rename(bmp = cell) %>%
          mutate(
            guild = NA_character_
          )
      )
    cell_means %>%
      mutate(
        response_metric = {{ response_metric }},
        model_family = {{ template_name }},
        specification = {{ specification }}
      ) %>%
      select(
        response_metric,
        model_family,
        specification,
        guild,
        bmp,
        estimate,
        lcl,
        ucl,
        excludes_zero
      )
  }

# Refit one model family under one specification.

refit_family <-
  function(
    .pool,
    .family,
    specification) {
    refit_cells(
      .pool = .pool,
      template_model =
        fitted_models %>%
        pluck(.family$template),
      template_name = .family$template,
      cell_variable = .family$cell_variable,
      grouping_vars = .family$grouping_vars,
      specification = specification,
      response_metric = .family$response_metric
    )
  }

# Build the pool for one metric; every design choice is an argument.

# Richness rows measuring a diversity index rather than a species count.

add_index_type <-
  function(.data) {
    .data %>%
      mutate(
        index_type =
          if_else(
            response_scale == "diversity",
            "diversity",
            "richness"
          ) %>%
          factor(
            levels = c("richness", "diversity")
          )
      )
  }

# The analysis pool: every categorical record 1_effect_sizes.R converted, on
# one Hedges' g scale whichever of the three routes it took.

read_effect_size_pool <-
  function() {
    bmp_read_table(
      "effect_sizes",
      required_columns = effect_size_columns
    )
  }

# The effects held out by the flagged-records specification.

# Keyed on the extraction content rather than on a row number: `row_id` is
# assigned by position after filtering, and the analysis sheets and the source
# database no longer share a surrogate key, so neither survives a re-export.
# A row marked resolved has been corrected upstream and is no longer held out,
# which is what keeps the specification answering "how much rests on records we
# have not yet verified" as verification proceeds.

excluded_effect_columns <-
  c(
    "key",
    "response_var",
    "species",
    "treatment",
    "control"
  )

read_excluded_effects <-
  function(
    file_path = "brian_sandbox/data/excluded_effects.csv") {
    empty <-
      excluded_effect_columns %>%
      set_names() %>%
      map(~ character()) %>%
      as_tibble()
    if (!fs::file_exists(file_path)) {
      return(empty)
    }
    listed <-
      read_csv(
        file_path,
        show_col_types = FALSE
      )
    missing_columns <-
      setdiff(
        c(excluded_effect_columns, "status"),
        names(listed)
      )
    if (length(missing_columns) > 0) {
      cli::cli_abort("excluded_effects.csv is missing {missing_columns}.")
    }
    listed %>%
      filter(status == "open") %>%
      distinct(
        across(
          all_of(excluded_effect_columns)
        )
      )
  }

build_pool <-
  function(
    response_metric,
    retain_fire = TRUE,
    drop_flagged = FALSE,
    by_guild = TRUE,
    pooled = FALSE) {
    pool <-
      effect_sizes %>%
      filter(
        response_metric == {{ response_metric }},
        is.finite(yi),
        is.finite(sei),
        sei > 0
      )
    if (!retain_fire) {
      pool <-
        pool %>%
        filter(!fire_excluded_original)
    }
    if (drop_flagged) {
      pool <-
        pool %>%
        anti_join(
          read_excluded_effects(),
          by = excluded_effect_columns
        )
    }
    if (by_guild) {
      pool <-
        pool %>%
        filter(
          !is.na(guild)
        )
    }
    if (pooled) {
      pool <-
        pool %>%
        filter(
          !is.na(guild)
        ) %>%
        mutate(
          source_guild = guild,
          guild = "all_grassland"
        )
    }
    pool %>%
      add_index_type()
  }

write_partial_estimates <-
  function(.estimates) {
    append_mode <- fs::file_exists(partial_estimates_file)
    write_csv(
      .estimates,
      partial_estimates_file,
      na = "",
      append = append_mode
    )
    invisible(.estimates)
  }

# One effect's studentized deleted residual, following Viechtbauer and Cheung
# (2010). The effect is compared with a random-effects mean fitted WITHOUT it,
# so it is never measured against an interval it helped produce. The
# DerSimonian-Laird estimator is closed form and so cannot fail to converge on
# the small cells this is applied to.

studentized_deleted_residual <-
  function(
    yi,
    sei,
    index) {
    retained <- setdiff(seq_along(yi), index)
    fit <-
      possibly(rma)(
        yi = yi[retained],
        sei = sei[retained],
        method = "DL"
      )
    if (is.null(fit)) {
      return(NA_real_)
    }
    residual_variance <-
      sei[index]^2 +
      fit$tau2 +
      as.numeric(fit$se)^2
    if (!is.finite(residual_variance) || residual_variance <= 0) {
      return(NA_real_)
    }
    (yi[index] - as.numeric(fit$beta)) / sqrt(residual_variance)
  }

# Residuals for every effect in one cell. A cell too small to leave one out
# returns missing residuals rather than unstable ones, so the shortfall is
# visible downstream instead of being silently scored as "no outliers".

cell_deleted_residuals <-
  function(
    yi,
    sei,
    min_effects = 3) {
    if (length(yi) < min_effects) {
      return(
        rep(NA_real_, length(yi))
      )
    }
    map_dbl(
      seq_along(yi),
      \(.index) {
        studentized_deleted_residual(
          yi = yi,
          sei = sei,
          index = .index
        )
      }
    )
  }

# An outlier is an effect whose studentized deleted residual clears the
# critical value. Viechtbauer and Cheung (2010) put that at 1.96, which about
# 5% will clear by chance -- so a flagged effect is refitted as a sensitivity
# check and never dropped.

flag_outliers <-
  function(
    .pool,
    join_vars,
    critical_value = 1.96) {
    .pool %>%
      mutate(
        across(
          any_of(guild_bmp_columns),
          as.character
        )
      ) %>%
      mutate(
        studentized_residual =
          cell_deleted_residuals(
            yi = yi,
            sei = sei
          ),
        .by = all_of(join_vars)
      ) %>%
      mutate(
        is_outlier =
          !is.na(studentized_residual) &
          abs(studentized_residual) > critical_value
      )
  }

# One inverse-variance weighted effect size per study per guild.

aggregate_within_study <-
  function(.pool) {
    .pool %>%
      mutate(
        weight = 1 / sei^2,
        guild =
          if ("guild" %in% names(.pool)) {
            as.character(guild)
          } else {
            "all"
          }
      ) %>%
      summarise(
        yi =
          sum(yi * weight) /
          sum(weight),
        sei =
          sqrt(
            1 / sum(weight)
          ),
        k = n(),
        .by = c(key, guild)
      )
  }

run_egger_test <-
  function(
    .data,
    label) {
    aggregated <-
      .data %>%
      aggregate_within_study()
    if (nrow(aggregated) < 10) {
      return(
        tibble(
          analysis = {{ label }},
          n_studies = nrow(aggregated)
        )
      )
    }
    model <-
      rma(
        yi = aggregated$yi,
        sei = aggregated$sei,
        method = "REML"
      )
    egger <- regtest(model, model = "lm")
    tibble(
      analysis = {{ label }},
      n_studies = nrow(aggregated),
      pooled_estimate = as.numeric(model$beta),
      egger_statistic = egger$zval,
      egger_p = egger$pval,
      asymmetry_detected = egger$pval < 0.05
    )
  }

# verification -------------------------------------------------------------

same_sign <-
  function(
    .a,
    .b) {
    signs_a <- sign(.a)
    signs_b <- sign(.b)
    signs_a == signs_b
  }

fit_reml_cell_means <-
  function(
    .pool,
    cell_variable) {
    model_data <-
      .pool %>%
      mutate(
        vi = sei^2,
        cell =
          .data[[cell_variable]] %>%
          as.factor()
      ) %>%
      as.data.frame()
    reml_fit <-
      rma.mv(
        yi = yi,
        V = vi,
        mods = ~ 0 + cell,
        random =
          list(
            ~ 1 | key,
            ~ 1 | es_id,
            ~ 1 | species_key
          ),
        data = model_data,
        method = "REML",
        sparse = TRUE
      )
    tibble(
      cell =
        reml_fit$b %>%
        rownames() %>%
        str_remove("^cell"),
      reml_estimate = as.numeric(reml_fit$b),
      reml_lcl = as.numeric(reml_fit$ci.lb),
      reml_ucl = as.numeric(reml_fit$ci.ub)
    )
  }

# posterior figures --------------------------------------------------------

# Tidy draws of one model's cell means: one row per draw and cell.

gather_cell_draws <-
  function(
    fit,
    term_prefix) {
    cell_pattern <-
      str_c("^b_", term_prefix)
    term_pattern <-
      str_c(
        "b_",
        term_prefix,
        ".*"
      )
    fit %>%
      gather_draws(
        !!sym(term_pattern),
        regex = TRUE
      ) %>%
      ungroup() %>%
      mutate(
        cell =
          .variable %>%
          str_remove(cell_pattern)
      )
  }

# Cell means split back into scope and practice; the pooled x BMP models share
# the guild x BMP term names, so both read through here.

gather_guild_bmp_draws <-
  function(fit) {
    fit %>%
      gather_cell_draws(term_prefix = "guild_bmp") %>%
      separate_wider_delim(
        cell,
        delim = "__",
        names = c("guild", "bmp")
      )
  }


# Practice axis ordered by posterior median, as the interval figures order it.

add_posterior_bmp_label <-
  function(.data) {
    .data %>%
      mutate(
        bmp_label =
          bmp %>%
          format_bmp() %>%
          fct_reorder(.value)
      )
  }

# Posterior mass above zero per cell, as the text drawn beside each slab.

summarise_posterior_probability <-
  function(
    .draws,
    grouping_vars) {
    .draws %>%
      summarise(
        prob_positive = mean(.value > 0),
        .by = all_of(grouping_vars)
      ) %>%
      mutate(
        probability_label =
          prob_positive %>%
          format_number() %>%
          str_c("P>0 = ", .)
      )
  }

# Density slab carrying the same median and 95% interval the forest plots
# draw, so the two figure families report one number.

add_posterior_layers <-
  function(
    plot_object,
    interval_width = 0.95,
    slab_alpha = 0.55,
    ...) {
    plot_object %>%
      add_zero_line() +
      stat_halfeye(
        .width = interval_width,
        point_interval = "median_qi",
        normalize = "xy",
        slab_alpha = slab_alpha,
        slab_linewidth = 0.3,
        point_size = 1.6,
        ...
      )
  }

# Probability text pinned to the right edge, clear of the widest slab. hjust
# above one insets it from the panel border.

add_probability_labels <-
  function(
    plot_object,
    label_data,
    label_size = 2.8,
    label_hjust = 1.12) {
    plot_object +
      geom_text(
        data = label_data,
        mapping = probability_label_mapping,
        hjust = label_hjust,
        size = label_size,
        colour = "grey25"
      )
  }

# One posterior forest panel per guild scope, for one response metric, with
# the panels stacked one per row.

build_posterior_guild_figure <-
  function(
    .draws,
    metric,
    plot_title) {
    plot_data <-
      .draws %>%
      add_guild_label() %>%
      add_posterior_bmp_label()
    label_data <-
      plot_data %>%
      summarise_posterior_probability(
        grouping_vars =
          c("guild_label", "bmp_label")
      )
    ggplot(
      data = plot_data,
      mapping = posterior_cell_aes
    ) %>%
      add_posterior_layers() %>%
      add_probability_labels(label_data = label_data) +
      facet_wrap(
        facets = vars(guild_label),
        ncol = 1,
        scales = "free_y"
      ) +
      scale_fill_manual(
        values = guild_colours,
        guide = "none"
      ) +
      scale_colour_manual(
        values = guild_colours,
        guide = "none"
      ) +
      scale_x_continuous(
        expand =
          expansion(
            mult = c(0.08, 0.3)
          )
      ) +
      labs(
        title = plot_title,
        subtitle = wrap_label(posterior_subtitle_note),
        x = effect_axis_label_for(metric),
        y = NULL
      ) +
      theme_bmp(base_size = 12)
  }
