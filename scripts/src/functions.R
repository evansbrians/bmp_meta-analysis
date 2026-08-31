# Every named function the project uses, cleaning and analysis alike.

# utility functions -------------------------------------------------------

# Standard error:

se <-
  function(.x) {
    sd(.x) /
      sqrt(
        length(.x)
      )
  }

# clean common names ------------------------------------------------------

# Function to clean common names:

fix_common_names <-
  function(.common_name) {
    .common_name %>%
      str_replace_all("-", " ") %>%
      str_remove_all("'") %>%
      str_to_snake()
  }

# tables handed between scripts ---------------------------------------------

# Path of one table in the plain-text store handed between scripts.

bmp_table_file <-
  function(table_name) {
    fs::path(
      "data/db_mirror",
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
# them was written by an earlier version of 2_screen_effects.R.

effect_size_columns <-
  c(
    "es_id",
    "effect_id",
    "key",
    "bmp",
    "response_metric",
    "response_scale",
    "effect_metric",
    "guild",
    "label_type",
    "species_key",
    "yi",
    "sei",
    "in_primary_pool",
    "pooled_only"
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
         Re-run {.file 2_screen_effects.R}."
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

# nest survival on the log hazard scale -------------------------------------

# With cll(x) = log(-log(x)), the exposure term in cll(S) = log(d) + cll(DSR)
# cancels across a contrast, so the difference below is exposure-free:

#   log hazard ratio = cll(survival_treatment) - cll(survival_control)

# Validated in claude/bmp_nest_survival_scale.md.

cloglog <-
  function(.survival) {
    log(
      -log(.survival)
    )
  }

# One arm's log cumulative hazard and its delta-method standard error.

# Percentages fold to proportions, a failure-direction row enters as its
# complement, and an unusable value returns missing.

arm_log_hazard <-
  function(
    xbar,
    group_sd,
    n,
    sign) {
    standard_error <- group_sd / sqrt(n)
    is_percent <-
      xbar > 1 &
      xbar <= 100
    survival <-
      if_else(
        is_percent,
        xbar / 100,
        xbar
      )
    standard_error <-
      if_else(
        is_percent,
        standard_error / 100,
        standard_error
      )
    survival <-
      if_else(
        sign == -1,
        1 - survival,
        survival
      )
    usable <-
      replace_na(
        survival > 0 &
          survival < 1 &
          standard_error > 0,
        FALSE
      )
    tibble(
      cll = value_when(usable, cloglog(survival)),
      cll_se =
        value_when(
          usable,
          standard_error / abs(survival * log(survival))
        )
    )
  }

# A coefficient reaches the hazard scale only if its scale is recorded:
# `logistic_exposure` and `logit` on the logit, `log` on the logarithm.

# Every route needs a baseline survival. Only logistic exposure defaults, to
# 0.95; the others return missing without one.

# `sign` is applied here, as on the arm route, so it is not applied again.

log_hazard_from_coefficient <-
  function(
    beta,
    se,
    sign,
    link,
    baseline_survival) {
    baseline <-
      case_when(
        !is.na(baseline_survival) ~ baseline_survival,
        link == "logistic_exposure" ~ 0.95,
        .default = NA_real_
      )
    treatment_survival <-
      case_when(
        link %in% c("logistic_exposure", "logit") ~
          plogis(
            qlogis(baseline) + beta
          ),
        link == "log" ~ baseline * exp(beta),
        link == "identity" ~ baseline + beta
      )

    # The delta-method gradient is taken with respect to whatever `beta`
    # moves: the linear predictor, or the survival itself.

    gradient <-
      case_when(
        link %in% c("logistic_exposure", "logit") ~
          (1 - treatment_survival) /
            abs(
              log(treatment_survival)
            ),
        link == "log" ~
          1 /
            abs(
              log(treatment_survival)
            ),
        link == "identity" ~
          1 /
            abs(
              treatment_survival * log(treatment_survival)
            )
      )
    usable <-
      replace_na(
        is.finite(beta) &
          is.finite(se) &
          se > 0 &
          baseline > 0 &
          baseline < 1 &
          treatment_survival > 0 &
          treatment_survival < 1,
        FALSE
      )
    tibble(
      yi =
        value_when(
          usable,
          -sign *
            (cloglog(treatment_survival) - cloglog(baseline))
        ),
      sei = value_when(usable, se * gradient)
    )
  }

# An odds ratio from a logistic-exposure model is exp(beta) on the logit, so
# its logarithm is the coefficient.

log_hazard_from_odds_ratio <-
  function(
    odds_ratio,
    lower_cl,
    upper_cl,
    sign,
    link,
    baseline_survival) {
    log_hazard_from_coefficient(
      beta = log(odds_ratio),
      se =
        confint_to_se(
          log(lower_cl),
          log(upper_cl)
        ),
      sign = sign,
      link = link,
      baseline_survival = baseline_survival
    )
  }

# The difference of log cumulative hazards, negated so positive means benefit.

# `yi` is therefore the NEGATIVE of a conventional log hazard ratio, and
# direction is already resolved: do NOT apply `sign` to these rows again.

log_hazard_contrast <-
  function(
    treatment,
    control) {
    tibble(
      yi = -(treatment$cll - control$cll),
      sei =
        sqrt(
          treatment$cll_se^2 + control$cll_se^2
        )
    )
  }

# geography ----------------------------------------------------------------

# The regions the geography sensitivity contrasts. Everywhere else is `other`.

# The region is ecological, not political, so the prairie provinces count.
# A record given only "canada" cannot be placed and stays `other`.

great_plains_places <-
  c(
    "alberta",
    "colorado",
    "kansas",
    "manitoba",
    "montana",
    "nebraska",
    "new_mexico",
    "north_dakota",
    "oklahoma",
    "saskatchewan",
    "south_dakota",
    "texas",
    "wyoming"
  )

southeast_states <-
  c(
    "alabama",
    "arkansas",
    "florida",
    "georgia",
    "kentucky",
    "louisiana",
    "mississippi",
    "north_carolina",
    "south_carolina",
    "tennessee",
    "virginia",
    "west_virginia"
  )

south_american_countries <-
  c(
    "argentina",
    "bolivia",
    "brazil",
    "chile",
    "colombia",
    "paraguay",
    "peru",
    "uruguay"
  )

# The region one recorded place belongs to. Places are named, so one outside
# the named regions is `other` rather than falling through to a continent.

classify_region <-
  function(
    .geography,
    .continent) {
    case_when(
      .geography %in% great_plains_places ~ "great_plains",
      .geography %in% southeast_states ~ "southeast_us",
      .geography %in% south_american_countries ~ "south_america",
      .continent == "europe" ~ "europe",
      .default = "other"
    )
  }

# One region per study, from the study_place table. A study spanning two
# regions is `multiple`, so a region filter never half-includes it.

# A study's places are recorded at more than one grain, so only the finest
# grain it carries is classified.

study_region_lookup <-
  function(.places) {
    .places %>%
      mutate(
        grain =
          geography_type %>%
          case_match(
            c("state", "province") ~ 1L,
            "country" ~ 2L,
            "continent" ~ 3L,
            .default = 4L
          )
      ) %>%
      filter(
        grain == min(grain),
        .by = study_key
      ) %>%
      mutate(
        region = classify_region(geography, continent)
      ) %>%
      summarise(
        region =
          if_else(
            n_distinct(region) == 1,
            first(region),
            "multiple"
          ),
        .by = study_key
      ) %>%
      select(
        key = study_key,
        region
      )
  }

# modelling ----------------------------------------------------------------

# Drops cells below the per-metric effect-size and study minima.

# A cell is modelled only above both floors. The paper floor is three for
# every metric, nest success included, as 2_screen_effects.R applies it.

inclusion_thresholds <-
  tribble(
    ~ response_metric, ~ metric_min_effect_sizes, ~ metric_min_studies,
    "abundance", 3L, 3L,
    "species_richness", 3L, 3L,
    "nest_success", 3L, 3L
  )

# Sampler settings. The seed fixes every fit in the analysis, primary and
# sensitivity alike. Four chains, as the convergence diagnostics assume.

sampler_settings <-
  list(
    chains = 4,
    iter = 8000,
    warmup = 2000,
    cores = 4,
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

# The responses whose pooled model can carry a guild cell below the paper
# floor, since the pooled cell stands on its own papers.

pooled_floor_exception_metrics <- "abundance"

# The bmp x response cells that can carry a pooled estimate: three papers among
# the records reaching the pooled pool, drawn from both guilds.

pooled_cell_support <-
  function(
    .data,
    min_papers = 3,
    metrics = pooled_floor_exception_metrics) {
    .data %>%
      filter(response_metric %in% metrics) %>%
      keep_pooled_rows() %>%
      summarise(
        pooled_papers = n_distinct(key),
        pooled_guilds = n_distinct(guild[!is.na(guild)]),
        .by = c(bmp, response_metric)
      ) %>%
      filter(
        pooled_papers >= min_papers,
        pooled_guilds > 1
      ) %>%
      select(
        bmp,
        response_metric
      ) %>%
      mutate(pooled_supported = TRUE)
  }

# The paper floor as a status per record: retained, held for the pooled model
# alone, or out of scope.

# A guild cell needs papers of its own, and its practice x response needs them
# across the guilds.

paper_floor_status <-
  function(
    .data,
    min_papers = 3) {
    .data %>%
      mutate(
        cell_papers = n_distinct(key),
        .by = c(bmp, response_metric, guild)
      ) %>%
      mutate(
        group_papers = n_distinct(key),
        .by = c(bmp, response_metric)
      ) %>%
      left_join(
        pooled_cell_support(
          .data,
          min_papers = min_papers
        ),
        by = c("bmp", "response_metric")
      ) %>%
      mutate(
        floor_status =
          case_when(
            group_papers < min_papers ~ "excluded",
            is.na(guild) |
              cell_papers >= min_papers ~ "retained",
            replace_na(pooled_supported, FALSE) ~ "pooled_only",
            .default = "excluded"
          )
      ) %>%
      select(
        !c(
          cell_papers,
          group_papers,
          pooled_supported
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

      # A prior supplied alongside refit_from is the prior-sensitivity case
      # and triggers a recompile; passing none reuses the compiled program.

      update_arguments <-
        list(
          object = refit_from,
          newdata = .data,
          chains = settings$chains,
          iter = settings$iter,
          warmup = settings$warmup,
          cores = settings$cores,
          seed = settings$seed,
          refresh = 0,
          silent = 2
        )
      if (!missing(priors) && !is.null(priors)) {
        update_arguments$prior <- priors
      }
      return(
        exec(update, !!!update_arguments)
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

# The practice vocabulary: the code the data carry against the formal name the
# manuscript prints. 3_build_database.R loads it as the database's bmp table.

# The extraction says `grazing_intensity` where the metadata says
# `reduce_grazing_intensity`; both name the one practice.

bmp_vocabulary <-
  tribble(
    ~ bmp, ~ bmp_name,
    "add_flushing_bar", "Add a Flushing Bar",
    "delay_hay", "Avoid Haying During Nesting Season",
    "dont_mow_at_night", "Do not Mow at Night",
    "eliminate_pesticides", "Eliminate Pesticides",
    "stream_exclusion_and_buffers",
    "Exclude Livestock From Streams and Plant Vegetative Buffers",
    "prescribed_fire", "Implement Prescribed Fire",
    "install_nest_boxes", "Install Nest-Boxes",
    "manage_in_patches", "Manage Fields in Patches",
    "mow_towards_refugia", "Mow Towards Refugia",
    "plant_nwsg", "Plant Native Grasses and Forbs",
    "edge_and_shrub_habitat", "Promote Edge and Shrub Habitat",
    "provide_overwintering_habitat",
    "Provide Overwintering Structure and Resources",
    "raise_blades", "Raise Cutting Blades",
    "grazing_intensity", "Reduce Grazing Intensity",
    "reduce_grazing_intensity", "Reduce Grazing Intensity",
    "remove_non_native_shrubs", "Remove Non-native Shrubs",
    "rotational_grazing", "Rotate Livestock Between Pastures"
  )

# Formal practice name for a code. A code the vocabulary does not hold is an
# error, not a blank label.

format_bmp <-
  function(
    .bmp,
    vocabulary = bmp_vocabulary) {
    unmapped <-
      .bmp %>%
      as.character() %>%
      setdiff(vocabulary$bmp) %>%
      discard(is.na)
    if (length(unmapped) > 0) {
      cli::cli_abort(
        "{.var bmp_vocabulary} has no formal name for {.val {unmapped}}."
      )
    }
    vocabulary$bmp_name[match(.bmp, vocabulary$bmp)]
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
            color = "grey88",
            linewidth = 0.25
          ),
        strip.background =
          element_rect(
            fill = "grey95",
            color = "black",
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

# Guild and label type, read off the record's own analysis class. A species
# with no class is marked, not dropped in silence.

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

# Every practice the cleaned sheets may carry. A value outside this list is an
# uncanonicalised string 2_clean_extraction_gsheet.R should have folded.

canonical_practices <-
  c(
    "delay_hay",
    "edge_and_shrub_habitat",
    "eliminate_pesticides",
    "install_nest_boxes",
    "keep_cats_indoors",
    "manage_in_patches",
    "mow_towards_refugia",
    "no_bmp",
    "plant_nwsg",
    "prescribed_fire",
    "provide_overwintering_habitat",
    "reduce_grazing_intensity",
    "remove_non_native_shrubs",
    "rotational_grazing",
    "stream_exclusion_and_buffers",
    "upgrade_to_darksky"
  )

# No practice is pooled for the models at present. Add a row to pool one
# practice into another; the extraction stays canonical either way.

practice_analysis_groups <-
  tibble(
    bmp = character(),
    analysis_bmp = character()
  )

# The coarser practice the models group on. Practice strings arrive canonical
# and one per row, so only the grouping is left.

add_analysis_bmp <-
  function(.data) {
    unknown <-
      .data %>%
      pull(bmp) %>%
      unique() %>%
      setdiff(canonical_practices)

    if (length(unknown) > 0) {
      cli::cli_abort(
        "Practices missing from {.var canonical_practices}: {unknown}."
      )
    }

    .data %>%
      left_join(
        practice_analysis_groups,
        by = join_by(bmp)
      ) %>%
      mutate(
        bmp = coalesce(analysis_bmp, bmp)
      ) %>%
      select(!analysis_bmp)
  }

# The one read path for the five cleaned extraction tables. Everything is
# canonical there already, so nothing is cleaned here.

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

# Split a response variable into a qualifier token -- a year, a site, a
# nesting stage -- and the base expression that token qualifies.

# A stage is a token because two stages of one nest are two results, not two
# expressions of one, so they must not compete as duplicates.

split_response_var <-
  function(.response_var) {
    qualifier <-
      regex(
        str_c(
          "\\(([^)]*)\\)",
          "[0-9]{2,4}(-[0-9]{2,4})?",
          "\\b(incubation|nestling|laying|brood[- ]rearing)\\b",
          sep = "|"
        ),
        ignore_case = TRUE
      )
    tokens <-
      .response_var %>%
      str_extract_all(qualifier) %>%
      map_chr(
        \(.token) {
          .token %>%
            str_remove_all("[()]") %>%
            str_to_lower() %>%
            sort() %>%
            str_c(collapse = "+")
        }
      )
    base <-
      .response_var %>%
      str_remove_all(qualifier) %>%
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
    .response_var,
    .preferences) {
    matches <-
      .preferences %>%
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

      # A record its guild cell cannot carry is not a guild record:

      filter(
        response_metric == {{ metric }},
        !is.na(guild),
        !pooled_only
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

# An assemblage total is its study's species-level records added up, so it
# enters only where that study reports no species for the practice.

keep_pooled_rows <-
  function(.data) {
    .data %>%
      filter(label_type == "species") %>%
      mutate(
        names_one_species =
          !is.na(guild) |
            species_group == "species"
      ) %>%
      filter(
        names_one_species |
          !any(names_one_species),
        .by = c(key, bmp, response_metric)
      )
  }

# The practices the pooled pool reads from BOTH guilds, so a pooled estimate
# never rests on one guild while reading as an assemblage result.

# The cell is judged on the records it holds rather than on two guild cells
# clearing the paper floor separately, since it is its own estimate.

keep_practices_covering_both_guilds <-
  function(.data) {
    .data %>%
      filter(
        n_distinct(guild[!is.na(guild)]) == 2,
        .by = bmp
      )
  }

# Every grassland class in one stratum, whether the row names a guild, one
# species, or an assemblage no species-level record covers.

build_pooled_pool <-
  function(
    .data,
    metric,
    pooled_label = "all_grassland") {
    .data %>%
      filter(response_metric == {{ metric }}) %>%
      keep_pooled_rows() %>%
      keep_practices_covering_both_guilds() %>%
      mutate(
        source_guild = analysis_class,
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
    metric,
    models) {
    fit <-
      models %>%
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
    metric,
    models) {
    fit <-
      models %>%
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

# The palette, axis labels and mappings every figure chain reads:

guild_display_levels <-
  c(
    "obligate_grassland",
    "facultative_grassland",
    "all_grassland"
  ) %>%
  format_guild()

guild_colors <-
  c(
    "#1B5E3C",
    "#B07A2A",
    "#3B4A6B"
  ) %>%
  set_names(guild_display_levels)

# One axis label per effect scale. An inlined figure knows which scale it is
# on, so it names the label it needs rather than choosing at run time.

effect_axis_label <- "Pooled effect size (Hedges' g, 95% credible interval)"

hazard_axis_label <-
  "Pooled effect size (log hazard ratio, 95% credible interval)"

filled_point_note <- "Filled points mark intervals excluding zero."

# Practice names run long, so every practice axis wraps at this width. Only
# the display label wraps, never a join key.

practice_label_width <- 40

# A star marks a cell that meets only the reduced threshold.

sample_size_label <-
  aes(
    x = ucl,
    label =
      str_c(
        "k=",
        k,
        "; n=",
        n_studies,
        if_else(
          meets_primary_threshold,
          "",
          "*",
          missing = ""
        )
      )
  )

read_table_output <-
  function(file_name) {
    file.path(
      "output/tables",
      str_c(file_name, ".csv")
    ) %>%
      read_csv(show_col_types = FALSE)
  }

add_bmp_label <-
  function(
    .data,
    label_width = practice_label_width) {
    .data %>%
      mutate(
        bmp_label =
          bmp %>%
          format_bmp() %>%
          str_wrap(width = label_width) %>%
          fct_reorder(estimate)
      )
  }

add_guild_label <-
  function(
    .data,
    guild_levels = guild_display_levels) {
    .data %>%
      mutate(
        guild_label =
          guild %>%
          format_guild() %>%
          factor(levels = guild_levels)
      )
  }

# sensitivity --------------------------------------------------------------

# Columns a refit treats as grouping factors rather than as values.

model_factor_columns <-
  c(
    "key",
    "effect_id",
    "species_key",
    "bmp",
    "guild",
    "guild_bmp"
  )

# Refit one prepared pool; NULL when no cell clears the thresholds.

refit_cells <-
  function(
    .pool,
    template_model,
    template_name,
    cell_variable,
    grouping_vars,
    specification,
    response_metric,
    priors = NULL,
    thresholds = NULL,
    factor_columns = model_factor_columns) {
    prepared <-
      .pool %>%
      apply_inclusion_thresholds(
        grouping_vars = grouping_vars,
        thresholds = thresholds %||% inclusion_thresholds
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
    # A one-level cell factor has no design matrix to build, and a pooled
    # estimate over a single cell is not what the tables report.

    if (n_distinct(prepared[[cell_variable]]) < 2) {
      cli::cli_warn(
        "{specification} leaves {template_name} with fewer than two cells."
      )
      return(NULL)
    }
    fit <-
      prepared %>%
      fit_meta_model(
        model_formula = NULL,
        priors = priors,
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
    specification,
    models,
    priors = NULL,
    thresholds = NULL) {
    refit_cells(
      .pool = .pool,
      template_model =
        models %>%
        pluck(.family$template),
      template_name = .family$template,
      cell_variable = .family$cell_variable,
      grouping_vars = .family$grouping_vars,
      specification = specification,
      response_metric = .family$response_metric,
      priors = priors,
      thresholds = thresholds
    )
  }

# Every categorical record 1_effect_sizes.R converted, before the screen.
# `effect_metric` names the scale, and the two scales are never pooled.

read_converted_effects <-
  function() {
    bmp_read_table("converted_effects")
  }

# The analysis pool: what 2_screen_effects.R kept.

read_effect_size_pool <-
  function() {
    bmp_read_table(
      "effect_sizes",
      required_columns = effect_size_columns
    )
  }

# The effects held out by the flagged-records specification.

# Keyed on the extraction content, not a row number, so it survives a
# re-export. A row marked resolved is no longer held out.

# Named for the data-quality flag it carries. The screen's own hold-outs are
# output/audits/excluded_effects.csv, travelling the other way.

flagged_effect_columns <-
  c(
    "key",
    "response_var",
    "species",
    "treatment",
    "control"
  )

read_flagged_effects <-
  function(
    file_path = "data/flagged_effects.csv") {
    empty <-
      flagged_effect_columns %>%
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
        c(flagged_effect_columns, "status"),
        names(listed)
      )
    if (length(missing_columns) > 0) {
      cli::cli_abort("flagged_effects.csv is missing {missing_columns}.")
    }
    listed %>%
      filter(status == "open") %>%
      distinct(
        across(
          all_of(flagged_effect_columns)
        )
      )
  }

# Records one screen reason held out, bound back onto a pool.

# A specification looser than the screen reads the records it wants off the
# audit 2_screen_effects.R leaves.

readmit_screened <-
  function(
    .pool,
    reason,
    metric,
    file_path = "output/audits/excluded_effects.csv") {
    held_out <-
      read_csv(
        file_path,
        show_col_types = FALSE
      ) %>%
      filter(
        excluded_by == reason,
        response_metric == metric
      ) %>%
      select(
        any_of(
          names(.pool)
        )
      )
    .pool %>%
      bind_rows(held_out)
  }

# Build the pool for one metric; every design choice is an argument.

build_pool <-
  function(
    .effect_sizes,
    response_metric,
    retain_fire = TRUE,
    retain_non_grassland = FALSE,
    drop_flagged = FALSE,
    min_papers = 3,
    guild_assigned_only = FALSE,
    by_guild = TRUE,
    pooled = FALSE,
    group_means_only = FALSE,
    one_per_study_cell = FALSE,
    only_region = NULL,
    drop_region = NULL) {
    pool <-
      .effect_sizes %>%
      filter(
        response_metric == {{ response_metric }},
        is.finite(yi),
        is.finite(sei),
        sei > 0
      )

    # A floor looser than the screen's own reads back the cells the screen
    # held out, before anything else runs.

    if (min_papers < 3) {
      pool <-
        pool %>%
        readmit_screened(
          reason = "paper_count",
          metric = response_metric
        )
    }
    if (!retain_fire) {
      pool <-
        pool %>%
        filter(!fire_excluded_original)
    }
    if (!retain_non_grassland) {
      pool <-
        pool %>%
        filter(!non_grassland_class)
    }
    if (!is.null(only_region)) {
      pool <-
        pool %>%
        filter(region %in% only_region)
    }
    if (!is.null(drop_region)) {
      pool <-
        pool %>%
        filter(!region %in% drop_region)
    }
    if (drop_flagged) {
      pool <-
        pool %>%
        anti_join(
          read_flagged_effects(),
          by = flagged_effect_columns
        )
    }

    # The conversion-route specification: only effects computed from reported
    # arm summaries, dropping the coefficient and test-statistic routes.

    if (group_means_only) {
      pool <-
        pool %>%
        filter(
          conversion %in%
            c(
              "two group means",
              "log hazard ratio from arm survival"
            )
        )
    }

    # The floor the screen applied is three papers, counted over the primary
    # pool. Another value is counted over the pool the specification builds.

    if (min_papers != 3) {
      pool <-
        pool %>%
        paper_floor_status(min_papers = min_papers) %>%
        filter(floor_status != "excluded") %>%
        mutate(
          pooled_only = floor_status == "pooled_only",
          .keep = "unused"
        )
    }

    # A species-level record without a guild still belongs to the pooled
    # stratum, so the guild requirement is the guild families' alone.

    if (by_guild && !pooled) {
      pool <-
        pool %>%
        filter(
          !is.na(guild),
          !pooled_only
        )
    }
    if (pooled) {
      if (guild_assigned_only) {
        pool <-
          pool %>%
          filter(
            !is.na(guild)
          )
      }

      # The pooled model covers only the practices read from both guilds, so
      # every refit applies that rule before relabelling.

      pool <-
        pool %>%
        keep_pooled_rows() %>%
        keep_practices_covering_both_guilds() %>%
        mutate(
          source_guild = analysis_class,
          guild = "all_grassland"
        )
    }
    if (one_per_study_cell) {
      pool <-
        pool %>%
        aggregate_one_per_study_cell()
    }
    pool
  }

# One inverse-variance weighted effect size per study and cell, bounding what
# treating a study's sampling errors as independent could cost.

aggregate_one_per_study_cell <-
  function(.pool) {
    .pool %>%
      mutate(
        weight = 1 / sei^2
      ) %>%
      summarise(
        yi =
          sum(yi * weight) /
          sum(weight),
        sei =
          sqrt(
            1 / sum(weight)
          ),
        species_key = first(species_key),
        .by =
          any_of(
            c(
              "key",
              "response_metric",
              "guild",
              "bmp"
            )
          )
      ) %>%
      mutate(
        effect_id =
          row_number() %>%
          str_c("agg_", .)
      )
  }

write_partial_estimates <-
  function(
    .estimates,
    file_path) {
    append_mode <- fs::file_exists(file_path)
    write_csv(
      .estimates,
      file_path,
      na = "",
      append = append_mode
    )
    invisible(.estimates)
  }

# Studentized deleted residual, Viechtbauer and Cheung (2010): the effect
# against a DerSimonian-Laird mean fitted without it.

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

# Residuals for one cell. A cell too small to leave one out returns missing
# residuals rather than unstable ones.

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

# An outlier clears the critical value. Flagged for the sensitivity refit,
# never dropped -- about 5% clear 1.96 by chance.

flag_outliers <-
  function(
    .pool,
    join_vars,
    cell_columns,
    critical_value = 1.96) {
    .pool %>%
      mutate(
        across(
          any_of(cell_columns),
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

# Egger regression plus the PET and PEESE adjusted estimates, all on the
# study-aggregated data (the standard tests assume independent effect sizes).

# PET is the Egger intercept; PEESE regresses on the sampling variance. Both
# are diagnostics, not replacement estimates.

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
    pet <-
      lm(
        yi ~ sei,
        data = aggregated,
        weights = 1 / sei^2
      )
    peese <-
      lm(
        yi ~ I(sei^2),
        data = aggregated,
        weights = 1 / sei^2
      )
    tibble(
      analysis = {{ label }},
      n_studies = nrow(aggregated),
      pooled_estimate = as.numeric(model$beta),
      egger_statistic = egger$zval,
      egger_p = egger$pval,
      asymmetry_detected = egger$pval < 0.05,
      pet_estimate = coef(pet)[[1]],
      pet_se =
        summary(pet) %>%
        coef() %>%
        magrittr::extract(1, 2),
      peese_estimate = coef(peese)[[1]],
      peese_se =
        summary(peese) %>%
        coef() %>%
        magrittr::extract(1, 2)
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

# REML refit of one pool's cell means. The random terms mirror the Bayesian
# formula for that pool.

fit_reml_cell_means <-
  function(
    .pool,
    cell_variable,
    random_terms =
      list(
        ~ 1 | key,
        ~ 1 | effect_id,
        ~ 1 | species_key
      )) {
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
        random = random_terms,
        data = model_data,
        method = "REML",
        sparse = TRUE
      )
    tibble(
      cell =
        reml_fit$b %>%
        rownames(),
      reml_estimate = as.numeric(reml_fit$b),
      reml_lcl = as.numeric(reml_fit$ci.lb),
      reml_ucl = as.numeric(reml_fit$ci.ub)
    ) %>%
      filter(
        str_starts(cell, "cell")
      ) %>%
      mutate(
        cell = str_remove(cell, "^cell")
      )
  }

# posterior figures --------------------------------------------------------

# The notes and mappings the posterior builders below take as argument
# defaults, beside the functions that read them.

posterior_slab_note <-
  str_c(
    "The slab is the posterior density of the cell mean, scaled to a common ",
    "height; the point and bar are the median and 95% credible interval the ",
    "forest plots show. P>0 is the posterior mass above zero."
  )

posterior_subtitle_note <-
  str_c(
    "Each guild estimated separately, and the two guilds pooled in their ",
    "own panel. ",
    posterior_slab_note
  )

# Pinned to the panel edge rather than to the interval, because a slab is
# wider than the interval it carries.

probability_label_mapping <-
  aes(
    x = Inf,
    label = edge_label
  )

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
  function(
    .data,
    label_width = practice_label_width) {
    .data %>%
      mutate(
        bmp_label =
          bmp %>%
          format_bmp() %>%
          str_wrap(width = label_width) %>%
          fct_reorder(.value)
      )
  }

# Sample size and posterior probability as one right-aligned label, so they
# cannot collide. Figure spaces are a digit wide, so the columns line up.

posterior_edge_labels <-
  function(
    .draws,
    .cells,
    grouping_vars,
    join_vars) {
    .draws %>%
      summarise_posterior_probability(grouping_vars = grouping_vars) %>%
      left_join(
        .cells %>%
          select(
            all_of(join_vars),
            k,
            n_studies
          ),
        by = join_vars
      ) %>%
      mutate(
        sample_note =
          glue::glue("k = {k}, {n_studies} studies") %>%
          as.character(),
        edge_label =
          str_c(
            str_pad(
              sample_note,
              max(str_width(sample_note)),
              side = "right",
              pad = "\u2007"
            ),
            "     ",
            probability_label
          )
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
